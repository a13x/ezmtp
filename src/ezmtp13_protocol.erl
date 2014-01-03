%% Copyright (c) 2013 Aleksandar Radulovic <alex@a13x.net>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

%%
%% Implementation of the 13/ZMTP Spec transport protocol
%% http://rfc.zeromq.org/spec:13
%%
-module(ezmtp13_protocol).
-export([start_link/4, init/4]).
-behaviour(ranch_protocol).

-include("ezmtp.hrl").

-opaque state() :: #zmtp_state{}.
-export_type([state/0]).


start_link(Ref, Socket, Transport, Opts) ->
	Pid = spawn_link(?MODULE, init, [Ref, Socket, Transport, Opts]),
	{ok, Pid}.

init(Ref, Socket, Transport, Opts) ->
	ok = ranch:accept_ack(Ref),
	ok = greeting(Transport, Socket),
	Handler=proplists:get_value(handler, Opts),
	loop(<<>>, #zmtp_state{socket=Socket, transport=Transport, frames=[], handler=Handler}).

greeting(Transport, Socket) ->
	case Transport:recv(Socket, 2, 5000) of
		{ok, <<1,0>>} ->
			Transport:send(Socket, <<1,0>>),
			ok;
		{error, _} ->
			ok = Transport:close(Socket),
			{error, wrong_greeting}
	end.

loop(Buffer, State=#zmtp_state{socket=Socket, transport=Transport}) ->
	case Transport:recv(Socket, 0, 5000) of
		{ok, Data} ->
			{ok, Rest} = parse_frames(<< Buffer/bytes, Data/bytes >>, State),
			loop(Rest, State);
		{error, _} ->
			ok = Transport:close(Socket)
	end.

parse_frames(<<>>, _State) ->
	{ok, <<>>};

parse_frames(Data, State=#zmtp_state{frames=Frames}) ->
	case parse_frame(Data, State) of
		{envelope, Buffer} ->
			parse_frames(Buffer, State);
		{more, Frame, Buffer} ->
			parse_frames(Buffer, State#zmtp_state{frames=[Frame|Frames]});
		{last, Frame, Buffer} ->
			execute(State#zmtp_state{frames=[Frame|Frames]}),
			{ok, Buffer}
	end.

parse_frame(<<1,1, Rest/bytes>>, _State) ->
	{envelope, Rest};

parse_frame(Data, State) ->
	case Data of
		<<255,Len:64,1,Buffer/bytes>> -> 
			extract_body(Buffer, Len - 1, more, State);
		<<255,Len:64,0,Buffer/bytes>> -> 
			extract_body(Buffer, Len - 1, last, State);
		<<Len:8,1,Buffer/bytes>> ->
			extract_body(Buffer, Len - 1, more, State);
		<<Len:8,0,Buffer/bytes>> -> 			
			extract_body(Buffer, Len - 1, last, State)
		
	end.

extract_body(Buffer, Len, Tag, _State) when Len =< byte_size(Buffer) ->
	<<Body:Len/bytes, Rest/bytes>> = Buffer,
	{Tag, Body, Rest};

extract_body(Buffer, Len, Tag, State) ->
	Size = Len - byte_size(Buffer),
	Transport = State#zmtp_state.transport,
	Socket = State#zmtp_state.socket,
	{ok, MoreData} = Transport:recv(Socket, Size, 5000),
	extract_body(<< Buffer/bytes, MoreData/bytes>>, Len, Tag, State).

execute(State=#zmtp_state{frames=Frames, handler=Handler, transport=Transport, socket=Socket}) ->
	FramesInRightOrder = lists:reverse(Frames),	
	{ok, Result} = Handler:handle(FramesInRightOrder, State),
	Transport:send(Socket, Result).