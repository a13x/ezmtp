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
%% Implementation of the 15/ZMTP Spec transport protocol
%% http://rfc.zeromq.org/spec:15
%%

-module(ezmtp_protocol).
-behaviour(ranch_protocol).
-export([greeting/1, start_link/4, init/4, handle/1, zmtp_frames/1]).

-include("ezmtp.hrl").

greeting(Type) ->
	R = #zmtp_greeting{sock_type = Type},
	?R2B(R).

start_link(ListenerPid, Socket, Transport, Opts) ->
    Pid = spawn_link(?MODULE, init, [ListenerPid, Socket, Transport, Opts]),
    {ok, Pid}.

init(ListenerPid, Socket, Transport, _Opts = []) ->
	ok = ranch:accept_ack(ListenerPid),
	Transport:send(Socket, greeting(3)),
	{ok, <<255,0:64,127,1,SockType,0:16>>} = Transport:recv(Socket, 0, 30000),
	Transport:send(Socket, <<0:8,1:8,1:8>>),
	io:format("Socket type is ~p~n", [SockType]), 
	loop(Socket, Transport, <<>>).

loop(Socket, Transport, Buffer) ->
	case Transport:recv(Socket, 0, 30000) of
		{ok, Data} ->
			Buffer2 = <<Buffer/bytes, Data/bytes>>,
			{Frames, Rest} = zmtp_frames(Buffer2),
			%lists:foreach(fun(X) -> handle(X) end, Frames),
			[spawn(?MODULE, handle, [X]) || X <- Frames],
			loop(Socket, Transport, Rest);
		{error, _} ->
			io:format("Disconnected~n")
	end.

handle(Data) ->
	io:format("Data size ~p~n", [byte_size(Data)]).

zmtp_frames(Data) ->
	zmtp_frames(Data, []).

zmtp_frames(<<0:8,Len:8, Rest/bytes>>, Frames) ->
	zmtp_frames(Rest, Len, Frames);

zmtp_frames(<<2:8,Len:64, Rest/bytes>>, Frames) ->
	zmtp_frames(Rest, Len, Frames);

zmtp_frames(Rest, Frames) ->
	{Frames, Rest}.

zmtp_frames(Data, Len, Frames) ->
	case zmtp_frame(Data, Len) of
		{more, Frame, Rest} ->
			zmtp_frames(Rest, [Frame|Frames]);
		{nomore, Rest} ->
			{Frames, Rest}
	end.

zmtp_frame(Data, Len) ->
	case Data of
		<<Frame:Len/bytes, Rest/bytes>> -> {more, Frame, Rest};
		_ -> {nomore, Data}
	end.

