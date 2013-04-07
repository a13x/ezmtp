-module(ezmtp_protocol).
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
	Transport:send(Socket, greeting(2)),
	{ok, <<255,0:64,127,1,SockType,0:16>>} = Transport:recv(Socket, 0, 30000),
	Transport:send(Socket, <<0:8,1:8,1:8>>),
	io:format("Socket type is ~p~n", [SockType]), 
	loop(Socket, Transport, <<>>).

loop(Socket, Transport, Buffer) ->
	case Transport:recv(Socket, 0, 30000) of
		{ok, Data} ->
			Buffer2 = <<Buffer/binary, Data/binary>>,
			{Frames, Rest} = zmtp_frames(Buffer2),
			lists:foreach(fun(X) -> handle(X) end, Frames),
			loop(Socket, Transport, Rest);
		{error, _} ->
			io:format("Disconnected~n")
	end.

handle(Data) ->
	io:format("Data size ~p~n", [byte_size(Data)]).

zmtp_frames(Data) ->
	zmtp_frames(Data, []).

zmtp_frames(<<0:8,Len:8, Rest/binary>>, Frames) ->
	io:format("Got a short message~n"),
	zmtp_frames(Rest, Len, Frames);

zmtp_frames(<<2:8,Len:64, Rest/binary>>, Frames) ->
	%io:format("Got a long message~n"),
	zmtp_frames(Rest, Len, Frames);

zmtp_frames(Rest, Frames) ->
	%io:format("nothing left~n"),
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
		<<Frame:Len/binary, Rest/binary>> ->
			{more, Frame, Rest};
		_ ->
			{nomore, Data}
	end.

