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

-define(R2B (Record), list_to_binary( tl( tuple_to_list(Record) ))).

-record(zmtp_greeting, {
	signature = <<255,0:64,127>>,
	revision = 1,
	sock_type = 0,
	identity = <<0:16>>
	}).

-record(zmtp_state, {
	socket :: inet:socket(),
	transport :: module(),
	handler :: module(),
	frames :: list()
}).
