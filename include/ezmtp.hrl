-define(R2B (Record), list_to_binary( tl( tuple_to_list(Record) ))).

-record(zmtp_greeting, {
	signature = <<255,0:64,127>>,
	revision = 1,
	sock_type = 0,
	identity = <<0:16>>
	}).