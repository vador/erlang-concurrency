-module(mailbox1).
-export([seq1/1,seq2/1,send/1]).

seq1(Pid) ->
    timer:sleep(500),
    receive
	Msg ->
	    io:format("message:~w~n",[Msg]),
	    seq1(Pid)
    end.

seq2(Pid) ->
    receive
	Msg ->
	    io:format("message:~w~n",[Msg]),
	    seq2(Pid)
    end.

send(Pid) ->
    Pid ! {first,1},
    Pid ! {second,2},
    Pid ! {third,3},
    Pid ! {fourth,4}.
    
