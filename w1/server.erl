-module(server).
-export([server/1,tester/0]).

server(Pid) ->
    receive 
	{check, String} ->
	    case palin:palindrome_check(String) of
		true ->
		    Pid ! {result,String ++ " is a palindrome"},
		    server(Pid);	
		_ ->
		    Pid ! {result,String ++ " is not a palindrome"},
		    server(Pid)	
	    end;
	_Msg -> 
	    ok
    end.


tester() ->
    Self = self(),
    Server = spawn(server,server,[Self]),
    Server ! {check, "Abba!"},
    receive
	Msg ->
	    io:format("~w~n",[Msg])
    end,
    Server ! {check, "Abbot!"},
    receive
	Msg2 ->
	    io:format("~w~n",[Msg2])
    end,
    Server ! stop.
