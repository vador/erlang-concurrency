-module(server).
-export([server/0,tester/0]).

server() ->
    receive 
	{check, Pid, String} ->
	    case palin:palindrome_check(String) of
		true ->
		    Pid ! {result,String ++ " is a palindrome"},
		    server();	
		_ ->
		    Pid ! {result,String ++ " is not a palindrome"},
		    server()	
	    end;
	_Msg -> 
	    ok
    end.


tester() ->
    Self = self(),
    Server = spawn(server,server,[]),
    Server ! {check, Self, "Abba!"},
    receive
	Msg ->
	    io:format("~w~n",[Msg])
    end,
    Server ! {check, Self, "Abbot!"},
    receive
	Msg2 ->
	    io:format("~w~n",[Msg2])
    end,
    Server ! stop.
