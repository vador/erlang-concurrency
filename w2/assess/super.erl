-module(super).
-export([super/0]).

%%%% Experiment with "exit kill" :
% when killing the "worker" thread from talk module
% it just prints "true" :
% {<0.61.0>,67} sent.
% 67 echoed.         
% {<0.61.0>,68} sent.
% 68 echoed.   
% 3> exit(Pid, kill).
% true
% Restarting the working restarts the counter to 0
% but echo replies ok.
%
% when killing the echo process, 
% the worker fails with an error, as there is
% no longer a process registered with "echo"
%
% {<0.64.0>,35} sent.          
% 35 echoed.                   
% 5> exit(whereis(echo), kill).
% true
% 6> 
%  =ERROR REPORT==== 19-Aug-2017::18:59:10 ===
% Error in process <0.64.0> with exit value:
%  {badarg,[{talk,work,1,[{file,"talk.erl"},{line,9}]}]}
%
% we can recover with registering anew a spawned echo :
% 7> register(echo, spawn(echo, listener, [])).
% true
% 8> Pid = spawn(talk,worker, []).             
% {<0.73.0>,0} sent.
% 0 echoed.


%%% Session explanation for supervisor is at the end of file
super() ->
    process_flag(trap_exit, true),
    E = spawn_link(echo,listener,[]),
    register(echo,E),
    io:format("echo spawned.~n"),
    T = spawn_link(talk,worker,[]),
    register(talk,T),
    io:format("worked spawned as Pid ~w.~n",[whereis(talk)]),
    loop(E,T).

loop(E,T) ->
     receive
	 {'EXIT', T, _} ->
	     io:format("worker exited, restarting"),
	     Talker = spawn_link(talk, worker, []),
	     register(talk, Talker),
	     io:format("worker restarted as Pid ~w.~n", [whereis(talk)]),
	     loop(E,Talker);
	 {'EXIT', E, _} ->
	     io:format("echo exited, restarting"),
	     timer:sleep(1000),
	     Echo = spawn_link(echo, listener, []),
	     register(echo, Echo),
	     io:format("listened restarted as Pid ~w.~n", [whereis(echo)]),
	     loop(Echo, T)
     end.


%%% playing with super, without time:sleep/1 :
%% 1> c(super).
%% {ok,super}
%% 2> Super = spawn(super, super, []).
%% echo spawned.
%% <0.64.0>
%% worked spawned as Pid <0.66.0>.
%% {<0.66.0>,0} sent.
%% 0 echoed.
%% ...
%% {<0.66.0>,59} sent.          
%% 59 echoed.                   
%% 3> exit(whereis(talk), kill).
%% worker exited, restartingtrue
%% worker restarted as Pid <0.68.0>.
%% {<0.68.0>,0} sent.
%% 0 echoed.
%% ...
%% <0.68.0>,34} sent.          
%% 34 echoed.                   
%% 4> exit(whereis(echo), kill).
%% echo exited, restartingtrue
%% listened restarted as Pid <0.70.0>.
%% {<0.68.0>,35} sent.
%% 35 echoed.


%%%% Now with timer:sleep(1000) 
%% when killing echo, worker sends a message
%% during echo downtime, so it is in turn killed.
%% when echo is restarted, the supervisor loop resume
%% and processes the exit from worker, so count is reset
%% to 0
%% 1> c(super).
%% {ok,super}
%% 2> Super = spawn(super, super, []).
%% echo spawned.
%% <0.64.0>
%% worked spawned as Pid <0.66.0>.
%% {<0.66.0>,0} sent.
%% 0 echoed.
%% ...
%% 18 echoed.                  
%% 3> exit(whereis(echo), kill).
%% echo exited, restartingtrue
%% 4> 
%% =ERROR REPORT==== 19-Aug-2017::19:56:41 ===
%% Error in process <0.66.0> with exit value:
%% {badarg,[{talk,work,1,[{file,"talk.erl"},{line,9}]}]}
%% listened restarted as Pid <0.69.0>.
%% worker exited, restartingworker restarted as Pid <0.70.0>.
%% {<0.70.0>,0} sent.
%% 0 echoed.



