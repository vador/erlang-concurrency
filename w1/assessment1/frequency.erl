%% Based on code from 
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(frequency).
-export([start/0,allocate/0,deallocate/1,stop/0,status/0,clear/0]).
-export([init/0]).

%% These are the start functions used to create and
%% initialize the server.

start() ->
    register(frequency,
	     spawn(frequency, init, [])).

init() ->
  Frequencies = {get_frequencies(), []},
  loop(Frequencies).

% Hard Coded
get_frequencies() -> [10,11,12,13,14,15].


%% if server latency is greater than timeout, the
%%   client will receive a "timout" atom 
%%   the mailbox will be cleared before following request

get_client_timeout() -> 100. % Client timeout for message
get_server_latency() -> 150. % simulate server heavy workload

%% The Main Loop

loop(Frequencies) ->
    Latency = get_server_latency(),
    receive
	{request, Pid, allocate} ->
	    timer:sleep(Latency),
	    {NewFrequencies, Reply} = allocate(Frequencies, Pid),
	    Pid ! {reply, Reply},
	    loop(NewFrequencies);
	{request, Pid , {deallocate, Freq}} ->
	    timer:sleep(Latency),
	    NewFrequencies = deallocate(Frequencies, Freq),
	    Pid ! {reply, ok},
	    loop(NewFrequencies);
	{request, Pid, status} ->
	    Pid ! {reply, Frequencies},
	    loop(Frequencies);
	{request, Pid, stop} ->
	    Pid ! {reply, stopped}
    end.

%% Functional interface
%% Adding Timeout for request
%% Mailbox is cleared at the latest possible time
%%   before sending request
%%   there still may be a race condition if response from
%%   previous request arrive between send and response from
%%   current request !

allocate() -> 
    Timeout = get_client_timeout(),
    clear(),
    frequency ! {request, self(), allocate},
    receive 
	{reply, Reply} -> Reply
    after Timeout ->
	    {timeout}
    end.

deallocate(Freq) -> 
    Timeout = get_client_timeout(),
    clear(),
    frequency ! {request, self(), {deallocate, Freq}},
    receive 
	{reply, Reply} -> Reply
    after Timeout ->
	    {timeout}
    end.

%% This function was created to check internal status
%%   of frequency (allocated or not)
status() ->
    clear(),
    frequency ! {request, self(), status},
    receive 
	    {reply, Reply} -> Reply
    end.

stop() -> 
    Timeout = get_client_timeout(),
    clear(),
    frequency ! {request, self(), stop},
    receive 
	    {reply, Reply} -> Reply
    after Timeout ->
	    {timeout}
    end.


%% The Internal Help Functions used to allocate and
%% deallocate frequencies.

allocate({[], Allocated}, _Pid) ->
  {{[], Allocated}, {error, no_frequency}};
allocate({[Freq|Free], Allocated}, Pid) ->
  {{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}}.

deallocate({Free, Allocated}, Freq) ->
  NewAllocated=lists:keydelete(Freq, 1, Allocated),
  {[Freq|Free],  NewAllocated}.

%% Mailbox clearing function
clear() ->
    receive
	Msg ->
	    io:format("Cleaned message : ~w~n",[Msg]),
	    clear()
    after 0 ->
	    ok
    end.

