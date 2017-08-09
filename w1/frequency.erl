-module(frequency).
-export([init/0,client/2]).

%%% How to use :
%% Sample session for test.
%Freq = spawn(frequency, init, []). 
%Client1 = spawn(frequency, client, [Freq, self()]).
%Client2 = spawn(frequency, client, [Freq, self()]).
%
%Client1 ! {request, self(), allocate}.
%Client1 ! {request, self(), allocate}.
%Client2 ! {request, self(), allocate}.
%Client2 ! {request, self(), {deallocate,11}}.
%Client2 ! {request, self(), allocate}.
%Client2 ! {request, self(), {deallocate,10}}.
%Client1 ! {request, self(), stop}.
%Client2 ! {request, self(), stop}.
%Freq ! {request, self(), stop}.   
%
%process_info(self(), messages).
%messages,[{reply,{ok,10}},
%           {reply,{error,client_already_allocated}},
%           {reply,{ok,11}},
%           {reply,ok},
%           {reply,{ok,11}},
%           {reply,{error,not_allocated_to_client}}]}
%           {ok,<0.119.0>,client_stopped},
%           {ok,<0.119.0>,client_stopped},
%           {reply,stopped}]}
%

% Server initialisation with hard coded frequencies
init() ->
    Frequencies = {get_frequencies(),[]}, 
    loop(Frequencies).

client(Server,Master) ->
    receive
	{request, Pid, stop} ->
	    Master ! {ok, Pid, client_stopped};
	{request, _Pid, Msg} -> Server ! {request, self(), Msg},
			       client(Server, Master);
	{reply, Msg2} -> Master ! {reply, Msg2},
			client(Server, Master)
    end.



% Hard coded 
get_frequencies() ->
     [10,11,12,13,14,15].

% Main server loop.
% Parse messages and act accordingly
loop(Frequencies) ->
    receive
	{request, Pid, allocate} ->
	    {NewFrequencies, Reply} =
		allocate(Frequencies, Pid),
	    Pid ! {reply, Reply},
	    loop(NewFrequencies);
	{request, Pid, {deallocate, Freq}} ->
	    {NewFrequencies, Reply} = deallocate(Frequencies, Freq, Pid),
	    Pid ! {reply, Reply},
	    loop(NewFrequencies);

	{request, Pid, stop} ->
	    Pid ! {reply, stopped}
    end.

% Allocate a free frequency to a client
% returns ok with allocated freq if a frequency is available, 
% error, no_frequency if not frequency available
% TODO : return error if client already has a frequency allocated
allocate({[], Allocated}, _Pid) -> 
    {{[], Allocated},
     {error, no_frequency}};
allocate({[Freq|Free], Allocated}, Pid) ->
    case lists:keymember(Pid, 2, Allocated) of
	false ->
	    {{Free, [{Freq, Pid}| Allocated]},
	     {ok, Freq}};
	true ->
	    {{[Freq|Free], Allocated},
	     {error, client_already_allocated}}
    end.

% deallocate a frequency
% returns the new {free,allocated} lists
% TODO : return a ok/error message
% TODO : refuse to deallocate if Freq was not allocated
%        to the client who request deallocation
deallocate({Free, Allocated}, Freq, Pid) ->
    case lists:member({Freq, Pid}, Allocated) of
	true ->
	    NewAllocated = lists:keydelete(Freq, 1, Allocated),
	    {{[Freq|Free], NewAllocated},
	     ok};
	false ->
	    {{Free, Allocated},
	     {error, not_allocated_to_client}}
    end.
    
