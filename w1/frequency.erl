-module(frequency).
-export([init/0]).

% Server initialisation with hard coded frequencies
init() ->
    Frequencies = {get_frequencies(),[]}, 
    loop(Frequencies).

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
	    NewFrequencies = deallocate(Frequencies, Freq),
	    Pid ! {reply, ok},
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
    {{Free, [{Freq, Pid}| Allocated]},
     {ok, Freq}}.

% deallocate a frequency
% returns the new {free,allocated} lists
% TODO : return a ok/error message
% TODO : refuse to deallocate if Freq was not allocated
%        to the client who request deallocation
deallocate({Free, Allocated}, Freq) ->
    NewAllocated = lists:keydelete(Freq, 1, Allocated),
    {[Freq|Free], NewAllocated}.
    
