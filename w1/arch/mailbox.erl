-module(mailbox).
-export([receiver/0,receiver_stop/0,receiver_seq/0]).

% reciever loops forever, printing each message that it receives
% with at least a second between each print.

% To use this function, spawn it in the shell like this:
%   Receiver = spawn(mailbox,receiver,[]).
% and then send messages to it from the shell like this:
%   Receiver ! foo.
% You will need to terminate this by hitting Ctrl-C in the 
% shell, and then choosing q.

receiver() ->
    timer:sleep(1000),
    receive
        X ->
            io:format("message: ~w~n",[X])
    end,
    receiver().

% reciever_stop loops forever, printing each message that it receives
% with at least a second between each print.
%
% It is run like receiver/0 above.
% 
% the only exception to this is the stop message, which halts the process.

receiver_stop() ->
    timer:sleep(1000),
    receive
        stop ->
            ok;
        X ->
            io:format("message: ~w~n",[X]),
            receiver_stop()
    end.

% reciever_seq will process two messages: both are pairs, and the
% first will begin {first,… and the second {second,… 
% it then terminates
%
% It is run like receiver/0 above.

receiver_seq() ->
    receive
        {first, _FirstString} -> io:format("received first message.~n",[])
    end,
    receive
        {second, _SecondString} -> io:format("received first message.~n",[])
    end.
   


