-module(fsm_log).
-export([open/1, start_call/2, end_call/1]).

open(Name) ->
    {ok, Fd} = file:open(Name, write),
    Fd.

start_call(Name, Other_end) ->
    {MegaSec, Sec, MicroSec} = now(),
    io:fwrite(Name, "~w: ", [Other_end]),
    io:fwrite(Name, "~w.~w.~w -", 
	      [MegaSec, Sec, MicroSec]).


end_call(Name) ->
    {MegaSec, Sec, MicroSec} = now(),
    io:format(Name, " ~w.~w.~w~n", [MegaSec, Sec, MicroSec]).
