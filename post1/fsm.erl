-module(fsm).
-export([add_user/0, dial/2, take_off_hook/1, put_on_hook/1]).
-export([init/0]).

add_user() ->
    spawn(fsm, init, []).

init() ->
    Fd = fsm_log:open(pid_to_list(self())),
    idle(Fd).

%% STATES
idle(Fd) ->
    io:format("now in idle~n"),
    receive
	{request, From, {dialing, Recipient}} -> % I call another person
	    From ! {reply, ok},
	    dialing(Recipient, Fd);
	{request, From, {receiving, Caller}} -> % calls me
	    From ! {reply, ok},
	    ringing(Caller, Fd)
    end.
    
ringing(Caller, Fd)->
    io:format("ringgg~n"),
    receive
	{request, hung_up} ->
	    idle(Fd);
	{request, From, take_off_hook} ->
	    Caller ! {request, answer},
	    From ! {reply, ok},
	    connected(Caller, Fd)
				
    after 5000 -> %after 3000, ring again
	    ringing(Caller, Fd)
    end.

dialing(Recipient, Fd) ->
    io:format("diallll~n"),
    receive
	{request, answer} ->
	    connected(Recipient, Fd);
	{request, From, put_on_hook} ->
	    Recipient ! {request, hung_up},
	    From ! {reply, ok},
	    idle(Fd)
    after 5000 ->
	    dialing(Recipient, Fd)
    end.

connected(Other_End, Fd) ->
    io:format("~w connected~n", [self()]),
    fsm_log:start_call(Fd, Other_End),
    receive
	{request, hung_up} ->
	    ok;
	{request, From, put_on_hook} ->
	    Other_End ! {request, hung_up},
	    From ! {reply, ok}    
    end,
    fsm_log:end_call(Fd),
    idle(Fd).


%% ACTIONS
dial(Caller, Recipient) ->
    Caller ! {request,self(), {dialing, Recipient}},
    Recipient ! {request, self(), {receiving, Caller}},
    receive
	{reply, ok} -> true
    end.

take_off_hook(My_Pid) ->
    My_Pid ! {request, self(), take_off_hook},
    receive
	{reply, ok} ->
	    {ok}
    end.

put_on_hook(My_Pid) ->
    My_Pid ! {request, self(), put_on_hook},
    receive
	{reply, ok} ->
	    {ok}
    end.
