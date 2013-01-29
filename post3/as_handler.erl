-module(as_handler).
-behaviour(gen_event).

-export([init/1, handle_event/2, handle_call/2, handle_info/2, code_change/3, terminate/2]).

init(Name) ->
	{ok, Fd} = file:open(Name, write),
	{ok, Fd}.

handle_event({add_user, Name, Email}, Fd) ->
	io:format(Fd, "Added user:~s email: ~s~n", [Name, Email]),
	{ok, Fd};
handle_event({remove_user, Name}, Fd) ->
	io:format(Fd, "Removed user:~s~n", [Name]),
	{ok, Fd};
handle_event(_, Fd) ->
	{ok, Fd}.

handle_call(_, Fd) ->
	{ok, ok, Fd}.

handle_info(_, Fd) ->
	{ok, Fd}.

code_change(_OldVsn, Fd, _Extra) ->
	{ok, Fd}.

terminate(_Reason, _State) ->
	ok.
