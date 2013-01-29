-module(assoc_store).
-behaviour(gen_server).

% API exports
-export([start_link/0, stop/0, add_user/2, remove_user/1, find_user/1, show_all/0]).

% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

% API Functions
start_link() ->
	gen_server:start_link({local, nodeA}, assoc_store, [], []).

stop() ->
	gen_server:call(?MODULE, stop).

add_user(Name, Email) ->
	gen_server:call(nodeA, {add_user, Name, Email}),
	gen_event:notify(error_man, {add_user, Name, Email}).

remove_user(Name) -> 
	gen_server:call(nodeA, {remove_user, Name}),
	gen_event:notify(error_man, {remove_user, Name}).

find_user(Name) ->
	gen_server:call(nodeA, {find_user, Name}).

show_all() ->
	gen_server:cast(nodeA, show_all).

% gen_server Functions
init([]) ->
	gen_event:start({local, error_man}),
	gen_event:add_handler(error_man, as_handler, [log]),
	{ok, []}.

handle_call({add_user, Name, Email}, _From, State) ->
	NewState = [{Name, Email}|State],
	io:format("~p~n", [NewState]),
	{reply, ok, NewState};

handle_call({remove_user, Name}, _From, State) ->
	User = lists:keyfind(Name, 1, State), %% add in try clause for false if entry doesnt exist
	NewState = lists:delete(User, State),
	{reply, ok, NewState};

handle_call({find_user, Name}, _From, State) ->  %alternate
	{Nam, Email} = lists:keyfind(Name, 1, State),
	io:format("User: ~s's email is ~s.~n", [Nam, Email]),
	{reply, ok, State};	

handle_call(stop, _From, State) -> 
    {stop, normal, State}.

handle_cast(show_all, State) ->
	io:format("~p~n", [State]),
	{noreply, State}.

handle_info(_, State) ->
	{ok, State}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

terminate(_Reason, _State) ->
	ok.
