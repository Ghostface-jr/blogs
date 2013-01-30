-module(my_db_gen).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, write/2, delete/1, read/1, match/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, match/2]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

write(Key, Element) ->
	gen_server:call(?MODULE, {write, Key, Element}).

delete(Key) ->
	gen_server:call(?MODULE, {delete, Key}).

read(Key) ->
	gen_server:cast(?MODULE, {read, Key}).

match(Element) ->
	gen_server:cast(?MODULE, {match, Element}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([]) ->
    {ok, []}.

handle_call({write, Key, Element}, _From, State) ->
	NewState = [{Key, Element} | State],
    {reply, ok, NewState};
handle_call({delete, Key}, _From, State) ->
	NewState = lists:keydelete(Key, 1, State),
	{reply, ok, NewState};
handle_call(_Msg, _From, State) ->  
	{noreply, State}.

handle_cast({read, Key}, State) ->
	{Key, Element} = lists:keyfind(Key, 1, State),
	io:format("~p~n", [{Key, Element}]),
	{noreply, State};
handle_cast({match, Element}, State) ->
	Matches = match(Element, State),
	io:format("found elements ~p~n", [Matches]),
	{noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

match(Element, List) ->
	match(Element, List, []).

match(_Element, [], Matches) ->
	Matches;
match(Element, [{Key, Element} | T_List], Matches) ->
	NewMatch = [{Key, Element} | Matches],
	match(Element, T_List, NewMatch);
match(Element, [{_Key, _Element} | T_List], Matches) ->
	match(Element, T_List, Matches).
