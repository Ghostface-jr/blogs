-module(my_db_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(normal, _StartArgs) ->
    my_db_sup:start_link().

stop(_State) ->
    ok.

