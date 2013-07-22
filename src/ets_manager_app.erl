-module(ets_manager_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

-spec start(atom(), application:restart_type()) -> ok | {error, term()}.
start(_StartType, _StartArgs) ->
    ets_manager_sup:start_link().

-spec stop(atom()) -> ok.
stop(_State) ->
    ok.
