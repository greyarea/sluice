-module(sluice).

%% API
-export([start/0, stop/0, add_source/1]).

start() ->
    ensure_started(sasl),
    ok = application:start(sluice).

stop() ->
    ok = application:stop(sluice).

add_source(_Server) ->
    ok.

%% internal functions

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, already_started} ->
            ok;
        Error ->
            Error
    end.
