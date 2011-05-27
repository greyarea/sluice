-module(sluice).

%% API
-export([start/0, stop/0]).

start() ->
    ensure_started(sasl),
    application:start(warlock).

stop() ->
    application:stop(warlock).

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
