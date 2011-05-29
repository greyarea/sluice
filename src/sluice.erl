-module(sluice).

%% API
-export([start/0, stop/0, add_source/1]).

start() ->
    ensure_started(sasl),
    ensure_started(inets),
    ok = application:start(sluice).

stop() ->
    ok = application:stop(sluice).

add_source(Source) ->
    supervisor:start_child(sluice_sup, [[{source, Source}]]).

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
