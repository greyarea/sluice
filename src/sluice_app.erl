-module(sluice_app).

-behaviour(application).

%% application callbacks
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Config = [{sink, env(sink)}],
    case sluice_sup:start_link(Config) of
        {ok, _} = Res ->
            Sources = env(sources),
            lists:foreach(
              fun(Source) ->
                      {ok, _} = supervisor:start_child(sluice_sup,
                                                       [[{source, Source}]])
              end, Sources),
            Res
    end.

stop(_State) ->
    ok.

%% internal functions

env(Key) ->
    case application:get_env(sluice, Key) of
        {ok, Val} ->
            Val
    end.
        
            
