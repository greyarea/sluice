-module(sluice_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% supervisor callbacks
-export([init/1]).

start_link(Config) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Config).

init(Config) ->
    Pollers = {undefined,
               {sluice_poller, start_link, [Config]},
               permanent,
               5000,
               worker,
               [sluice_poller]},
    {ok, {{simple_one_for_one, 5, 10}, [Pollers]}}.
