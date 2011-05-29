-module(sluice_poller).

-behaviour(gen_fsm).

%% API
-export([start_link/2]).

%% gen_fsm exports
-export([init/1,
         terminate/3,
         code_change/4,
         handle_event/3,
         handle_sync_event/4,
         handle_info/3,
         connecting/2,
         polling/2]).

-include("sluice_logging.hrl").

-define(TIME_BETWEEN_YEAR_ZERO_AND_1970, 62167219200).

-record(sd, {source, sink, sink_sock, req_id}).

start_link(Config1, Config2) ->
    gen_fsm:start_link(?MODULE, Config1 ++ Config2, []).

init(Config) ->
    Source = proplists:get_value(source, Config),
    Sink = proplists:get_value(sink, Config),
    {ok, connecting, #sd{source=Source, sink=Sink}, 0}.

terminate(_Reason, _StateName, _StateData) ->
    ok.

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

handle_event(_Event, _StateName, StateData) ->
    {stop, unknown_event, StateData}.

handle_sync_event(_Event, _From, _StateName, StateData) ->
    {stop, unknown_sync_event, StateData}.

handle_info(HttpInfo = {http, _}, StateName, StateData) ->
    ?MODULE:StateName(HttpInfo, StateData);
handle_info({tcp, SinkSock, _}, StateName,
            StateData = #sd{sink_sock=SinkSock}) ->
    %% ignore incoming data from the sink
    inet:setopts(SinkSock, [{active, once}]),
    {next_state, StateName, StateData};
handle_info({tcp_closed, SinkSock}, _StateName,
            StateData = #sd{sink_sock=SinkSock}) ->
    {stop, disconnected_from_sink, StateData};
handle_info({tcp_error, SinkSock, Reason}, _StateName,
            StateData = #sd{sink_sock=SinkSock}) ->
    {stop, {tcp_error, Reason}, StateData}.

connecting(timeout, StateData = #sd{sink={SinkHost, SinkPort}}) ->
    {ok, SinkSock} = gen_tcp:connect(SinkHost, SinkPort,
                                     [binary, {active, once}, {packet, line}]),
    schedule_first_poll(),
    {next_state, polling, StateData#sd{sink_sock=SinkSock}}.

polling(poll, StateData = #sd{source=Source}) ->
    ?INFO("starting poll"),
    ReqId = start_poll(Source),
    {next_state, polling, StateData#sd{req_id=ReqId}};
polling({http, {ReqId, {error, Reason}}}, StateData = #sd{req_id=ReqId}) ->
    ?INFO("poll error: ~p", [Reason]),
    schedule_retry_poll(),
    {next_state, polling, StateData#sd{req_id=undefined}};
polling({http, {ReqId, {{_, 200, _}, _, Body}}},
        StateData = #sd{sink_sock=SinkSock, req_id=ReqId}) ->
    {struct, Props} = mochijson2:decode(Body),

    Metrics = proplists:get_value(<<"metrics">>, Props),
    Lines = json_to_lines(Metrics),
    ?INFO("sending ~p", [Lines]),
    gen_tcp:send(SinkSock, Lines),

    schedule_next_poll(),
    {next_state, polling, StateData#sd{req_id=undefined}};
polling({http, {ReqId, Reply}}, StateData = #sd{req_id=ReqId}) ->
    ?INFO("poll non-200 reply: ~p", [Reply]),
    {next_state, polling, StateData#sd{req_id=undefined}}.

%% internal functions

schedule_first_poll() ->
    schedule_poll(0).

schedule_next_poll() ->
    schedule_poll(60).

schedule_retry_poll() ->
    schedule_poll(10).

schedule_poll(Seconds) ->
    gen_fsm:send_event_after(Seconds * 1000, poll),
    ok.

start_poll({Host, Port}) ->
    Url = lists:flatten(io_lib:format("http://~s:~b/", [Host, Port])),
    {ok, ReqId} = httpc:request(get, {Url, []}, [],
                                [{sync, false}, {receiver, self()}]),
    ReqId.

json_to_lines({struct, Props}) ->
    Stamp = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
    UnixStamp = Stamp - ?TIME_BETWEEN_YEAR_ZERO_AND_1970,
    
    lists:flatten([io_lib:format("~s ~w ~w~n", [Key, Val, UnixStamp]) ||
                      {Key, Val} <- Props]).
    
