%% @private
-module(http_cache_store_disk_expired_resp_sweeper).

-include("http_cache_store_disk.hrl").

-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2]).

-define(DEFAULT_INTERVAL, 3 * 1000).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
    schedule_sweep(),
    {ok, []}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(sweep, State) ->
    telemetry:span([http_cache_store_disk, expired_resp_sweeper],
                   #{},
                   fun() ->
                      sweep(),
                      {ok, #{}}
                   end),
    schedule_sweep(),
    {noreply, State}.

sweep() ->
    do_sweep(ets:first(?OBJECT_TABLE)).

do_sweep('$end_of_table') ->
    ok;
do_sweep(ObjectKey) ->
    Now = unix_now(),
    case ets:lookup(?OBJECT_TABLE, ObjectKey) of
        [{_, _, _, _, #{grace := Expires}, _}] when Expires =< Now ->
            http_cache_store_disk:delete_object(ObjectKey, expired);
        _ ->
            ok
    end,
    do_sweep(ets:next(?OBJECT_TABLE, ObjectKey)).

schedule_sweep() ->
    erlang:send_after(sweep_interval(), self(), sweep).

sweep_interval() ->
    application:get_env(http_cache_store_disk,
                        expired_resp_sweep_interval,
                        ?DEFAULT_INTERVAL).

unix_now() ->
    os:system_time(second).
