%% @private
-module(http_cache_store_disk_table_holder).

-include("http_cache_store_disk.hrl").

-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
    ets:new(?CONFIG_TABLE, [named_table, public]),
    ets:new(?OBJECT_TABLE,
            [ordered_set, named_table, public, compressed, {read_concurrency, true}]),
    ets:new(?LRU_TABLE, [ordered_set, named_table, public]),
    ets:new(?WORKER_PID_TABLE, [named_table, public]),
    http_cache_store_disk_stats:set_limit_reached(false),
    {ok, []}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Request, State) ->
    {noreply, State}.
