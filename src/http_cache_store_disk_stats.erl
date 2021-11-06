%% @private
-module(http_cache_store_disk_stats).

-include("http_cache_store_disk.hrl").

-behaviour(gen_server).

-export([allocated_memory_used/0, set_limit_reached/1, is_limit_reached/0]).
-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2]).

-define(DEFAULT_INTERVAL, 1000).
-define(DEFAULT_LIMIT, 0.7).

set_limit_reached(IsReached) ->
    ets:insert(?CONFIG_TABLE, {limit_reached, IsReached}),
    ok.

is_limit_reached() ->
    [{_, IsReached}] = ets:lookup(?CONFIG_TABLE, limit_reached),
    IsReached.

allocated_memory_used() ->
    MemoryLimit = application:get_env(http_cache_store_disk, memory_limit, ?DEFAULT_LIMIT),
    allocated_memory_used(MemoryLimit).

allocated_memory_used(0.0) ->
    1.0;
allocated_memory_used(0) ->
    1.0;
allocated_memory_used(Ratio) when is_float(Ratio) ->
    system_memory_use() / Ratio;
allocated_memory_used(MaxSize) when is_integer(MaxSize) ->
    (table_memory_used(?OBJECT_TABLE) + table_memory_used(?LRU_TABLE)) / MaxSize.

system_memory_use() ->
    system_memory_use(maps:from_list(
                          memsup:get_system_memory_data())).

system_memory_use(#{available_memory := Available, system_total_memory := Total}) ->
    1 - Available / Total;
system_memory_use(MemData) ->
    Cached = map_get(cached_memory, MemData),
    Buffered = map_get(buffered_memory, MemData),
    Free = map_get(free_memory, MemData),
    Total = map_get(system_total_memory, MemData),
    1 - (Cached + Buffered + Free) / Total.

table_memory_used(Table) ->
    WordSize = persistent_term:get(os_word_size),
    ets:info(Table, memory) * WordSize.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
    WordSize = memsup:get_os_wordsize() div 8,
    persistent_term:put(os_word_size, WordSize),
    schedule_collect(),
    {ok,
     #{total_mem => 0,
       objects_mem => 0,
       objects_count => 0,
       lru_mem => 0}}.

handle_call(stats, _From, Stats) ->
    {reply, Stats, Stats}.

handle_cast({}, Stats) ->
    {noreply, Stats}.

handle_info(collect_stats, _Stats) ->
    ObjectsMem = table_memory_used(?OBJECT_TABLE),
    ObjectsCount = ets:info(?OBJECT_TABLE, size),
    LRUMem = table_memory_used(?LRU_TABLE),
    Stats =
        #{total_mem => ObjectsMem + LRUMem,
          objects_mem => ObjectsMem,
          objects_count => ObjectsCount,
          lru_mem => LRUMem},
    telemetry:execute([http_cache_store_disk, memory], Stats, #{}),
    schedule_collect(),
    {noreply, Stats}.

schedule_collect() ->
    erlang:send_after(collect_interval(), self(), collect_stats).

collect_interval() ->
    application:get_env(http_cache_store_disk, pull_table_stats_interval, ?DEFAULT_INTERVAL).
