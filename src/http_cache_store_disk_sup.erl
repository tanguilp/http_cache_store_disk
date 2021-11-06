%% @private
-module(http_cache_store_disk_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).
%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: #{id => Id, start => {M, F, A}}
%% Optional keys are restart, shutdown, type, modules.
%% Before OTP 18 tuples must be used to specify a child. e.g.
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    Children =
        [#{id => http_cache_store_disk_table_holder,
           start => {http_cache_store_disk_table_holder, start_link, []}},
         #{id => http_cache_store_disk_stats,
           start => {http_cache_store_disk_stats, start_link, []}},
         #{id => http_cache_store_disk_expired_resp_sweeper,
           start => {http_cache_store_disk_expired_resp_sweeper, start_link, []}},
         #{id => http_cache_store_disk_outdated_lru_sweeper,
           start => {http_cache_store_disk_outdated_lru_sweeper, start_link, []}},
         #{id => http_cache_store_disk_lru_nuker_memory,
           start => {http_cache_store_disk_lru_nuker_memory, start_link, []}},
         #{id => http_cache_store_disk_lru_nuker_disk,
           start => {http_cache_store_disk_lru_nuker_disk, start_link, []}},
         #{id => http_cache_store_disk_worker_sup,
           start => {http_cache_store_disk_worker_sup, start_link, []},
           type => supervisor}]
        ++ maybe_cluster_mon(),
    {ok, {{one_for_one, 0, 1}, Children}}.

%%====================================================================
%% Internal functions
%%====================================================================

maybe_cluster_mon() ->
    case application:get_env(http_cache_store_disk, cluster_enabled, false) of
        true ->
            [#{id => http_cache_store_disk_cluster_mon,
               start => {http_cache_store_disk_cluster_mon, start_link, []}}];
        false ->
            []
    end.
