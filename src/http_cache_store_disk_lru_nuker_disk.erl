%% @private
-module(http_cache_store_disk_lru_nuker_disk).

-include_lib("kernel/include/file.hrl").

-include("http_cache_store_disk.hrl").

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-define(DEFAULT_DISK_LIMIT, 0.92).
-define(DEFAULT_DISK_LIMIT_CHECK_INTERVAL, 60000).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
    CacheDir = http_cache_store_disk_file:cache_dir(),
    schedule_check(),
    {ok, CacheDir}.

handle_call(_Request, _From, CacheDir) ->
    {reply, ok, CacheDir}.

handle_cast(_Request, CacheDir) ->
    {noreply, CacheDir}.

handle_info(check, CacheDir) ->
    telemetry:span([http_cache_store_disk, lru_nuker, disk],
                   #{},
                   fun() ->
                      check_disk_usage(CacheDir),
                      {ok, #{}}
                   end),
    schedule_check(),
    {noreply, CacheDir}.

check_disk_usage(CacheDir) ->
    {DiskSize, DiskUsage} = get_disk_usage(CacheDir),
    DiskLimit = disk_limit(),
    case DiskUsage > DiskLimit of
        true ->
            http_cache_store_disk_stats:set_limit_reached(true),
            BytesToDelete = ceil((DiskUsage - DiskLimit) * DiskSize),
            nuke_objects(BytesToDelete),
            http_cache_store_disk_stats:set_limit_reached(false);
        false ->
            ok
    end.

nuke_objects(BytesToDelete) when BytesToDelete =< 0 ->
    ok;
nuke_objects(BytesToDelete) ->
    case ets:first(?LRU_TABLE) of
        '$end_of_table' ->
            ok;
        {Timestamp, ObjectKey} = LRUKey ->
            case ets:lookup(?OBJECT_TABLE, ObjectKey) of
                [{_, _, _, {_Status, RespHeaders, file}, _, Timestamp}] ->
                    ContentLengthBin = proplists:get_value(<<"content-length">>, RespHeaders),
                    ContentLength = erlang:binary_to_integer(ContentLengthBin),
                    ets:delete(?LRU_TABLE, LRUKey),
                    http_cache_store_disk:delete_object(ObjectKey, lru_nuked_disk),
                    nuke_objects(BytesToDelete - ContentLength);
                % either the object exists, but with another sequence number which means the
                % current LRU key is outdated (a more recent one exists), or the object doesn't
                % exist anymore
                _ ->
                    ets:delete(?LRU_TABLE, LRUKey),
                    nuke_objects(BytesToDelete)
            end
    end.

get_disk_usage(CacheDir) ->
    case disksup:get_disk_info(CacheDir) of
        [{_, 0, 0, 0}] ->
            logger:error("Invalid disk usage data, disksup does not seem to work. Fix it or cache directory will fill without any limitation"),
            {0, 0};
        [{_, TotalKib, _, Capacity}] ->
            {TotalKib * 1024, Capacity / 100}
    end.

schedule_check() ->
    erlang:send_after(check_interval(), self(), check).

disk_limit() ->
    application:get_env(http_cache_store_disk, disk_limit, ?DEFAULT_DISK_LIMIT).

check_interval() ->
    application:get_env(http_cache_store_disk,
                        disk_limit_check_interval,
                        ?DEFAULT_DISK_LIMIT_CHECK_INTERVAL).
