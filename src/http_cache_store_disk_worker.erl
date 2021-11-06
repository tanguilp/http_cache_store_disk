%% @private
-module(http_cache_store_disk_worker).

-include_lib("kernel/include/file.hrl").

-include("http_cache_store_disk.hrl").

-define(DELAY_BEFORE_DELETE, 1000).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

init(_) ->
    DeleteListTable = ets:new(delete_list, [set, private]),
    schedule_batch_delete(),
    {ok, #{delete_list_table => DeleteListTable}}.

handle_call(_, _, State) ->
    {reply, ok, State}.

handle_cast({cache_object, Object}, #{delete_list_table := DeleteListTable} = State) ->
    cache_object(Object, DeleteListTable),
    {noreply, State};
handle_cast({delete_object, ObjectKey},
            #{delete_list_table := DeleteListTable} = State) ->
    discard_object(ObjectKey, DeleteListTable),
    {noreply, State};
handle_cast({invalidate_url, UrlDigest}, State) ->
    invalidate_url(UrlDigest),
    {noreply, State};
handle_cast({invalidate_by_alternate_key, AltKeys}, State) ->
    invalidate_by_alternate_key(AltKeys),
    {noreply, State};
handle_cast({warm_me_up, {Node, NbObjects}}, State) ->
    warmup_node(Node, NbObjects),
    {noreply, State};
handle_cast({remote_object_available, {Node, {ObjectKey, Expires}}}, State) ->
    maybe_request_cached_object(Node, ObjectKey, Expires),
    {noreply, State};
handle_cast({remote_object_request, {Node, ObjectKey}}, State) ->
    send_requested_object(Node, ObjectKey),
    {noreply, State};
handle_cast({remote_object_response, Object},
            #{delete_list_table := DeleteListTable} = State = State) ->
    cache_object(Object, DeleteListTable),
    {noreply, State}.

handle_info(batch_delete_objects, #{delete_list_table := DeleteListTable} = State) ->
    batch_delete_objects(DeleteListTable),
    schedule_batch_delete(),
    {noreply, State}.

cache_object({RequestKey, UrlDigest, VaryHeaders, Response, RespMetadata},
             DeleteListTable) ->
    ObjectKey = http_cache_store_disk:object_key(RequestKey, VaryHeaders),
    write_to_disk(ObjectKey, VaryHeaders, UrlDigest, Response, RespMetadata, DeleteListTable).

write_to_disk(ObjectKey,
              VaryHeaders,
              UrlDigest,
              Response,
              RespMetadata,
              DeleteListTable) ->
    {Status, RespHeaders, RespBody} = Response,
    Now = unix_now(second),
    FilePath = http_cache_store_disk_file:filepath(ObjectKey),
    % The sendfile system call used when a file is sent is not atomic, and
    % a file being overwritten at the same moment will result in garbage
    % being sent instead of the original file.
    % By deleting and recreating the file, we expect a new file and FD to
    % be created. The deleted file is still sent by already initiated senfile
    file:delete(FilePath),
    case file:write_file(FilePath, RespBody, [raw]) of
        ok ->
            ets:insert(?OBJECT_TABLE,
                       {ObjectKey,
                        VaryHeaders,
                        UrlDigest,
                        {Status, RespHeaders, file},
                        RespMetadata,
                        Now}),
            ets:insert(?LRU_TABLE, {{Now, ObjectKey}}),
            ets:delete(DeleteListTable, ObjectKey),
            Expires = map_get(grace, RespMetadata),
            http_cache_store_disk_cluster_mon:broadcast_object_available(ObjectKey, Expires);
        {error, _} = Error ->
            Error
    end.

discard_object(ObjectKey, DeleteListTable) ->
    case ets:lookup_element(?OBJECT_TABLE, ObjectKey, 4) of
        {_Status, _RespHeaders, file} ->
            ets:insert(DeleteListTable, {ObjectKey, unix_now(millisecond)});
        _ ->
            ok
    end,
    ets:delete(?OBJECT_TABLE, ObjectKey).

invalidate_url(UrlDigest) ->
    invalidate_url(UrlDigest, ets:first(?OBJECT_TABLE)).

invalidate_url(_UrlDigest, '$end_of_table') ->
    ok;
invalidate_url(UrlDigest, ObjectKey) ->
    case ets:lookup(?OBJECT_TABLE, ObjectKey) of
        [{_, _, UrlDigest, _, _, _}] ->
            http_cache_store_disk:delete_object(ObjectKey, url_invalidation),
            invalidate_url(UrlDigest, ets:next(?OBJECT_TABLE, ObjectKey));
        _ ->
            invalidate_url(UrlDigest, ets:next(?OBJECT_TABLE, ObjectKey))
    end.

invalidate_by_alternate_key(AltKeys) ->
    invalidate_by_alternate_key(AltKeys, ets:first(?OBJECT_TABLE)).

invalidate_by_alternate_key(_AltKeys, '$end_of_table') ->
    ok;
invalidate_by_alternate_key(AltKeys, ObjectKey) ->
    case ets:lookup(?OBJECT_TABLE, ObjectKey) of
        [{_, _, _, _, #{alternate_keys := ObjectAltKeys}, _}] ->
            case lists:any(fun(AltKey) -> lists:member(AltKey, ObjectAltKeys) end, AltKeys) of
                true ->
                    http_cache_store_disk:delete_object(ObjectKey, alternate_key_invalidation),
                    invalidate_by_alternate_key(AltKeys, ets:next(?OBJECT_TABLE, ObjectKey));
                false ->
                    invalidate_by_alternate_key(AltKeys, ets:next(?OBJECT_TABLE, ObjectKey))
            end;
        _ ->
            invalidate_by_alternate_key(AltKeys, ets:next(?OBJECT_TABLE, ObjectKey))
    end.

warmup_node(Node, NbObjects) ->
    warmup_node(Node, ets:last(?LRU_TABLE), NbObjects).

warmup_node(_Node, '$end_of_table', _NbObjects) ->
    ok;
warmup_node(_Node, _Key, 0) ->
    ok;
warmup_node(Node, {Timestamp, ObjectKey} = LRUKey, NbObjects) ->
    case ets:lookup(?OBJECT_TABLE, ObjectKey) of
        [{{RequestKey, _}, VaryHeaders, UrlDigest, Response, RespMetadata, Timestamp}] ->
            case response_with_bin_body(ObjectKey, Response) of
                {ok, ResponseWithBinBody} ->
                    CachedObject =
                        {RequestKey, UrlDigest, VaryHeaders, ResponseWithBinBody, RespMetadata},
                    http_cache_store_disk_cluster_mon:send_cached_object(Node, CachedObject),
                    warmup_node(Node, ets:prev(?LRU_TABLE, LRUKey), NbObjects - 1);
                {error, _} ->
                    warmup_node(Node, ets:prev(?LRU_TABLE, LRUKey), NbObjects)
            end;
        _ ->
            warmup_node(Node, ets:prev(?LRU_TABLE, LRUKey), NbObjects)
    end.

send_requested_object(Node, ObjectKey) ->
    case ets:lookup(?OBJECT_TABLE, ObjectKey) of
        [{{RequestKey, _}, VaryHeaders, UrlDigest, Response, RespMetadata, _}] ->
            case response_with_bin_body(ObjectKey, Response) of
                {ok, ResponseWithBinBody} ->
                    CachedObject =
                        {RequestKey, UrlDigest, VaryHeaders, ResponseWithBinBody, RespMetadata},
                    http_cache_store_disk_cluster_mon:send_cached_object(Node, CachedObject);
                {error, _} ->
                    ok
            end;
        [] ->
            ok
    end.

response_with_bin_body(ObjectKey, {Status, RespHeaders, file}) ->
    FilePath = http_cache_store_disk_file:filepath(ObjectKey),
    case file:read_file(FilePath) of
        {ok, RespBody} ->
            {ok, {Status, RespHeaders, RespBody}};
        {error, _} = Error ->
            Error
    end.

maybe_request_cached_object(Node, ObjectKey, RemoteExpires) ->
    case ets:lookup(?OBJECT_TABLE, ObjectKey) of
        [] ->
            http_cache_store_disk_cluster_mon:request_cached_object(Node, ObjectKey);
        [{_, _, _, _, #{grace := Expires}, _}] when Expires < RemoteExpires ->
            http_cache_store_disk_cluster_mon:request_cached_object(Node, ObjectKey);
        _ ->
            ok
    end.

batch_delete_objects(DeleteListTable) ->
    Now = unix_now(millisecond),
    MinTime = Now - delay_before_delete(),
    MatchSpec = [{{'$1', '$2'}, [{'<', '$2', MinTime}], ['$1']}],
    ToDelete = ets:select(DeleteListTable, MatchSpec),
    lists:map(fun(ObjectKey) ->
                 FilePath = http_cache_store_disk_file:filepath(ObjectKey),
                 ets:delete(DeleteListTable, ObjectKey),
                 file:delete(FilePath)
              end,
              ToDelete).

schedule_batch_delete() ->
    timer:send_after(1000, batch_delete_objects).

delay_before_delete() ->
    application:get_env(http_cache_store_disk, delay_before_delete, ?DELAY_BEFORE_DELETE).

unix_now(Unit) ->
    os:system_time(Unit).
