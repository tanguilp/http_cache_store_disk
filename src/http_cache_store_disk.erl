%% @private
-module(http_cache_store_disk).

-include("http_cache_store_disk.hrl").

-behaviour(http_cache_store_behaviour).

-export([list_candidates/2, get_response/2, put/6, notify_response_used/2,
         invalidate_url/2, invalidate_by_alternate_key/2, delete_object/2, object_key/3, lru/2]).

list_candidates(RequestKey, _Opts) ->
    Spec =
        [{{{RequestKey, '$1', '$2'}, '$3', '_', {'$4', '$5', '_'}, '$6', '_'},
          [],
          [['$1', '$2', '$3', '$4', '$5', '$6']]}],
    Now = os:system_time(second),
    [{{RequestKey, VaryKeyPart, ChunkKeyPart}, Status, RespHeaders, VaryHeaders, RespMetadata}
     || [VaryKeyPart, ChunkKeyPart, VaryHeaders, Status, RespHeaders, RespMetadata]
            <- ets:select(?OBJECT_TABLE, Spec),
        Now < map_get(grace, RespMetadata)].

get_response(ObjectKey, _Opts) ->
    case ets:lookup(?OBJECT_TABLE, ObjectKey) of
        [{ObjectKey,
          _VaryHeaders,
          _UrlDigest,
          {Status, RespHeaders, file},
          RespMetadata,
          _Timestamp}] ->
            FilePath = http_cache_store_disk_file:filepath(ObjectKey),
            {Status, RespHeaders, {file, FilePath}, RespMetadata};
        [] ->
            undefined
    end.

put(RequestKey, UrlDigest, VaryHeaders, Response, #{grace := _} = RespMetadata, _Opts) ->
    http_cache_store_disk_worker_sup:execute({cache_object,
                                              {RequestKey,
                                               UrlDigest,
                                               VaryHeaders,
                                               Response,
                                               RespMetadata}}).

invalidate_url(UrlDigest, _Opts) ->
    http_cache_store_disk_cluster_mon:broadcast_invalidate_url(UrlDigest),
    case http_cache_store_disk_worker_sup:execute({invalidate_url, UrlDigest}) of
        ok ->
            {ok, undefined};
        {error, _} = Error ->
            Error
    end.

invalidate_by_alternate_key(AltKeys, _Opts) ->
    http_cache_store_disk_cluster_mon:broadcast_invalidate_by_alternate_key(AltKeys),
    case http_cache_store_disk_worker_sup:execute({invalidate_by_alternate_key, AltKeys}) of
        ok ->
            {ok, undefined};
        {error, _} = Error ->
            Error
    end.

notify_response_used(ObjectKey, _Opts) ->
    Now = os:system_time(second),
    case ets:update_element(?OBJECT_TABLE, ObjectKey, {6, Now}) of
        true ->
            lru(ObjectKey, Now),
            ok;
        false ->
            ok
    end.

delete_object(ObjectKey, Reason) ->
    telemetry:execute([http_cache_store_disk, object_deleted], #{}, #{reason => Reason}),
    http_cache_store_disk_worker_sup:execute({delete_object, ObjectKey}),
    ok.

object_key(RequestKey, VaryHeaders, RespMetadata) ->
    {RequestKey,
     crypto:hash(sha256, erlang:term_to_binary(VaryHeaders)),
     chunk_id(RespMetadata)}.

chunk_id(#{parsed_headers := #{<<"content-range">> := {Unit, Start, End, Len}}}) ->
    UnitBin =
        if is_atom(Unit) ->
               atom_to_binary(Unit);
           true ->
               Unit
        end,
    StartBin = integer_to_binary(Start),
    EndBin = integer_to_binary(End),
    LenBin =
        case Len of
            '*' ->
                <<"*">>;
            _ ->
                integer_to_binary(Len)
        end,
    <<UnitBin/binary, " ", StartBin/binary, "-", EndBin/binary, "/", LenBin/binary>>;
chunk_id(_RespMetadata) ->
    <<>>.

lru(ObjectKey, Timestamp) ->
    case ets:update_element(?OBJECT_TABLE, ObjectKey, {6, Timestamp}) of
        true ->
            ets:insert(?LRU_TABLE, {{Timestamp, ObjectKey}});
        false ->
            ok
    end.
