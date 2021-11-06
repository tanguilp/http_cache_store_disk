-module(http_cache_store_disk_file).

-include("http_cache_store_disk.hrl").

-export([filepath/1, configure_cache_dir/0, partition/1, cache_dir/0]).

-define(SEP, <<"/">>).

filepath(ObjectKey) ->
    PathWithoutExt = path(ObjectKey),
    <<PathWithoutExt/binary, ?OBJECT_EXT/binary>>.

configure_cache_dir() ->
    [make_path(DirIdx, SubDirIdx)
     || DirIdx <- lists:seq(1, ?NB_DIRS), SubDirIdx <- lists:seq(1, ?NB_SUBDIRS)].

-spec partition(binary()) -> binary().
partition(Path) ->
    PathSplit = binary:split(Path, ?SEP, [trim_all, global]),
    AllParts = [list_to_binary(Partition) || {Partition, _, _} <- disksup:get_disk_data()],
    AllPartsSplit = [binary:split(Part, ?SEP, [trim_all, global]) || Part <- AllParts],
    BiggestPrefix =
        lists:foldl(fun(PartSplit, Acc) ->
                       IsBiggerPrefix =
                           is_path_prefix(PartSplit, PathSplit)
                           andalso length(PartSplit) > length(Acc),
                       case IsBiggerPrefix of
                           true ->
                               PartSplit;
                           false ->
                               Acc
                       end
                    end,
                    [],
                    AllPartsSplit),
    {Part, _} = lists:keyfind(BiggestPrefix, 2, lists:zip(AllParts, AllPartsSplit)),
    Part.

is_path_prefix([Dir | RestPart], [Dir | RestPath]) ->
    is_path_prefix(RestPart, RestPath);
is_path_prefix([], _) ->
    true;
is_path_prefix(_, _) ->
    false.

path(ObjectKey) ->
    ObjectKeyBin = term_to_binary(ObjectKey),
    % We don't take the first byte because otherwise similar filenames would end up similar
    % in the same subdirectories
    <<_, DirIdx:6, SubDirIdx:6, _/bits>> = ObjectKeyHash = crypto:hash(sha224, ObjectKeyBin),
    BaseFilename = base64_url_encode(ObjectKeyHash),
    CacheDir = cache_dir(),
    DirIdxBin = list_to_binary(io_lib:format("~2..0B", [DirIdx])),
    SubDirIdxBin = list_to_binary(io_lib:format("~2..0B", [SubDirIdx])),
    <<CacheDir/binary,
      DirIdxBin/binary,
      ?SEP/binary,
      SubDirIdxBin/binary,
      ?SEP/binary,
      BaseFilename/binary>>.

make_path(DirIdx, SubDirIdx) ->
    CacheDir = cache_dir(),
    DirIdxBin = list_to_binary(io_lib:format("~2..0B", [DirIdx])),
    SubDirIdxBin = list_to_binary(io_lib:format("~2..0B", [SubDirIdx])),
    Path =
        <<CacheDir/binary,
          DirIdxBin/binary,
          <<"/">>/binary,
          SubDirIdxBin/binary,
          <<"/">>/binary>>,
    filelib:ensure_path(Path).

cache_dir() ->
    case application:get_env(http_cache_store_disk, cache_dir) of
        {ok, BaseDirBin} when is_binary(BaseDirBin) ->
            <<BaseDirBin/binary, ?SEP/binary>>;
        {ok, BaseDir} when is_list(BaseDir) ->
            BaseDirBin = list_to_binary(BaseDir),
            <<BaseDirBin/binary, ?SEP/binary>>;
        undefined ->
            erlang:error(cache_directory_not_configured)
    end.

base64_url_encode(Bin) ->
    B64Bin = base64:encode(Bin),
    << <<(urlencode_char(D))>> || <<D>> <= B64Bin, D =/= $= >>.

urlencode_char($/) ->
    $_;
urlencode_char($+) ->
    $-;
urlencode_char(D) ->
    D.
