%% @private

-module(http_cache_store_disk_app).

-include("http_cache_store_disk.hrl").

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    case http_cache_store_disk_sup:start_link() of
        {ok, Pid} ->
            handle_cache_dir(),
            {ok, Pid};
        Other ->
            Other
    end.

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

handle_cache_dir() ->
    http_cache_store_disk_file:configure_cache_dir().
