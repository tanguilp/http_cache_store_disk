%% @private
-module(http_cache_store_disk_cluster_mon).

-behaviour(gen_server).

-define(WARMUP_NB_OBJECTS, 5000).
-define(WARMUP_TIMEOUT, 20 * 1000).

-export([broadcast_invalidate_url/1, broadcast_invalidate_by_alternate_key/1,
         broadcast_object_available/2, request_cached_object/2, send_cached_object/2]).
-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2]).

broadcast_invalidate_url(UrlDigest) ->
    gen_server:abcast(nodes(), ?MODULE, {invalidate_url, UrlDigest}).

broadcast_invalidate_by_alternate_key(AltKeys) ->
    gen_server:abcast(nodes(), ?MODULE, {invalidate_by_alternate_key, AltKeys}).

broadcast_object_available(ObjectKey, Expires) ->
    gen_server:abcast(nodes(),
                      ?MODULE,
                      {remote_object_available, {node(), {ObjectKey, Expires}}}).

request_cached_object(Node, ObjectKey) ->
    gen_server:cast({?MODULE, Node}, {remote_object_request, {node(), ObjectKey}}).

send_cached_object(Node, Object) ->
    gen_server:cast({?MODULE, Node}, {remote_object_response, Object}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
    net_kernel:monitor_nodes(true),
    WarmupTimeout =
        application:get_env(http_cache_store_disk, warmup_timeout, ?WARMUP_TIMEOUT),
    timer:send_after(WarmupTimeout, stop_warmup),
    {ok, []}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({_, _} = Cmd, State) ->
    http_cache_store_disk_worker_sup:execute(Cmd),
    {noreply, State}.

handle_info(stop_warmup, State) ->
    net_kernel:monitor_nodes(false),
    {noreply, State};
handle_info({nodeup, Node}, State) ->
    NbObjects =
        application:get_env(http_cache_store_disk, warmup_nb_objects, ?WARMUP_NB_OBJECTS),
    gen_server:cast({?MODULE, Node}, {warm_me_up, {node(), NbObjects}}),
    {noreply, State};
handle_info(_Request, State) ->
    {noreply, State}.
