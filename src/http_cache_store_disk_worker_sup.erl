%% @private
-module(http_cache_store_disk_worker_sup).

-include("http_cache_store_disk.hrl").

-behaviour(supervisor).

-export([start_link/0, execute/1]).
-export([init/1]).
-export([start_worker/1]).

-define(MAX_WORKER_QUEUE_LEN, 50).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

execute({Type, _} = Cmd)
    when Type == cache_object;
         Type == remote_object_available;
         Type == remote_object_request;
         Type == remote_object_response ->
    case http_cache_store_disk_stats:is_limit_reached() of
        false ->
            do_execute(Cmd);
        true ->
            {error, storage_limit_reached}
    end;
execute(Cmd) ->
    do_execute(Cmd).

do_execute(Cmd) ->
    NbWorkers = ets:lookup_element(?CONFIG_TABLE, nb_workers, 2),
    Partition = erlang:phash2(Cmd) rem NbWorkers,
    Worker = ets:lookup_element(?WORKER_PID_TABLE, Partition, 2),
    case is_priority_command(Cmd) of
        true ->
            gen_server:cast(Worker, Cmd),
            ok;
        false ->
            case worker_is_overloaded(Worker) of
                false ->
                    gen_server:cast(Worker, Cmd),
                    ok;
                true ->
                    {error, concurrency_limit_reached}
            end
    end.

is_priority_command({delete_object, _}) ->
    true;
is_priority_command({invalidate_url, _}) ->
    true;
is_priority_command({invalidate_by_alternate_key, _}) ->
    true;
is_priority_command({warm_me_up, _}) ->
    true;
is_priority_command(_) ->
    false.

worker_is_overloaded(Worker) ->
    {message_queue_len, MessageQueueLen} = erlang:process_info(Worker, message_queue_len),
    MessageQueueLen > max_worker_queue_len().

init(_) ->
    NbWorkers = nb_workers(),
    ets:insert(?CONFIG_TABLE, {nb_workers, NbWorkers}),
    ChildSpecs =
        [#{id => Partition,
           start => {?MODULE, start_worker, [Partition]},
           restart => permanent,
           shutdown => brutal_kill,
           modules => [http_cache_store_disk_worker]}
         || Partition <- lists:seq(0, NbWorkers - 1)],
    {ok, {#{strategy => one_for_one}, ChildSpecs}}.

start_worker(Partition) ->
    case http_cache_store_disk_worker:start_link() of
        {ok, Pid} ->
            register_pid(Partition, Pid),
            {ok, Pid};
        Other ->
            Other
    end.

register_pid(Partition, Pid) ->
    ets:insert(?WORKER_PID_TABLE, {Partition, Pid}).

max_worker_queue_len() ->
    application:get_env(http_cache_store_disk, max_worker_queue_len, ?MAX_WORKER_QUEUE_LEN).

nb_workers() ->
    application:get_env(http_cache_store_disk,
                        nb_workers,
                        erlang:system_info(schedulers_online)).
