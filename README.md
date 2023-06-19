http_cache_store_disk
=====

`http_cache_store_disk` is an disk LRU cache that can be used as a backend for `http_cache`.
It implements the `http_cache_store` behaviour.

It supports:
- on-disk caching, with limit in % of disk usage
- clustering, using BEAM distribution. The following events are broadcast:
  - newly cached HTTP responses (in an efficient manner)
  - invalidation requests
  - warmup: already present nodes send their most recently used cached HTTP responses to joining nodes
- telemetry events (see [Telemetry](#telemetry))
- backpressure mechanisms to avoid overloading the whole system with caching operations
- the optional `http_cache_store:invalidate_by_alternate_key/1` callback

Under the hood, it simply saves the HTTP response body on disk as a file (other HTTP caches use other methods)
and related libraries (such as `plug_http_cache`) use the `sendfile` system call when available.
This enables sending files extremely fast because:
- this avoids going back and forth from userland and kernel. All sending operation is done in the
kernel, from the file to the socket directly
- read files are cached in-memory by the kernel. That is, popular content will keep in memory and
send to the socket direct, without being reread from the disk. In reality, this implementation is
a **memory + disk** backend for `http_cache`, the memory part being handled directly by the kernel

Stored responses are nuked as soon as a configurable disk space occupation threshold is reached.
It does not support configuration of a fixed amount of bytes for disk usage, mainly because:
- the number of bytes does not reflect the disk usage, since file occupy more space that their
number of bytes (see the `--apparent-size` option of the `du` program for instance)
- the author couldn't actualy have it working, probably for the reason mentioned above

For in-memory caching, see: [`http_cache_store_memory`](https://github.com/tanguilp/http_cache_store_memory).

## Support

OTP26+

## Usage

This is an OTP application, and automatically starts.

### Setting the right thresholds

Metadata about HTTP responses written on disk are stored in-memory. The overhead is about 1kb per
stored response.

Therefore, 1 million stored responses will occupy around 1GB or memory.

When using this store as a backend for library that uses the `sendfile` system call,
such as [`plug_http_cache`](https://github.com/tanguilp/plug_http_cache), you should take into
consideration that the kernel caches responses in memory. In other words, if caching metadata
takes a huge amount of memory (say 99%), then you will not benefit from the automatic caching
from the kernel and files will be read from the disk every time they are sent, resulting in
slower sending operations.

Memory limit is set to `0.7` for this purpose. If you use very rapid disk (SSD), you might want to
reconsider this default.

### Configuration parameters

- `cache_dir` [**Mandatory**]: the directory where to store cache data. If it does not exists, it is
created. No default. **Beware**: this directory is irreversibly swept on startup. Don't set `/` or
even `/tmp`!
- `disk_limit`: maximum disk usage as a float. Above this limit, oldest objects start being nuked.
Defaults to `0.92`. Note that some file systems start performing poorly when approaching the 100%
mark
- `memory_limit`: how much memory is allocated for metadata.
If this is an integer, then it's the number of bytes allocated to store the cached
responses. If it is a float, it's the system memory threshold that triggers nuking older entries.
Defaults to `0.7`, that is, as soon as 70% of the system memory is used, objects are deleted until
system memory use no longer exceeds this threshold
- `delay_before_delete`: when a cached object is deleted, it's kept on disk for some time to allow
reading the file content before deletion (for example from your code) and avoid race condition.
When using this backend's API directly, you should always consider the case when the file is deleted
before you can actually read it. Note that the `sendfile` syscall doesn't care if the file is deleted
while being sent (the kernel keeps the file somewhere until it is sent in full). Defaults to
`1000` ms
- `max_worker_queue_len`: max number of objects in the workers' mailbox before they start
discarding it. Defaults to 50
- `cluster_enabled`: exchange of information between nodes of the Erlang cluster is enabled.
Defaults to `false`
- `nb_workers`: how many workers are to be working at the same time for adding new cache
entries (including from remote nodes). Defaults to the number of active schedulers
- `pull_table_stats_interval`: how often memory stats are retrieved and associated telemetry event
emitted, in milliseconds. Defaults to `1000`
- `warmup_nb_objects`: how many objects are sent to joining nodes when they request warm-up.
Default to `5000`
- `warmup_timeout`: how long the warmup process is active, that is it tries to get objects from
joining nodes, in milliseconds. Default to `20000`
- `disk_limit_check_interval`: how often to check for disk limit, and trigger LRU nuking when
exceeded, in milliseconds. Defaults to `60000`. Take under consideration that the Unix `df` program
is called, so you should not call it too often (< 1 second).
- `mem_limit_check_interval`: how often to check for memory limit, and trigger LRU nuking when
exceeded, in milliseconds. Defaults to `1000`.
- `expired_resp_sweep_interval`: how often expired responses are purged, in milliseconds.
Defaults to `3000`
- `outdated_lru_sweep_interval`: how often outdated LRU entries are purged, in milliseconds.
Defaults to `2000`

The following options can be modified at runtime:
- `disk_limit`
- `memory_limit`
- `delay_before_delete`
- `max_worker_queue_len`
- `pull_table_stats_interval`
- `disk_limit_check_interval`
- `mem_limit_check_interval`
- `expired_resp_sweep_interval`
- `outdated_lru_sweep_interval`

## Installation

Erlang (rebar3):

```erlang
{deps, [{http_cache_store_disk, "~> 0.2.2"}]}.
```

Elixir:

```elixir
{:http_cache_store_disk, "~> 0.2.2"}
```

## Telemetry

- `[http_cache_store_disk, object_deleted]` is emitted whenever an object is deleted
  - Measurements: none
  - Metadata:
    - `reason`: one of `lru_nuked_memory`, `lru_nuked_disk`, `expired`, `url_invalidation`, `alternate_key_invalidation`
- `[http_cache_store_disk, memory]` is emitted regularly by the stats service
  - Measurements:
    - `total_mem`: total memory used by `http_cache_store_disk` subsystems
    - `objects_mem`: memory used by `http_cache_store_disk` to store HTTP responses
    - `lru_mem`: memory used by `http_cache_store_disk` to store LRU data
    - `objects_count`: number of HTTP responses cached
  - Metadata: none
- `[http_cache_store_disk, lru_nuker]`: events triggered by the LRU nuker process
(uses `telemetry:span/3`)
- `[http_cache_store_disk, expired_lru_entry_sweeper]`: events triggered by the LRU sweeper process
(uses `telemetry:span/3`)
- `[http_cache_store_disk, expired_resp_sweeper]`: events triggered by the outdated response
sweeper process (uses `telemetry:span/3`)
