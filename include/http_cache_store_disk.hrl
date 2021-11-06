-ifndef(HTTP_CACHE_STORE_DISK_HRL).

-define(HTTP_CACHE_STORE_DISK_HRL, 1).
-define(CONFIG_TABLE, http_cache_store_disk_table_config).
-define(OBJECT_TABLE, http_cache_store_disk_table_object).
-define(LRU_TABLE, http_cache_store_disk_table_lru).
-define(WORKER_PID_TABLE, http_cache_store_disk_table_worker_pid).
-define(OBJECT_EXT, <<".object">>).
-define(METADATA_EXT, <<".metadata">>).
-define(NB_DIRS, 64).
-define(NB_SUBDIRS, 64).

-endif.
