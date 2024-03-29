# CHANGELOG

All notable changes to this project will be documented in this file.

The format is based on Keep a Changelog and this project adheres to Semantic Versioning.

## [0.3.1] - 2024-03-29

### Fixed

- [`http_cache_store_disk_stats`] Fix an issue that occurs on systems that return only
free memory and system memory (via memsup)

## [0.3.0] - 2023-06-22

### Changed

- [`http_cache_store_disk`] Removed dependency to `http_cache`

## [0.2.2] - 2023-06-19

### Fixed

- [`http_cache_store_disk.erl`] Fix a bug that caused the http cache directory
not being swept on startup. As a consequence, old objects not purged before the
application stopped where stored forever

## [0.2.1] - 2023-06-19

### Changed

- [`http_cache_store_disk.erl`] Switch to OTP26+, so as to benefit from the new
`disksup:get_disk_info/1`

### Fixed

- [`http_cache_store_disk.erl`] Fix a bug that caused workers to crash when an
object was deleted twice by a nuker process

## [0.2.0] - 2023-04-25

Initial release
