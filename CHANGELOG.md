# CHANGELOG

All notable changes to this project will be documented in this file.

The format is based on Keep a Changelog and this project adheres to Semantic Versioning.

## [0.2.1] - 2023-06-19

### Changed

- [`http_cache_store_disk.erl`] Switch to OTP26+, so as to benefit from the new
`disksup:get_disk_info/1`

### Fixed

- [`http_cache_store_disk.erl`] Fix a bug that caused workers to crash when an
object was deleted twice by a nuker process

## [0.2.0] - 2023-04-25

Initial release
