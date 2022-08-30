
# 0.3.4.0

* Remove use of `readProcess` for some handlers ([#41](https://github.com/iconnect/ridley/pull/41)).
* Make `getDiskStats`, `mkDiskGauge` internal functions ([#41](https://github.com/iconnect/ridley/pull/41)).
* Rename `diskUsageMetrics` to `newDiskUsageMetrics` (([#41](https://github.com/iconnect/ridley/pull/41))).
* Make `registerMetrics` resilient to synchronous exceptions (([#42](https://github.com/iconnect/ridley/pull/42)))