# Revision history for hydra-github-bridge

## UNRELEASED (YYYY-MM-DD)

 * Make check-run name prefix and job filtering configurable via environment variables 
   and NixOS module options. The NixOS module accepts the following new options:

    * checkRunPrefix
    * filterJobs

 * Add a Server-Sent Events HTTP server for streaming Hydra build status. This is enabled
   by the new NixOS module options:

    * enableSse
    * ssePort

 * Fix severe DB load caused by the worker dequeue query scanning the entire
   `github_status_payload` table on every poll. Replaced with a query using
   `NOT EXISTS` and a partial index, and added a periodic pruning thread to
   discard superseded notifications.


## 0.2.1.0 (2026-03-10)

 * Add a `waitForHydraServerPort` NixOS option that adds an ExecStartPost to hydra-server
   so that it waits for the port to become ready.

## 0.2.0.0 (2026-03-10)

 * Consolidate `github-hydra-bridge` and `hydra-github-bridge`

   The bidirectional bridge functionality is now in a single executable
   `hydra-github-bridge`. The `github-hydra-bridge` package and NixOS module have been
   removed. The `hydra-github-bridge` NixOS module now accepts the following new options:

    * ghSecretFile
    * hydraUser
    * hydraPassFile
    * port

 * Change format of `GITHUB_APP_INSTALL_IDS`

   Now uses delimited format (`"org1=1234,org2=5678"`) rather than Haskell syntax
   (`"[(\"org1\", 1234)]"`). The NixOS module now accepts attrsets, eg:

     ghAppInstallIds = {
       org1 = 1234;
       org2 = 5678;
     };

 * Validate GitHub app installation IDs against the configured whitelist before processing
   webhooks to prevent unauthorized repos from being built.

 * Fix crash when refreshing authorization tokens if unauthorized repos are found.

 * Upgrade to GHC 9.12 and update all dependencies

 * Add local testing support

   Add comprehensive NixOS integration tests and a Nix flake devShell providing a mock
   GitHub server (mockoon-cli) and a script to send fake GitHub webhooks (fake-send-webhook).

 * Fix small NixOS module issues

   - Compare `ghAppKeyFile` with `null` (not `""`)
   - Compare `hydraPassFile` with `null` (not `""`)
   - Compare `ghAppInstallIds` with `"[]"` (not `[]`)

## 0.1.0.0

* Initial release
