# Revision history for hydra-github-bridge

## 0.3.0.0 (2026-05-25)

 * Remove multi-instance support in NixOS module:

   ```
   # Old multi-instance style:
   services.hydra-github-bridge.public = { 
     # Service options
   };

   # New style:
   services.hydra-github-bridge = { 
     # Service options
   };
   ```

   Additional NixOS module changes:

   * `ghTokenFile` option has been removed; use `ghSecretFile` instead
   * `enable` default has been changed to `false`
   * `ghAppKeyFile`, `ghAppId` and `ghAppInstallIds` are all now required
   * `ghUserAgent` is no longer required

 * Drop x86_64-darwin support due to nixpkgs deprecation of the platform

 * Make Hydra jobset `keepnr` configurable via environment variable `HYDRA_KEEP_EVALS` or
   NixOS module option `hydraKeepEvals` (default is `2`)

 * Add a watchdog thread that periodically logs per-thread heartbeats and DB/web-server
   diagnostics. The watchdog only logs; it does not take any other action

 * Fix server crash when reading malformed build logs

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
