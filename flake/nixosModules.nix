{moduleWithSystem, ...}: {
  flake.nixosModules = {
    hydra-github-bridge = moduleWithSystem (perSystem @ {config}: {
      config,
      lib,
      pkgs,
      ...
    }: let
      cfg = config.services.hydra-github-bridge;
    in {
      options.services.hydra-github-bridge = with lib; {
        enable =
          mkEnableOption "hydra github bridge"
          // {
            default = true;
          };

        package = mkOption {
          type = types.package;
          default = perSystem.config.packages.hydra-github-bridge;
          defaultText = "hydra-github-bridge";
          description = "The hydra to github webhook bridge";
        };

        ghAppId = mkOption {
          type = types.int;
          default = 0;
          description = ''
            The GitHub App ID to sign authentication JWTs with.
          '';
        };

        ghAppInstallIds = mkOption {
          type = types.attrsOf types.int;
          default = {};
          description = ''
            Mapping of organization names to GitHub App installation ids to
            authenticate with.
          '';
        };

        ghUserAgent = mkOption {
          type = types.str;
          default = null;
          description = ''
            The user agent to use for authorization with the GitHub API.
            This must match the app name if you authenticate using a GitHub App token.
          '';
        };

        ghAppKeyFile = mkOption {
          type = types.path;
          default = "";
          description = ''
            Path to a file containing the GitHub App private key for authorization with GitHub.
          '';
        };

        ghTokenFile = mkOption {
          type = with types; nullOr path;
          default = null;
          description = ''
            Path to a file containing the GitHub token for authorization with GitHub.
          '';
        };

        ghSecretFile = mkOption {
          type = types.nullOr types.path;
          default = null;
          description = ''
            The agreed upon secret with GitHub for the Webhook payloads.
          '';
        };

        hydraHost = mkOption {
          type = types.str;
          example = "http://hydra.example.com:8080";
          default = "localhost";
          description = ''
            The host or URL of hydra.
          '';
        };

        hydraUser = mkOption {
          type = types.str;
          default = "";
          description = ''
            The user to authenticate as with hydra.
          '';
        };

        hydraPassFile = mkOption {
          type = types.nullOr types.path;
          default = null;
          description = ''
            A file containing the password to authenticate with against hydra.
          '';
        };

        hydraDb = mkOption {
          type = types.str;
          default = "";
          description = ''
            Hydra DB host string. Empty means unix socket.
          '';
        };

        port = mkOption {
          type = types.port;
          default = 8811;
          description = ''
            The port to listen on for webhooks.
          '';
        };

        environmentFile = mkOption {
          type = types.nullOr types.path;
          default = null;
          description = ''
            plaintext environment file, containing and `HYDRA_DB_USER`, and `HYDRA_DB_PASS`.
          '';
        };

        waitForHydraServerPort =
          mkEnableOption ''
            delay of hydra-server started state until its port is ready.
            Note this adds an ExecStartPost= to hydra-server.
            It does not make the hydra-github-bridge wait as the name might suggest.
          ''
          // {
            default = true;
          };
      };

      config = lib.mkIf cfg.enable {
        systemd.services = {
          hydra-github-bridge = {
            wantedBy = ["hydra-server.service"];
            after = ["postgresql.service" "hydra-server.service"];

            startLimitIntervalSec = 0;

            serviceConfig =
              {
                User = config.users.users.hydra.name;
                Group = config.users.groups.hydra.name;

                Restart = "always";
                RestartSec = "10s";

                LoadCredential =
                  lib.optional (cfg.ghTokenFile != null) "github-token:${cfg.ghTokenFile}"
                  ++ lib.optional (cfg.ghAppKeyFile != null) "github-app-key-file:${cfg.ghAppKeyFile}"
                  ++ lib.optional (cfg.hydraPassFile != null) "hydra-pass:${cfg.hydraPassFile}"
                  ++ lib.optional (cfg.ghSecretFile != null) "github-secret:${cfg.ghSecretFile}";

                StateDirectory = "hydra";
              }
              // lib.optionalAttrs (cfg.environmentFile != null)
              {EnvironmentFile = builtins.toPath cfg.environmentFile;};

            environment =
              {
                HYDRA_HOST = cfg.hydraHost;
                HYDRA_DB = cfg.hydraDb;
                PORT = toString cfg.port;
              }
              // lib.optionalAttrs (cfg.ghUserAgent != "") {
                GITHUB_USER_AGENT = cfg.ghUserAgent;
              }
              // lib.optionalAttrs (cfg.ghAppId != 0) {
                GITHUB_APP_ID = toString cfg.ghAppId;
              }
              // lib.optionalAttrs (cfg.ghAppInstallIds != {}) {
                GITHUB_APP_INSTALL_IDS = let
                  mkPairStr = org: installId: "${org}=${builtins.toString installId}";
                in
                  lib.pipe cfg.ghAppInstallIds [
                    (lib.mapAttrsToList mkPairStr)
                    (lib.concatStringsSep ",")
                  ];
              }
              // lib.optionalAttrs (cfg.hydraUser != "") {
                HYDRA_USER = cfg.hydraUser;
              };

            script = ''
              ${lib.optionalString (cfg.ghTokenFile != null) ''export GITHUB_WEBHOOK_SECRET=$(< "$CREDENTIALS_DIRECTORY"/github-token)''}
              ${lib.optionalString (cfg.ghAppKeyFile != null) ''export GITHUB_APP_KEY_FILE="$CREDENTIALS_DIRECTORY"/github-app-key-file''}
              ${lib.optionalString (cfg.hydraPassFile != null) ''export HYDRA_PASS=$(< "$CREDENTIALS_DIRECTORY"/hydra-pass)''}
              ${lib.optionalString (cfg.ghSecretFile != null) ''export KEY=$(< "$CREDENTIALS_DIRECTORY"/github-secret)''}

              export HYDRA_STATE_DIR="$STATE_DIRECTORY"

              exec ${lib.getExe cfg.package}
            '';
          };
        }
        // lib.optionalAttrs cfg.waitForHydraServerPort {
          # Delay systemd's dependencies until Hydra actually listens.
          # This is needed for After= ordering of the github-hydra-bridge
          # because that tries to log in to use Hydra's API when it starts.
          hydra-server.postStart = let
            script = pkgs.writeShellApplication {
              name = "hydra-wait-for-port";
              runtimeInputs = [pkgs.netcat];
              text = ''
                while ! nc -z localhost ${toString config.services.hydra.port} 2> /dev/null; do
                  sleep 1
                done
              '';
            };
          in ''
            timeout 30 ${lib.getExe script}
          '';
        };
      };
    });

    hydra-attic-bridge = moduleWithSystem (perSystem @ {config}: {
      config,
      lib,
      pkgs,
      ...
    }: let
      cfg = config.services.hydra-attic-bridge;
    in {
      options.services.hydra-attic-bridge = with lib; {
        enable = mkEnableOption "github attic bridge";
        package = mkOption {
          type = types.package;
          default = perSystem.config.packages.hydra-attic-bridge;
          defaultText = "hydra-attic-bridge";
          description = "The hydra to attic bridge";
        };
        host = mkOption {
          type = types.str;
          default = "";
          description = ''
            Hydra DB host string. Empty means unix socket.
          '';
        };
        attic = mkOption {
          type = types.str;
          default = "localhost:8080";
          description = ''
            The attic URL to use for the bridge.
          '';
        };
        cache = mkOption {
          type = types.str;
          description = ''
            The attic cache name.
          '';
        };
        workers = mkOption {
          type = types.int;
          default = 1;
          description = ''
            Number of concurrent worker threads handling attic uploads.
          '';
        };
        environmentFile = mkOption {
          type = types.nullOr types.path;
          default = null;
          description = ''
            plaintext environment file, containing and `HYDRA_USER`, `HYDRA_PASS`, and `ATTIC_TOKEN`.
          '';
        };
      };
      config = lib.mkIf cfg.enable {
        systemd.services.hydra-attic-bridge = {
          wantedBy = ["multi-user.target"];
          after = ["postgresql.service"];
          partOf = ["hydra-server.service"]; # implies after (systemd/systemd#13847)

          startLimitIntervalSec = 0;

          serviceConfig =
            {
              ExecStart = "@${cfg.package}/bin/hydra-attic-bridge hydra-attic-bridge";

              User = config.users.users.hydra.name;
              Group = config.users.groups.hydra.name;

              Restart = "always";
              RestartSec = "10s";
            }
            // lib.optionalAttrs (cfg.environmentFile != null)
            {EnvironmentFile = builtins.toPath cfg.environmentFile;};
          environment = {
            ATTIC_HOST = cfg.attic;
            ATTIC_CACHE = cfg.cache;
            HYDRA_HOST = cfg.host;
            ATTIC_WORKERS = toString cfg.workers;
          };
        };
      };
    });
  };
}
