{ machine = { config, pkgs, lib, ... }: 
  {

    systemd.services.log-classifier = {
      wantedBy = [ "multi-user.target" ];

      # first let's copy the important files
      preStart =
          let projectPath = ./.;
          in "ls -la ${projectPath} && cp -R ${projectPath}/tmp-secrets /tmp/ && cp -R ${projectPath}/knowledgebase /tmp/";

      script = "${(import ./release.nix).log-classifier-web}/bin/log-classifier-web";
    };

    # We presume that the AWS has a security group for these.
    networking.firewall.allowedTCPPorts = [
      80 443 # nginx
      #8100 # for testing
    ];

    services.nginx = {

      enable = true;

      virtualHosts =
       let vhostDomainName = "log-classifier.io";
       in {
        "log-classifier.${vhostDomainName}" = {
          #enableACME = true;
          #addSSL = true;
          locations = {
            "/".proxyPass = "http://127.0.0.1:8100";
          };
          # Otherwise nginx serves files with timestamps unixtime+1 from /nix/store
          extraConfig = ''
            if_modified_since off;
            add_header Last-Modified "";
            etag off;
          '';
        };
      };

      eventsConfig = ''
        worker_connections 1024;
      '';

      appendConfig = ''
        worker_processes 4;
        worker_rlimit_nofile 2048;
      '';

    };
  };
}
