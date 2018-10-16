{ machine = { pkgs, ... }: {
    nixpkgs.config = import ./release.nix;

    systemd.services.log-classifier = {
      wantedBy = [ "multi-user.target" ];
      script = ''
        ${pkgs.log-classifier-web}/bin/web
      '';

    };
  };
}
