let
  log-classifier = import ./default.nix;
in
log-classifier.overrideAttrs (oldAttrs: {
  shellHook = ''
    stack --nix build
  '';
  LANG = "en_US.UTF-8";
  doCheck = false;
})
