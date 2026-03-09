{ sources ? import ./npins
, pkgs ? import sources.nixpkgs {}
}:
let
  hpkgs = pkgs.haskellPackages.override {
    overrides = hnew: hold: {
      shake-blog = hnew.callCabal2nix "shake-blog" ./shake { };
    };
  };
in
hpkgs.shake-blog
