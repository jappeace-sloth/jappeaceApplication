{ sources ? import ./npins
, pkgs ? import sources.nixpkgs {}
}:
let
  hpkgs = pkgs.haskellPackages.override {
    overrides = hnew: hold: {
      shake-blog = hnew.callCabal2nix "shake-blog" ./shake { };
    };
  };
  ignore = import ./nix/gitignoreSource.nix { inherit (pkgs) lib; };
in
pkgs.stdenv.mkDerivation {
  name = "shake-blog-site";
  src = ignore.gitignoreSource ./.;
  nativeBuildInputs = [ hpkgs.shake-blog pkgs.glibcLocales ];
  LANG = "en_US.UTF-8";
  LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
  buildPhase = ''
    shake-blog build
  '';
  installPhase = ''
    cp -r _site $out
  '';
}
