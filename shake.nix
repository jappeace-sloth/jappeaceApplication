{ pkgs ? import ./nix/new-pin.nix {}
}:
pkgs.writeText "hello-world" "hello world"
