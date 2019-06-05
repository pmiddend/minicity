let
  src = builtins.fetchTarball {
    name = "nixos-unstable";
    url = https://github.com/nixos/nixpkgs/archive/4543559aead7172e6da78325bb1b760ddb729df2.tar.gz;
    # git ls-remote https://github.com/nixos/nixpkgs-channels nixos-unstable
    sha256 = "0fnk1af44818qwrvpdj2hyravg6qi98p0sc6dk2agmyhppy1rd0m";
  };
  # pkgs = import src {};
  pkgs = import <nixpkgs> {};
  haskellPkgs = pkgs.haskellPackages;
in haskellPkgs.callPackage ./executable.nix {}
