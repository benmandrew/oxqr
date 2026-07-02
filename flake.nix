{
  description = "OxQR - zero-alloc QR code generator in OxCaml";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs { inherit system; };

        # Pinned oxcaml/opam-repository commit, matching CI. Must be >=
        # e36bb6bb (2026-04-13), which added cut-fix.patch to the
        # oxcaml-compiler package fixing a `cut -d'=' -f2` truncation bug
        # that breaks the build on compilers needing an explicit -std flag
        # (e.g. Apple clang on newer Xcode CLT).
        oxcamlRepoUrl = "git+https://github.com/oxcaml/opam-repository.git#d57b5d40e6334c82845a9aee5008ed39c53343b8";

        pythonEnv = pkgs.python3.withPackages (ps: [ ps.matplotlib ]);
      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs =
            with pkgs;
            [
              opam
              git
              gnumake
              autoconf
              automake
              m4
              pkg-config
              unzip
              curl
              rsync
              which
              patch
              pythonEnv
              libffi
            ]
            ++ pkgs.lib.optionals pkgs.stdenv.isLinux [ bubblewrap ]
            ++ pkgs.lib.optionals pkgs.stdenv.isDarwin [ libiconv ];

          shellHook = ''
            export OPAMYES=1
            # Default 60s solver timeout is too short for this dependency
            # set (OxCaml compiler + oxqr's full opam universe), and CI
            # runners are slow enough that even 300s isn't reliably enough.
            # A nonzero tolerance lets the solver stop at a near-optimal
            # solution instead of proving global optimality, which is what
            # actually makes this fast (per opam's own suggestion).
            export OPAMSOLVERTIMEOUT=900
            export OPAMSOLVERTOLERANCE=0.05

            if [ ! -d "$HOME/.opam" ]; then
              opam init --no-setup --disable-sandboxing --bare
            fi

            if opam repository list --all --short 2>/dev/null | grep -qx "ox"; then
              opam repository set-url ox "${oxcamlRepoUrl}"
            else
              opam repository add ox "${oxcamlRepoUrl}"
            fi

            if [ ! -d "_opam" ]; then
              opam switch create . 5.2.0+ox --repos ox,default
            fi

            eval $(opam env)
            opam install . -y --deps-only --with-test
          '';
        };
      }
    );
}
