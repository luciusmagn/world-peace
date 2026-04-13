{
  description = "World Peace language compiler and REPL";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs =
    { self, nixpkgs }:
    let
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];

      forAllSystems =
        function:
        nixpkgs.lib.genAttrs systems (
          system:
          function system nixpkgs.legacyPackages.${system}
        );
    in
    {
      packages = forAllSystems (
        system: pkgs:
        rec {
          default = world-peace;

          world-peace-neovim = pkgs.vimUtils.buildVimPlugin {
            pname = "world-peace-neovim";
            version = "0.1.0";
            src = ./editors/neovim;

            meta = {
              description = "Neovim runtime files for World Peace";
              homepage = "https://github.com/luciusmagn/world-peace";
              license = pkgs.lib.licenses.mit;
              platforms = pkgs.lib.platforms.unix;
            };
          };

          world-peace = pkgs.stdenvNoCC.mkDerivation {
            pname = "world-peace";
            version = "0.1.0";

            src = self;

            nativeBuildInputs = [
              pkgs.makeWrapper
              pkgs.sbcl
            ];

            buildPhase = ''
              runHook preBuild

              export HOME="$TMPDIR"
              sbcl --non-interactive \
                --eval '(require :asdf)' \
                --eval '(asdf:load-asd (merge-pathnames "world-peace.asd" (uiop:getcwd)))' \
                --eval '(asdf:make :world-peace)'

              runHook postBuild
            '';

            doCheck = true;

            checkPhase = ''
              runHook preCheck

              export HOME="$TMPDIR"
              ./check-lisp
              ./peace run examples/tour/01-hello.wp

              runHook postCheck
            '';

            installPhase = ''
              runHook preInstall

              install -Dm755 peace "$out/bin/peace"
              wrapProgram "$out/bin/peace" \
                --prefix PATH : "${pkgs.lib.makeBinPath [ pkgs.curl ]}"

              install -Dm644 README.org "$out/share/doc/world-peace/README.org"
              install -Dm644 world-peace.org "$out/share/doc/world-peace/world-peace.org"
              mkdir -p "$out/share/world-peace/examples"
              cp -R examples/* "$out/share/world-peace/examples/"
              mkdir -p "$out/share/world-peace/editors"
              cp -R editors/* "$out/share/world-peace/editors/"

              runHook postInstall
            '';

            meta = {
              description = "Tiny no-types language implemented on SBCL";
              homepage = "https://github.com/luciusmagn/world-peace";
              license = pkgs.lib.licenses.mit;
              mainProgram = "peace";
              platforms = pkgs.lib.platforms.unix;
            };
          };
        }
      );

      apps = forAllSystems (
        system: pkgs:
        rec {
          default = world-peace;

          world-peace = {
            type = "app";
            program = "${self.packages.${system}.world-peace}/bin/peace";
          };
        }
      );

      checks = forAllSystems (
        system: pkgs:
        {
          default = self.packages.${system}.world-peace;
        }
      );

      devShells = forAllSystems (
        system: pkgs:
        {
          default = pkgs.mkShell {
            packages = [
              pkgs.curl
              pkgs.sbcl
            ];
          };
        }
      );
    };
}
