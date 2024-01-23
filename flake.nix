{

inputs = {
  nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
  purs-nix.url = "github:purs-nix/purs-nix/ps-0.15";
};

outputs = { self, ... }@inputs: let

  system = "x86_64-linux";
  pkgs = inputs.nixpkgs.legacyPackages.${system};
  purs-nix = inputs.purs-nix { inherit system; };

  easy-purescript-nix = import
    (pkgs.fetchFromGitHub {
      owner = "justinwoo";
      repo = "easy-purescript-nix";
      rev = "d5fe5f4b210a0e4bac42ae0c159596a49c5eb016";
      sha256 = "sha256-a9Hhqf/Fi0FkjRTcQr3pYDhrO9A9tdOkaeVgD23Cdrk=";
    }) { inherit pkgs; };

  my-purs-nix = purs-nix.purs (import ./package.nix { inherit purs-nix pkgs; });


  # nb. We compile our codebase with purescript-psa [1] to have better
  # compiler error/warning messages. since purs-nix does not itself
  # support using psa [2], we have to reimplement some of the compilation
  # and bundling logic ourselves
  #
  # the upshot of this is that some compilation/bundling logic is
  # effectively duplicated. this means that changing bundling/compilation
  # details, such as modifying some tool's flags, will require both
  # changing how we invoke purs-nix and changing our own bundling/compilation
  # implementation.
  #
  # [1]: https://www.npmjs.com/package/purescript-psa
  # [2]: https://github.com/purs-nix/purs-nix/issues/45

  esbuild-format = "iife";
    # ^ necessary in some cases due to js bizareness
    #   compare 'var top = 5; console.log(top);'
    #   with '(function() { var top = 5; console.log(top); })'

  main-module = "Main";

  # purs-nix command
  #
  # At the time of writing we only use this for 'purs-nix srcs', hence
  # the lack of other options
  purs-nix-command =
    my-purs-nix.command {
      srcs = [ "$PWD/src" ];
      test = "/dev/null";  # using nix 'null' value breaks?
    };

  # purs-nix command ignoring non-lib code like samples/
  purs-nix-command-lib-only =
    my-purs-nix.command {
      srcs = [ "$PWD/src" ];
      test = "/dev/null";
      output = "out/purs-cache";
      name = "purs-nix-lib-only";
      bundle = {
        esbuild.format = esbuild-format;
        module = main-module;
      };
    };

  # Purescript warnings to suppress when compiling the codebase
  censor-warnings = [

    # Suppress warning on use of '_' in types
    "WildcardInferredType"

    # Suppress warning when wildcard-importing into a shared module name, eg.
    #
    #   import Thing as X
    #   import Other (thing) as X
    #
    # We do this several times when we re-exporting entire Mation.Gen.<Name> modules
    "ImplicitQualifiedImport"

    # Suppress warning on variable shadowing
    # Variable shadowing is expected & intended when using 'prune'
    "ShadowedName"

    # Suppress warning on unused values
    # This is a temporary exception for Mation.Lenses
    "UnusedDeclaration"

  ];


  # Stuff for dev shell and derivation
  builds-args = {
    buildInputs = [

        # For compiling/bundling the project
        purs-nix-command
        easy-purescript-nix.psa
        easy-purescript-nix.purs
        pkgs.esbuild

        # For compiling the docs
        purs-nix-command-lib-only

        # For serving the result
        pkgs.python3

        # For running generate.js
        pkgs.nodejs

        # For file watching
        pkgs.entr
        pkgs.findutils  # `find`

      ];

    shellHook = ''
      root=$PWD

      function notula.compile {(
        cd "$root" &&
        mkdir -p out/app &&

        # Compile the application. Use 'psa' for sophisticated warning/error handling
        psa \
          --censor-codes=${pkgs.lib.strings.concatStringsSep "," censor-warnings} \
          --stash=out/.psa-stash \
          $(purs-nix srcs) --output out/purs-cache
      )}

      function notula.bundle {(
        cd "$root" &&
        notula.compile &&

        # Bundle the application
        echo 'import { main } from "./out/purs-cache/${main-module}/index.js"; main()' \
          | esbuild \
              --bundle \
              --format='${esbuild-format}' \
              --log-level=warning \
              --outfile=out/app/main.js \
              &&

        # Emit index.html
        cp src/index.html out/app/index.html
      )}

      function notula.devt {(
        cd "$root" &&
        mkdir -p out/app &&
        python3 -m http.server --directory out/app & trap "kill $!" EXIT
        export -f notula.compile notula.bundle
        { find . \( -name '*.purs' -o -name '*.js' -o -name '*.html' -o -name '*.md' \) \
                 -a ! -path './out/*'
        } | entr -cs "notula.bundle && echo 'You may need to reload your browser'"
      )}

    '';
  };

in {

  devShells.${system}.default =
    pkgs.mkShell builds-args;

  packages.${system}.default =
    pkgs.stdenv.mkDerivation {
      name = "notula";
      src = ./.;
      buildInputs = builds-args.buildInputs;
      installPhase = ''
        ${builds-args.shellHook}
        notula.bundle
        mkdir $out
        mv out/app/* $out
      '';
    };

};

}
