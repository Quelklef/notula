# purs-nix package information

{ purs-nix, pkgs }: let

mation-pkg =
  purs-nix.build {
    name = "mation";
    info = /package.nix;
    src.path =
      pkgs.fetchFromGitHub
        { owner = "quelklef";
          repo = "mation";
          rev = "425bba4ac9939aee2998518063e90a8a6e23271d";
          sha256 = "1s5kqf2rs8ll8bql65hvddk2fgrfpamb8kczcd22mrxkizmz9rln";
        };
  };

in {

  src = "mation";

  dependencies =
    with purs-nix.ps-pkgs;
    [
      mation-pkg

      prelude
      console
      effect
      control
      maybe
      either
      tuples
      strings
      foreign
      foldable-traversable
      profunctor-lenses
      unlift
      nullable
      string-parsers
    ];

}
