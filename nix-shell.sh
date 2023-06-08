nix-shell -p "haskellPackages.ghcWithPackages (ps: with ps; [$(cat haskell-dependencies.txt | tr '\n' ' ')])" --run fish
