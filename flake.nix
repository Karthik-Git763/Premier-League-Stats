{
  description = "EPL Analysis R Environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs = { self, nixpkgs }:
    let
      supportedSystems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
      forAllSystems = nixpkgs.lib.genAttrs supportedSystems;
    in
    {
      devShells = forAllSystems (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
        in
        {
          default = pkgs.mkShell {
            packages = with pkgs; [
              R
              rPackages.tidyverse
              rPackages.plotly
              rPackages.corrplot
              rPackages.scales
              rPackages.RColorBrewer
              rPackages.gridExtra
            ];
          };
        }
      );
    };
}
