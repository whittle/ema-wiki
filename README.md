# Ema Wiki

This is a simple wiki-style static site generator using [Ema](https://ema.srid.ca/). It pulls in an external directory of [Pandoc-flavored Markdown](https://pandoc.org/MANUAL.html#pandocs-markdown) files for the contents of the wiki.

## Getting Started

To develop with full IDE support in Visual Studio Code, follow these steps:

- [Install Nix](https://nixos.org/download.html) & [enable Flakes](https://nixos.wiki/wiki/Flakes)
- Run `nix-shell --run haskell-language-server` to sanity check your environment 
- Open the repository [as single-folder workspace](https://code.visualstudio.com/docs/editor/workspaces#_singlefolder-workspaces) in Visual Studio Code
    - Install the recommended extensions
    - <kbd>Ctrl+Shift+P</kbd> to run command "Nix-Env: Select Environment" and select `shell.nix`. The extension will ask you to reload VSCode at the end.
- Press <kbd>Ctrl+Shift+B</kbd> in VSCode, or run `bin/run` (`bin/run-via-tmux` if you have tmux installed) in terminal, to launch the Ema dev server, and navigate to http://localhost:9001/

All but the final step need to be done only once. Check [the Ema tutorial](https://ema.srid.ca/start/tutorial) next.

## Note

- This project uses [relude](https://github.com/kowainik/relude) as its prelude, as well as [Tailwind+Blaze](https://ema.srid.ca/guide/helpers/tailwind) as CSS utility and HTML DSL. Even though the author highly recommends them, you are of course free to swap them out for the library of your choice.
- Configuration:
  - To change the port, see file bin/run
  - To change the CLI arguments used by bin/run, see file .ghcid
  - To update Ema to latest Git revision, run `nix flake lock --update-input ema`
  - To add/remove Haskell dependencies, see the .cabal file. If a dependency is unavailable in nixpkgs, you can override it (to point to say a Git repo) in the `overrides` attribute of flake.nix. You can imitate the manner in which the `ema` (or `lvar`) package itself is overriden.
- To generate static site, run: `nix build; mkdir ./output && (cd ./content  && ../result/bin/ema-wiki gen $(pwd)/output)`
  - You might want to change or remove the `<base>` tag in `Main.hs` depending where you will be deploying the site.

## Search

Search for this example is provided by [Algolia](https://www.algolia.com/)’s
[DocSearch](https://docsearch.algolia.com/) product,
