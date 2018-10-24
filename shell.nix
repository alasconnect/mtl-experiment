{ compiler ? "ghc843" }:

(import ./. { inherit compiler; }).env
