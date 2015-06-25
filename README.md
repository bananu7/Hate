Häte
======

[![Circle CI](https://circleci.com/gh/bananu7/Hate.svg?style=svg)](https://circleci.com/gh/bananu7/Hate)

Hate is a small framework for graphical haskell games and applications. It's heavily inspired by [Love](http://love2d.org/) and aims at similar ease of use, but within the power of Haskell's type and concurrency safety.

Hate features a pure 2D rendering API, backed by raw, low-level OpenGL, and a simple way to gather input from the user (via [GLFW-b](http://hackage.haskell.org/package/GLFW-b)).

The `samples` folder features a few examples on how to get started, but you have to consider the API to be extremely unstable and it's subject to change at any time.

Feel free to submit feature requests, opinions etc. through the GitHub issues system.

###Installation

Hate is developed and tested using GHC. If you're a beginner, the easiest way to get it is [Haskell Platform](https://www.haskell.org/platform/). You probably also want Git.

To verify the installation, do this:

    $ ghc --version
    The Glorious Glasgow Haskell Compilation System, version 7.8.3

    $ cabal --version
    cabal-install version 1.18.0.5
    using version 1.18.1.3 of the Cabal library
    
If it looks similar (the numbers are bigger), then it's probably OK. To proceed, run those commands:

    git clone https://github.com/bananu7/Hate.git
    cd Hate
    cabal sandbox init
    cabal install --dependencies-only
    cabal run sample_shapes
    
To use sandboxing functionality, you need a fairly recent Cabal, but it's highly recommended, as it helps to avoid a lot of problems. You can then install Hate into that sandbox by typing `cabal install`.

The samples available at the time of writing are:

 * `sample_shapes`
 * `sample_scheduler`
 * `sample_sprite`
 * `sample_spritesheet`
 * `sample_asteroids`
