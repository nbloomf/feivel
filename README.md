# Feivel

Feivel is a simple templating language aimed at math teachers which can also be used as an interactive calculator. It is released as free software under the GNU General Public License version 3.

The goal of Feivel is to make it possible to write math homework problems using "random" data but whose difficulty we can precisely control. To this end it has built-in support for manipulating algebraic doodads like matrices and polynomials. In this usage Feivel syntax will probably be layered on top of e.g. LaTeX, but this is not required.

Feivel is in an extremely experimental state. It is missing lots of desired features, poorly documented, subject to breaking changes, unoptimized, and amateurishly written. However the few features it does have seem to work OK and are unlikely to destroy your machine. That said - be careful.



## Installation

Currently the only way to install Feivel is by building it from source. See the [Development](#development) section for info.



## Documentation

There are a few resources for learning how to use Feivel.

- **Tutorial:** There is a short tutorial in doc/tutorial. It is readable as plain text (in markdown), but to build an HTML version, run "make tutorial" in the project root directory. You will need a recent version of [pandoc](pandoc.org).

- **Examples:** There are some example templates in doc/examples. Each one comes with a makefile; have a look at these to see how to process templates.

- **Tests:** The test/gold directory has a large collection of input-output pairs for testing intended behavior. Each test demonstrates one or two features. Feivel will not get a new version number if any of the tests do not pass, so these are guaranteed to represent actual behavior.



## Development

Feivel is written in Haskell and licensed under the GNU GPLv3. Currently the only way to install Feivel is to build it from source.

- **To obtain the source code:** The public repo is at [GitHub](https://github.com/nbloomf/feivel).

- **To build Feivel from source:** (Assuming a unix-like operating system)
  - Run "make feivel" in the project's root directory.
  - You will need GHC version 7.6.3 or higher and cabal version 1.20 or higher, as well as several libraries, all of which are available on Hackage. (For a list of dependencies see feivel.cabal in the root directory.)
  - The easiest way to get GHC and cabal is with the [Haskell Platform](https://www.haskell.org/platform/). The extra libraries can be installed using cabal.
  - Compilation has been successfully tested on Fedora 21 and Ubuntu 15.04. If you build Feivel on another OS, or have problems doing so, I'd like to hear about it.

- **To load Feivel into GHCi:** (for typechecking, repl-ing, and debugging)
  - Load Feivel.Debug from the project root directory.
  - You may need to add src/ and test/ to the GHCi path; this is done by keeping the hidden file .ghci (which is in the repo) in the project root.

- **To run the tests:**
  - There is a suite of "golden tests" of the syntax. To run the golden tests, say "make golden" in the project root directory. (Warning, there are a lot of them. It takes about 8 seconds on my cruddy machine.)
  - You will need a recent version of [shelltestrunner](joyful.org/shelltestrunner/).



## Authors

Feivel is written and maintained by Nathan Bloomfield (nbloomf at gmail dot com).
