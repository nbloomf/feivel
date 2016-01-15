# Feivel

This is the source repository of Feivel, a simple text-oriented templating language aimed at math teachers. Feivel is released as free software under the GNU General Public License version 3.

Feivel allows us to easily write documents with state -- author-defined variables -- with which we can perform basic operations that affect the document itself after a processing step. Think mailmerge plus spreadsheet formulas plus types plus immutable state. Feivel is intended to be used as part of a pipeline processing plain text and makes no assumptions about whatever markup it is (or isn't) layered on top of. The on-label use of Feivel is to produce math problems using "random" data whose difficulty we can precisely control. As such, it has (basic!) built in support for some mathy data types like polynomials and matrices.

As a boring example, here is a Feivel-flavored Fizz Buzz...

    [for int @i in Range(1;20) say [[cond
      case (Divides(15; @i)) [fizz-buzz]
      case (Divides(3;  @i)) [fizz]
      case (Divides(5;  @i)) [buzz]
      default [@i]
    ] ] endfor]

...which produces the following output:

    1 2 fizz 4 buzz fizz 7 8 fizz buzz 11 fizz 13 14 fizz-buzz 16 17 fizz 19 buzz

**This project is in an extremely experimental state.** It is missing many basic features, poorly documented, subject to breaking changes, unoptimized, and amateurishly written. Under no circumstances should it be used by anyone, and the author disclaims responsibility for any Unspeakable Badness that may result from its use. **That said,** the few features it does have seem to work OK and are unlikely to destroy your machine, and I (Nathan) use it frequently with no problems. So be careful and have fun.



## Installation

Currently the only way to install Feivel is by building it from source. This section includes just the commands needed to install and use Feivel; see the [Development](#development) section for more info.

* Required Requirements: GHC >7.6.3, cabal >1.20
* Optional Requirements: git, pandoc, shelltestrunner, graphmod

### Install Commands

    # Get dependencies
    cabal update
    cabal install parsec containers regex-posix random-fu
    cabal install rvar random-extras transformers mtl random
    cabal install directory monadlist primes gtk tostring

    git clone https://github.com/nbloomf/carl.git
    cd carl
    make
    cd ..

    # Compile and install feivel
    git clone https://github.com/nbloomf/feivel.git
    cd feivel
    make

    # Run tests (shelltestrunner required)
    make golden

If these commands finish with no errors, you can try out feivel's gui with

    feivel --repl &

and test feivel from the command line with something like

    echo "[:int: Rand({1;2;3}) :]" | feivel



## Documentation

There are a few resources for learning how to use Feivel.

- **Tutorial:** There is a short tutorial in doc/tutorial. It is readable as plain text (in markdown), but you can build an HTML version by saying `make tutorial` in the project root directory. You will need a recent version of [pandoc](pandoc.org).

- **Examples:** There are some example templates in doc/examples. Each one comes with a makefile; have a look at these to see how to process templates.

- **Tests:** Tests are not documentation (ahem) but... the test/gold directory has a large collection of input-output pairs for testing intended behavior. Each test demonstrates one or two features. Feivel will not get a new version number if any of the tests do not pass, so these tests "should" represent actual behavior.



## Development

Feivel is written in Haskell and licensed under the GNU GPLv3. Currently the only way to install Feivel is to build it from source.

- **To obtain the source code:** The public repo is at [GitHub](https://github.com/nbloomf/feivel).

- **To build Feivel from source:** (Assuming a unix-like operating system)
  - Say `make feivel` in the project's root directory.
  - You will need GHC version 7.6.3 or higher and cabal version 1.20 or higher, as well as several libraries, all of which are available on Hackage. (For a list of dependencies see feivel.cabal in the root directory.)
  - The easiest way to get GHC and cabal is with the [Haskell Platform](https://www.haskell.org/platform/). The extra libraries can be installed using cabal.
  - Compilation has been successfully tested on Fedora 21 and Ubuntu 15.04. If you build Feivel on another OS, or have problems doing so, I'd like to hear about it.

- **To load Feivel into GHCi:** (for typechecking, repl-ing, and debugging)
  - Load `Feivel.Debug` from the project root directory. This module has some useful functions for testing the larger moving parts of Feivel (the parser and the evaluator).
  - You may need to add src/ and test/ to the GHCi path; this can be done by keeping the hidden file .ghci (which is in the repo) in the project root.

- **To run the tests:**
  - There is a suite of "golden tests" of the syntax. To run the golden tests, say `make golden` in the project root directory. (Warning, there are a lot of them. It takes about 8 seconds on my cruddy machine.)
  - You will need a recent version of [shelltestrunner](joyful.org/shelltestrunner/).



## Authors

Feivel is written and maintained by Nathan Bloomfield (nbloomf at gmail dot com).



## Project Goals

* Make my job easier, or at least more interesting
* Be fun to tinker with
* Learn things
* Be useful
* Suck less
