# What is cblrepo?

The goal of `cblrepo` is to aid in maintaining a consistent set of Haskell packages, e.g. for a Linux distribution.  Currently it's heavily influenced by the work required to maintain Haskell packages for [Arch Linux](http://www.archlinux.org/), but it's proven useful also for other distributions.

# Building it

It uses CABAL, so as soon as its dependencies are satisfied it builds and installs with the familiar 3 steps:

    $ ./Setup.hs configure
    $ ./Setup.hs build
    $ ./Setup.hs install

Alternatively one can use the `cabal` tool:

   $ cabal install

# Using it

The following sections cover some of the more used commands, but it's not an exhaustive description of the tool.  One can list all commands can be accessed using

    $ cblrepo --help

and the help for an individual command is accessed using

    $ cblrepo <command> --help

## Syncing with Hackage

The program can maintain a cache of all the packages on Hackage, this cache is used in some of the commands.  The cache is updated using

    $ cblrepo sync

By default the cache is stored in `~/.cblrepo/`, the location can be controlled using `--appdir=`.

## Adding packages

All added packages are kept in a database, named `cblrepo.db`, which should be in the current directory (unless the `--db=` argument is used).  The database will be created on first add, if it doesn't exist already.

To `cblrepo` there are three types of packages:

*GHC package* -- A package provided by GHC, e.g. `base`.  Added using the `-g` (or `--ghc-pkg=`) flag.  Multiple package can be added at the same time by using the flag multiple times.  For each package only the package name and version is recorded.  Ex:

    $ cblrepo add -g base,4.3.1.0 -g array,0.3.0.2

*Distro package* -- A package provided by the distribution.  Added using the `-d` (or `--distro-pkg=`) flag.  Multiple package can be added at the same time using the flag multiple times.  For each package package name, version and release is recorded.  Ex:

    $ cblrepo add -d xmonad-contrib,0.10,1 -d zlib,0.5.3.1,2.1

*Repo package* -- A package maintained using `cblrepo`.  Added using either `-u`/`--cbl-url` (adding by URL), `-f`/`--cbl-file` (adding by file name), or by just giving the package name and version on the command line (the details are then extracted from the cache).  Ex:

    $ cblrepo add -u http://hackage.haskell.org/packages/archive/dataenc/0.14.0.3/dataenc.cabal
    $ cblrepo add -f my-cbl-files/dataenc.cabal
    $ cblrepo add dataenc,0.14.0.3

The release number after adding is set to 1.

If there are any unsatisfiable dependencies they will be reported by `cblrepo` and no changes will be made to the database.

## Updating packages

The *add* command is used to update packages as well.

## Generating PKGBUILDs for packages

Use the *pkgbuild* command to generate the source Arch Linux package.

    $ cblrepo pkgbuild yesod

## Adding patches for packages

In some, hopefully rare, cases the packages found on Hackage require patching in order to work properly.  There are three types of patches used by `cblrepo` at the moment:

*Cabal patches* -- A patch `<patch dir>/<pkg name>.cabal` is applied to the CABAL file before it's use.  It's also included in the Arch Linux source package created with the `pkgbuild` command.

*Pkgbuild patches* -- A patch `<patch dir>/<pkg name>.pkgbuild` is applied to the generated PKGBUILD when executing the `pkgbuild` command.

*Source patches* -- A patch `<patch dir>/<pkg name>.source` is included in the Arch Linux source package created with the `pkgbuild` command.

The `<pkg name>` value is the exact name of the package as it appears in Hackage; e.g., `http://hackage.haskell.org/package/bindings-GLFW-3.0.3.2/bindings-GLFW.cabal` would have `bindings-GLFW` as the package name.
The default location for patches is the dir `./patches`, but `cblrepo` can be told to look elsewhere by using the `--patchdir=` flag.

### Details of patches

`cblrepo` uses the external tool `patch` to apply patches.  There are a few technical details worth knowing to make sure that `cblrepo`, and the files it generates, can work with your patches.

Patches for CABAL files and PKGBUILD files are applied using the pattern

    patch <original file> <patch file>

which means that the path depth in the patch is of no importance at all.  It is however important that these patches contain diffs for a single file only.

Patches for the source is only ever used in generated PKGBUILD files, and then it's applied using the pattern

    patch -p4 < <patch file>

from within the package source (i.e. `${srcdir}/<pkg dir>`).  The reason for this particular patch depth should be obvious after reading the following section.

### Example of working with patches

Knowledge of the tool [`quilt`](http://savannah.nongnu.org/projects/quilt) is extremely useful when working with patches.  In the following example we add the package DBus (version 0.4) and as you'll see it requires all three kind of patches.

*patches/DBus.cabal*

Download the Cabal file for DBus:

    $ wget $(cblrepo urls DBus,0.4)

then create a new patch and add the Cabal file:

    $ quilt new DBus.cabal
    $ quilt add DBus.cabal

Now go ahead and make the changes to the file and then record them in the patch:

    $ quilt refresh

Once the changes have been recorded it's safe to remove the Cabal file, and if you want you can also remove all the `quilt` files:

    $ rm DBus.cabal
    $ rm -fr .pc patches/series

It's now possible to add the package:

    $ cblrepo add DBus,0.4

*patches/DBus.pkgbuild*

DBus also requires some changes to the generated PKGBUILD, so generate the source package and then use `quilt` to record the necessary changes to it:

    $ cblrepo pkgbuild DBus
    $ quilt new DBus.pkgbuild
    $ quilt edit haskell-dbus/PKGBUILD
    <make edits>
    $ quilt refresh

(Here we use the `quilt` command `edit` to add the file and open an editor in a single command.)  To verify that the patch we remove some files and re-generate the source package:

    $ rm -fr haskell-dbus .pc patches/series
    $ cblrepo pkgbuild DBus

If we now inspect the generated PKGBUILD file it should contain the desired changes.

*patches/DBus.source*

Finally DBus requires some minor changes to its source.  We start with moving into the directory containing the source package, download and extract all files, and create the source patch:

    $ cd haskell-dbus
    $ makepkg -o
    $ quilt new DBus.source

Now we can move into the extracted source and `quilt edit` files to our hearts' content.  Finally record the changes and clean up:

    $ quilt refresh
    $ cd <top-dir>
    $ rm -fr haskell-dbus .pc patches/series

When we now re-generate the source package all our patches will be used:

    $ cblrepo pkgbuild DBus
    $ tree haskell
    haskell-dbus/
    ├── cabal.patch
    ├── haskell-dbus.install
    ├── PKGBUILD
    ├── PKGBUILD.orig
    └── source.patch

### Modifying patches

The command `quilt import` makes it easy to work with existing patches.  Also remember that the `--patchdir=` flag for `cblrepo` can be used to *prevent* use of patches by e.g. pointing it to `/tmp`.

# Contact

Please report bugs and suggestions for improvements at [github](https://github.com/magthe/cblrepo).
