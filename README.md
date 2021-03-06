# Package notes

Some things I tend to forget when creating a fresh package with RcppArmadillo, header files and Travis CI and codecov enabled. 

## Installation

To create a package called `my_pkg`:
```
devtools::install_github("ankargren/package_notes")
skeleton::arma_package_template(name = "my_pkg", 
                                path = "~/Desktop")
```
Next, go to `Tools -> Project options -> Build tools` and add `--clean` to the `Build and Reload` field. (This enables a run of the `cleanup` file after the package has been installed, which removes unnecessary auxiliary files.)

## Documentation

The files that the template package uses can be found in the `inst` folder here on GitHub. They are also documented below (with slight modifications).

### .Rbuildignore

```
^.*\.Rproj$
^\.Rproj\.user$
^README.rmd$
^README_cache$
^codecov.yml$
^README.md$
^cran-comments.md$
^man-roxygen$
^\.travis.yml$
^config.yml$
^docs$
^README_files$
```

### configure file

Change `PKG_NAME` and `PKG_VERSION` in the following `configure.ac`:
```
## Rbuildignore

## configure.ac

## -*- mode: autoconf; autoconf-indentation: 4; -*-
##
##  Copyright (C) 2016 - 2017  Dirk Eddelbuettel for
##  the RcppArmadillo package. Licensed under GPL-2 or later
##  This file is a subset of the configure.ac used by
##  RcppArmadillo.

## require at least autoconf 2.61
AC_PREREQ(2.61)

## Process this file with autoconf to produce a configure script.
AC_INIT([PKG_NAME], PKG_VERSION)

## Set R_HOME, respecting an environment variable if one is set
: ${R_HOME=$(R RHOME)}
if test -z "${R_HOME}"; then
	AC_MSG_ERROR([Could not determine R_HOME.])
fi
## Use R to set CXX and CXXFLAGS
CXX=$(${R_HOME}/bin/R CMD config CXX)
CXXFLAGS=$("${R_HOME}/bin/R" CMD config CXXFLAGS)

## We are using C++
AC_LANG(C++)
AC_REQUIRE_CPP

## Default the OpenMP flag to the empty string.
## If and only if OpenMP is found, expand to $(SHLIB_OPENMP_CXXFLAGS)
openmp_flag=""
openmp_cflag=""

## Check for broken systems produced by a corporation based in Cupertino
AC_MSG_CHECKING([for macOS])
RSysinfoName=$("${R_HOME}/bin/Rscript" --vanilla -e 'cat(Sys.info()[["sysname"]])')
if test x"${RSysinfoName}" == x"Darwin"; then
   AC_MSG_RESULT([found])
   AC_MSG_WARN([OpenMP unavailable and turned off.])
   openmp_flag="-DARMA_DONT_USE_OPENMP"
else
   AC_MSG_RESULT([not found as on ${RSysinfoName}])
   ## Check for OpenMP
   AC_MSG_CHECKING([for OpenMP])
   ## if R has -fopenmp we should be good
   allldflags=$(${R_HOME}/bin/R CMD config --ldflags)
   hasOpenMP=$(echo ${allldflags} | grep -- -fopenmp)
   if test x"${hasOpenMP}" == x""; then
	  AC_MSG_RESULT([missing])
	  openmp_flag="-DARMA_DONT_USE_OPENMP"
   else
	  AC_MSG_RESULT([found])
	  openmp_flag='$(SHLIB_OPENMP_CXXFLAGS)'
	  openmp_cflag='$(SHLIB_OPENMP_CFLAGS)'
   fi
fi

AC_SUBST([OPENMP_CFLAG], ["${openmp_cflag}"])
AC_SUBST([OPENMP_FLAG], ["${openmp_flag}"])
AC_CONFIG_FILES([src/Makevars])
AC_OUTPUT
```

Then run `autoconf` in the folder where `configure.ac` is saved. Copy the `configure` file to the package root. If it doesn't show up as an executable, run `chmod +x ~/path/to/file`.

### Makevars.in

The `Makevars.in` file that goes with the `configure` script is:
```
CXX_STD = CXX11
PKG_CXXFLAGS = @OPENMP_FLAG@ -I../inst/include
PKG_LIBS = @OPENMP_FLAG@ $(LAPACK_LIBS) $(BLAS_LIBS) $(FLIBS)
```
(Note: Having `@OPENMP_CFLAG@` in `PKG_LIBS` is frowned upon by CRAN, C and C++ flags should not be mixed.)

### Makevars

```
CXX_STD = CXX11
PKG_CXXFLAGS = $(SHLIB_OPENMP_CXXFLAGS) -I../inst/include
PKG_LIBS = $(SHLIB_OPENMP_CXXFLAGS) $(LAPACK_LIBS) $(BLAS_LIBS) $(FLIBS)
```

### cleanup file

For most cases, the following is enough:
```
#!/bin/sh

rm -f config.* src/Makevars
```

### Travis CI

A basic `.travis.yml` file:
```
# R for travis: see documentation at https://docs.travis-ci.com/user/languages/r

language: R
sudo: false
cache: packages
os:
  - linux
  - osx
r:
  - oldrel
  - release
  - devel

r_check_args: --as-cran

matrix:
  exclude:
    - os: osx
      r: devel

branches:
  only:
  - master

r_github_packages:
  - jimhester/covr
r_binary_packages:
  - Rcpp
  - RcppArmadillo
  - testthat
  - roxygen2
  - devtools

after_success:
  - tar -C .. -xf $PKG_TARBALL
- Rscript -e 'covr::codecov()'
```

### codecov

Go to the codecov site, turn the repo on, get the token. Add the `codecov.yml` file:

```
comment: false

coverage:
  status:
    project:
      default:
        target: auto
        threshold: 1%
    patch:
      default:
        target: auto
        threshold: 1%

language: R
sudo: false
cache: packages
after_success:
- Rscript -e 'covr::codecov()'
```

Go to R and run:
```
install.packages("covr")
library(covr)
codecov(token = "YOUR_TOKEN_GOES_HERE")
```
