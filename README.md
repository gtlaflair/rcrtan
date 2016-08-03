# rcrtan

[![Build Status](https://travis-ci.org/gtlaflair/rcrtan.svg?branch=master)](https://travis-ci.org/gtlaflair/rcrtan)

rcrtan provides functions for criterion-referenced test analyses as described in Brown & Hudson (2002). Currently it supports the following item and test analyses:

* Item discrimination
     + Item facility (including IF for passing test takers and failing test takers)
     + B-index
     + Agreement index
     + Item Phi
  
* Test dependability
     + Subkoviak's (1988) single administration $\kappa$
     + Subkoviak's (1988) single administration $\rho$

# Installation

Get the development version from github:

```R
# install.packages('devtools')
devtools::install_github('gtlaflair/rcrtan)
```