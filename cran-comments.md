## Resubmission

This is a third resubmission. In this version, I have:

* Added the ISBN number for Brown & Hudson (2002) to the Description field in the DESCRIPTION file

## Resubmission

This is a second resubmission. In this version, I have:

* Omitted the redundant "R Functions for" in the Title field of the DESCRIPTION
* Omitted the redudnant "This package provides R functions for" in the Description field of the DESCRIPTION
* Added a CITATION file that includes a formatted reference for Brown & Hudson (2002), it's ISBN, and a URL to the book



## Resubmission

This is a resubmission. In this version, I have:

* Removed the .bib file that was causing a NOTE
* Added cran-comments.md to .Rbuildignore, which was also causing a NOTE


## Test environments


## R CMD check results
There were no ERRORs or WARNINGs.

There were two NOTEs

> Non-standard files/directories found at top level:
  ‘bibs.bib’ ‘cran-comments.md’
  
> Maintainer: ‘Geoff LaFlair <gtlaflair@gmail.com>’

## Downstream dependencies
I have also run R CMD check on downstream dependencies of rcrtan. No ERRORs or WARNINGs were found.