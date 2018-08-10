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