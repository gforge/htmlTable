## Test environments
* local Windows install, R 3.2.3
* Docker rocker/r-devel for develop environment

## R CMD check results
There were no ERRORs, WARNINGs, or NOTEs.

## Downstream dependencies
There were no downstream dependencies

## Minor changes since last submission trial
I've updated according to Dr. Ligges suggestions with
single-quotes around software names. He also suggested
to quote CSS but I don't believe this qualifies as
a software name but a technique. I've changed to lower-case
in order to better signal this.

I've also rebuilt the vignettes using knitr instead of rmarkdown.
There should now be a vignette index.