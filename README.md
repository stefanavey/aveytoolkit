# README #

### What is this repository for? ###

* This repository holds the aveytoolkit package I've created for working with the data

## How do I get set up? ##

### Dependencies ###

The following R packages are required for proper installation

     install.packages(c('devtools', 'ggplot2', 'gplots'))

### Installation ###

     library(devtools)
     source(url("https://bitbucket.org/spa23/aveytoolkit-r-package/raw/a273aa5668295a0c27ded078dd86dce1862c4b05/R/aveytoolkit_getLoginDetails.R"))
     credentials <- getLoginDetails
     install_bitbucket(repo = "aveytoolkit-r-package", username = "spa23", 
                       auth_user=credentials["loginID"], password=credentials["password"], 
		       ref="default")

### Who do I talk to? ###

* Stefan Avey <stefan.avey@yale.edu>
