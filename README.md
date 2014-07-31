# README #

### What is this repository for? ###

* This repository holds the aveytoolkit package I've created for working with the data

## How do I get set up? ##

### Dependencies ###

The following R packages are required for proper installation: `devtools` `ggplot2` `gplots`

Install these from the R prompt by running

     install.packages(c("devtools", "ggplot2", "gplots"))

### Installation ###

#### Secure Installation ###
The secure way to install - without putting your password into plain text is described first

     library(devtools) # for install_bitbucket function
     source(url("https://gist.githubusercontent.com/mages/2aed2a053e355e3bfe7c/raw/getLoginDetails.R"))
     ## A box should come up, just click ok without entering anything 
     credentials <- getLoginDetails() 
     ## now enter your Bitbucket username and password in the window
     rm(getLoginDetails)
     install_bitbucket(repo = "aveytoolkit-r-package", username = "spa23", 
                       auth_user=credentials["loginID"], password=credentials["password"], ref="default")

#### Less-secure Installation ####
Alternatively, the less secure way is to put in the username and password directly

     library(devtools) # for install_bitbucket function
     install_bitbucket(repo = "aveytoolkit-r-package", username = "spa23", 
                       auth_user="<user_name>", password="<password>", ref="default")

### Who do I talk to? ###

* Stefan Avey <stefan.avey@yale.edu>
