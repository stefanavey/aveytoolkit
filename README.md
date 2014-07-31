# README #

### What is this repository for? ###

* This repository holds the aveytoolkit package I've created for working with the data

## How do I get set up? ##

### Dependencies ###

The following R packages are required for proper installation

     install.packages(c('devtools', 'ggplot2', 'gplots'))

### Installation ###

     library(devtools) # for install_bitbucket function

The secure way to install - without putting your password into plain text is described first

     source(url("https://gist.githubusercontent.com/mages/2aed2a053e355e3bfe7c/raw/getLoginDetails.R"))
     ## A box should come up, just click ok without entering anything 
     credentials <- getLoginDetails() 
     ## now enter username and password in the prompt
     install_bitbucket(repo = "aveytoolkit-r-package", username = "spa23", 
                       auth_user=credentials["loginID"], password=credentials["password"], ref="default")

Alternatively, the less secure way is to put in the username and password directly

     install_bitbucket(repo = "aveytoolkit-r-package", username = "spa23", 
                       auth_user="<user_name>", password="<password>", ref="default")

### Who do I talk to? ###

* Stefan Avey <stefan.avey@yale.edu>
