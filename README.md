# README #

### What is this repository for? ###

* This repository holds the aveytoolkit package I've created for working with the data

## How do I get set up? ##

### Dependencies ###

The following R packages are required for proper installation: `devtools` `ggplot2` `gplots`

Install these from the R prompt by running
```
#!R
install.packages(c("devtools", "ggplot2", "gplots"))
```

### Installation ###

#### Secure Installation ###
The secure way to install - without putting your password into plain text is described first

```
#!R
library(devtools) # for install_bitbucket function
getLoginDetails <- function() {
## Authors Markus Gesmann, Barry Rowlingson
## http://www.r-bloggers.com/simple-user-interface-in-r-to-get-login-details/
## Based on code by Barry Rowlingson
## http://r.789695.n4.nabble.com/tkentry-that-exits-after-RETURN-tt854721.html#none
require(tcltk)
tt<-tktoplevel()
tkwm.title(tt,"Get login details")
Name <- tclVar("Login ID")
Password <- tclVar("Password")
entry.Name <-tkentry(tt,width="20",textvariable=Name)
entry.Password <-tkentry(tt,width="20", show="*",textvariable=Password)
tkgrid(tklabel(tt,text="Please enter your login details."))
tkgrid(entry.Name)
tkgrid(entry.Password)
OnOK <- function() { tkdestroy(tt) }
OK.but <-tkbutton(tt,text=" OK ",command=OnOK)
tkbind(entry.Password, "<Return>",OnOK)
tkgrid(OK.but)
tkfocus(tt)
tkwait.window(tt)
invisible(c(loginID=tclvalue(Name), password=tclvalue(Password)))
}
credentials <- getLoginDetails()
## enter your Bitbucket username and password in the window
rm(getLoginDetails)
install_bitbucket(repo = "aveytoolkit-r-package", username = "spa23",
                       auth_user=credentials["loginID"], password=credentials["password"], ref="default")
rm(credentials)   # Remove loginID and password
```

#### Less-secure Installation ####
Alternatively, the less secure way is to put in the username and password directly

```
#!R
library(devtools) # for install_bitbucket function
install_bitbucket(repo = "spa23/aveytoolkit-r-package", 
                  auth_user="<user_name>", password="<password>", ref="default")
```

### Who do I talk to? ###

* Stefan Avey <stefan.avey@yale.edu>