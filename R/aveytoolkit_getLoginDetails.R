#' getLoginDetails
#'
#' Uses tcltk to display a prompt for a loginID and password
#'
#' @import tcltk
#' @return an invisible named vector of loginID and password
#' @author Markus Gesmann, Barry Rowlingson
#' @details This function displays a window for a user to enter a loginID and password without showing the password.
#' @keywords aveytoolkit
#' @seealso \code{\link{tcltk}}
#' @references http://www.r-bloggers.com/simple-user-interface-in-r-to-get-login-details/ http://r.789695.n4.nabble.com/tkentry-that-exits-after-RETURN-tt854721.html#none
#' @export
#' @examples
#' credentials <- getLoginDetails()
#' ## Do what needs to be done with loginID and password
#' rm(credentials) # Delete credentials

getLoginDetails <- function(){
  ## Based on code by Barry Rowlingson
  ## http://r.789695.n4.nabble.com/tkentry-that-exits-after-RETURN-tt854721.html#none
  ## require(tcltk)
  tt<-tktoplevel()
  tkwm.title(tt,"Get login details")
  Name <- tclVar("Login ID")
  Password <- tclVar("Password")
  entry.Name <-tkentry(tt,width="20",textvariable=Name)
  entry.Password <-tkentry(tt,width="20", show="*",textvariable=Password)
  tkgrid(tklabel(tt,text="Please enter your login details."))
  tkgrid(entry.Name)
  tkgrid(entry.Password)
  OnOK <- function() 
    {
      tkdestroy(tt)
    }
  OK.but <-tkbutton(tt,text=" OK ",command=OnOK)
  tkbind(entry.Password, "<Return>",OnOK)
  tkgrid(OK.but)
  tkfocus(tt)
  tkwait.window(tt)
  invisible(c(loginID=tclvalue(Name), password=tclvalue(Password)))
}

