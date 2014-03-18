# Example code from http://cran.r-project.org/web/packages/Brobdingnag/vignettes/brob.pdf

setClass("swift",
         representation = "VIRTUAL"
         )
setClass("brob",
         representation = representation(x="numeric",positive="logical"),
         prototype = list(x=numeric(),positive=logical()),
         contains = "swift"
         )

.Brob.valid <- function(object){
  len <- length(object@positive)
  if(len != length(object@x)){
    return("length mismatch")
  } else {
    return(TRUE)
  }
}

setValidity("brob", .Brob.valid)

"brob" <- function(x=double(),positive){
  if(missing(positive)){
    positive <- rep(TRUE,length(x))
  }
  if(length(positive)==1){
    positive <- rep(positive,length(x))
  }
  new("brob",x=as.numeric(x),positive=positive)
}

.Brob.print <- function(x, digits=5){
  noquote( paste(c("-","+")[1+x@positive],"exp(",signif(x@x,digits),")",sep=""))
}

print.brob <- function(x, ...){
  jj <- .Brob.print(x, ...)
  print(jj)
  return(invisible(jj))
}

setMethod("show", "brob", function(object){print.brob(object)})

