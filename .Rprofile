# Hardcode a https mirror.
.First <- function() {
  options(
    download.file.method = "libcurl",
    repos = c(CRAN = "https://cran.rstudio.com/"),
    max.print = 1000,
    browserNLdisabled = TRUE
  )
}

if (interactive()) {
  suppressMessages(require(devtools))
}


# Stick these in their own environment so that rm(list=ls()) doesn't
# kill them
attach(list(
    table0 = function(...) { table(list(...), useNA='ifany') } ,
    sum0   = function(...) { sum(..., na.rm=TRUE) } ,
    mean0  = function(...) { mean(..., na.rm=TRUE) } ,
    peek   = function(x, n=3) {
      if (is.null(dim(x))) {
        n <- min(n, length(x)); return(x[1:n])
      };
      n <- min(n, dim(x)); x[1:n, 1:n]
    },
    round0 = function(x, digits=2, ...) {
      if(!is.data.frame(x)) {
        return(round(x, digits=digits, ...))
      }

      isnum <- sapply(x, is.numeric)

      x[,isnum] <- round(x[,isnum], digits=digits, ...)
      return(x)
    }),
    name = 'MyFunctions'
    )
