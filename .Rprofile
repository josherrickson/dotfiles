# Hardcode a https mirror.
.First <- function() {
  options(
    download.file.method = "libcurl",
    repos = c(CRAN = "https://cran.rstudio.com/"),
    max.print = 1000,
    dplyr.print_max = 1000#,
#    browserNLdisabled = TRUE
  )
}

if (interactive()) {
  if (require(devtools, quietly = TRUE)) {
    yesno <- function(...) {
      cat(paste0(..., collapse = ""))
      # For whatever reason, devtools:::yesno returns `TRUE` if you select a No
      # option, and `FALSE` if you select a Yes option
      utils::menu(c("Yes", "No")) != 1
    }
    utils::assignInNamespace("yesno", yesno, "devtools")
    rm(yesno)
  }
}

# Add rrelaxiv to Drat's repos, if drat is installed
if(require(drat, quietly = TRUE)) {
  suppressMessages(drat::addRepo("rrelaxiv", "https://errickson.net/rrelaxiv"))
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
    },
    logit = function (x) {
      log(x/(1 - x))
    },
    invlogit = function (x) {
      1/(1 + exp(-x))
    }),
    name = 'MyFunctions'
    )
