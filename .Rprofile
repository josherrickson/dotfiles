# Hardcode a https mirror.
.First <- function() {
  options(
    download.file.method = "libcurl",
    repos = c(CRAN = "https://cran.rstudio.com/"),
    max.print = 1000,
    dplyr.print_max = 1000#,
#    browserNLdisabled = TRUE
  )
  cat(paste0("Version: ", version$major, ".", version$minor, " ",
             version$status, "\n"))
}

pkg <- utils::installed.packages()[, "Package"]

# Better yesno function from devtools
if (isTRUE("devtools" %in% pkg)) {
  setHook(packageEvent("devtools", "onLoad"), {
    yesno <- function(...) {
      cat(paste0(..., collapse = ""))
      utils::menu(c("Yes", "No")) != 1
    }
    utils::assignInNamespace("yesno", yesno, "devtools")
    rm(yesno)
  })
}

# Add rrelaxiv to drat repos
if (isTRUE("drat" %in% pkg)) {
  setHook(packageEvent("drat", "onLoad"), {
    drat::addRepo("rrelaxiv", "https://errickson.net/rrelaxiv")
  })
}

# Load grinch library for means
# https://github.com/josherrickson/grinch
if (isTRUE("grinch" %in% pkg)) {
  require(grinch, quietly = TRUE)
}

rm(pkg)



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
