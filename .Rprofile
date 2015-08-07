# Hardcode a https mirror.
options(
  download.file.method = "libcurl",
  repos = c(CRAN = "https://mirrors.nics.utk.edu/cran/"),
  max.print = 1000)

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
    }),
    name = 'MyFunctions'
    )
