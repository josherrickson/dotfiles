#!/bin/bash

echo "i <- rownames(installed.packages())" > __tmp__.R

# library() calls
grep -oh --include=\*.{Rmd,R} -r * -e "library([A-Za-z0-9]\+)" | sed 's_library(_"_' | sed 's_)_",_' | perl -p -e 's/\n/ /' | sed 's_^_new <- c(_' | sed  's_, *$_)_' >> __tmp__.R

# require() calls
grep -oh --include=\*.{Rmd,R} -r * -e "require([A-Za-z0-9]\+)" | sed 's_require(_"_' | sed 's_)_",_' | perl -p -e 's/\n/ /' | sed 's_^_new <- c(new, _' | sed  's_, *$_)_' >> __tmp__.R

echo "install.packages(new[!new %in% i])" >> __tmp__.R

# For debugging
#cat __tmp__.R

R --quiet -e "source('__tmp__.R')"

rm __tmp__.R
