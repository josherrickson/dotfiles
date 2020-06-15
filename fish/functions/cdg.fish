function cdg -d "Return to top directory in Git repo"
    set -l topdir (git rev-parse --show-toplevel)
    cd $topdir
end
