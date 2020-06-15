function git_status_all -d "Loops through subdirectories performing `git status`"
    set -l current (pwd)
    for dir in (find . -type d -name ".git")
        set -l gitdir (echo $dir | string replace -r '.git$' '')
        cd $gitdir
        echo "GIT STATUS IN "$gitdir
        git status -s
        cd $current
    end
end
