# Defined in - @ line 1
function grep --wraps='grep --color=always' --description 'alias grep=grep --color=always'
 command grep --color=always $argv;
end
