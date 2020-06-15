# Defined in - @ line 1
function fgrep --wraps='fgrep --color=always' --description 'alias fgrep=fgrep --color=always'
 command fgrep --color=always $argv;
end
