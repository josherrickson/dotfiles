# Defined in - @ line 1
function ls --wraps='ls -AhFG' --description 'alias ls=ls -AhFG'
 command ls -AhFG $argv;
end
