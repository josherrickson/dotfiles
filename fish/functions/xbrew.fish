# Defined in - @ line 1
function xbrew --wraps='arch -x86_64 /usr/local/bin/brew' --description 'alias xbrew=arch -x86_64 /usr/local/bin/brew'
 command arch -x86_64 /usr/local/bin/brew $argv;
end
