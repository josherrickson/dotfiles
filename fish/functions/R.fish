# Defined in - @ line 1
function R --wraps='R --quiet' --description 'alias R=R --quiet'
 command R --quiet $argv;
end
