# Defined in - @ line 1
function lst --wraps='date ;ls -lt | head' --description 'alias lst=date;ls -lt | head'
  date ;ls -lt | head $argv;
end
