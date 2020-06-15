# Defined in - @ line 1
function install_R_deps --wraps='bash ~/.bash-scripts/install_R_deps.sh' --description 'alias install_R_deps=bash ~/.bash-scripts/install_R_deps.sh'
  bash ~/.bash-scripts/install_R_deps.sh $argv;
end
