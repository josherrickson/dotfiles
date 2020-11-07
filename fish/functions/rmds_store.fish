function rmds_store
  find ~ -name ".DS_Store" -depth -exec rm {} \; $argv;
end
