# Defined in - @ line 1
function rmds_store --wraps=find\ \~\ -name\ \".DS_Store\"\ -depth\ -exec\ rm\ \{\}\ \\\; --description alias\ rmds_store=find\ \~\ -name\ \".DS_Store\"\ -depth\ -exec\ rm\ \{\}\ \\\;
  find ~ -name ".DS_Store" -depth -exec rm {} \; $argv;
end
