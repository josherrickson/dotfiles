# Defined in - @ line 1
function lesscsv --description 'Display CSV file'
 command cat $argv | perl -pe 's/((?<=,)|(?<=^)),/ ,/g;'  | column -t -s,
end
