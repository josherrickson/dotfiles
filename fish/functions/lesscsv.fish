function lesscsv --description 'Display CSV file'
 cat $argv | perl -pe 's/((?<=,)|(?<=^)),/ ,/g;'  | column -t -s,
end
