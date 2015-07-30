#!/bin/bash
mv fanboy-complete.txt fanboy-complete.txt.old
wget https://www.fanboy.co.nz/r/fanboy-complete.txt
[ -f fanboy-complete.txt ] && rm fanboy-complete.txt.old
mv fanboy-ultimate.txt fanboy-ultimate.txt.old
wget https://www.fanboy.co.nz/r/fanboy-ultimate.txt
[ -f fanboy-ultimate.txt ] && rm fanboy-ultimate.txt.old
