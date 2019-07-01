#!/usr/bin/env bash

cd "`dirname "$0"`/.."

echo "digraph {"

for i in `find apps/ -name '*.app.src' `;do
	orig=`echo $i|cut -d/ -f4|cut -d. -f1|cut -d_ -f2-`
	echo "// $orig"
	for dest in `cat $i|grep -Pv '^\s*%'|grep -e '\[ automate'  -e ' , automate'|cut -d_ -f2-|cut -d% -f1`;do
		echo "$orig -> $dest;"
	done
	echo
done
echo "}"
