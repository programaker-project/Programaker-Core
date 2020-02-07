#!/usr/bin/env bash

cd "`dirname "$0"`/.."

output=$(
    echo "digraph {"
    echo "  {rank=same; rest_api bot_engine}"

    for i in `find apps/ -name '*.app.src' `;do
	      orig=`echo $i|cut -d/ -f4|cut -d. -f1|cut -d_ -f2-`
	      echo "    // $orig"
	      for dest in `cat $i|grep -Pv '^\s*%'|grep -e '\[ automate'  -e ' , automate'|cut -d_ -f2-|cut -d% -f1`;do
		        echo "    $orig -> $dest;"
	      done
	      echo
    done
    echo "}"
      )

case "$1" in
    # Render
    *.png)
        echo "$output" | dot -Tpng /dev/stdin -o "$1"
        ;;
    *.svg)
        echo "$output" | dot -Tsvg /dev/stdin -o "$1"
        ;;

    # Ignore
    "")
        echo "$output"
        ;;

    # Save as text
    *)
        echo "$output" > "$1"
        ;;
esac
