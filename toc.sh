#!/bin/sh

TOC="TABLEOFCONTENT.md"

rm -f $TOC

echo "# Documentation index" > $TOC

find . -name \*.md -exec awk 'function formatLine(line) {anchor=tolower(line); gsub(" ","-",anchor); printf("[%s](%s#%s)\n",line,FILENAME,anchor); }
BEGIN{ print ""}
/^# /{ printf("- "); formatLine(substr($0,3)); }
/^## /{ printf("  * "); formatLine(substr($0,4)); }' {} \; > $TOC
