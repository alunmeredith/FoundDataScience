#!/bin/bash
	FILES = (find . -name \*.csv -print)
	for f in $FILES
	do
		mongoimport -d DSproject -c nationalCrime --type csv --file "$f" --headerline
	done

#FILES="./2015-01/*"