#!/bin/bash

#1.Inform you if  you're local repo is up to date with the remote repo
git remote show origin > status.txt

if [ $(grep "out of date" status.txt | wc -l) -gt 0 ]
then 
     echo "Your local repository is out of date"
else
     echo "Your local repository is up to date"
fi


#2.Put all uncommited changes in a file changes.log
git diff --staged > changes.log #method 1
git diff  > changes.log         #method 2
echo "uncommited changes put in changes.log"

#3.Put each line from every file of your project with the tag #TODO into a file todo.log
grep -r "#TODO" --exclude todo.log . > todo.log
echo "lines with #TODO put in todo.log"

#4.Check all haskell files for syntax errors and puts the results into error.log
shopt -s nullglob

find . -name "*.hs" | 
     while read f 
     do
         ghc -fno-code $f 2>> error.log
     done
echo "haskell syntax errors put in error.log"
