# The Documentation of ProjectAnalyze.sh

A shell script to perform the following functionality:

 1. Informs you if your local repo is up to date with the remote repo
 
 2. Puts all uncommited changes in a file changes.log 
 
 3. Puts each line from every file of your project with the tag #TODO into a file todo.log

 4. Checks all haskell files for syntax errors and puts the results into error.log
 

## The discriptions of functions in th shell:

- Function 1:

Basically I use the `git remote show origin` command to track the remote repo. At the bottom of the feedback, there is a line telling you if the current working directory is up-to-date or out-of-date. To extract this piece of information, I direct the feedback into a file and grep the key words from it and echo the corresonding update status based on the key words.



- Function 2:

`git diff` - showing the differences of the working directory with the index.

`git diff --staged` - showing the differences of the staging index and the remote repo.This command will tell you the git added but not commited yet changes.



- Function 3:

`grep -r "#TODO" --exclude todo.log .` is for grepping each line with "#TUDO" tag from the files(exclusing todo.log itself) of current directory and subdirectories; 
`> todo.log` is for directing the results in the todo.log.



- Function 4:

At the beginning of this part, I put `shopt -s nullglob` for using glob pattern properly. In the main part, I use a while loop piping with the find command for finding all haskell files in the current directory as well as the subdirectory and iterating those files to direct syntax errors into error.log.


