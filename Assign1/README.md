                 The documentation of the ProjectAnalyze.sh
How it's used:
#1 Informs you if you're local repo is up to date with the remote repo
#2 Puts all uncommited changes in a file changes.log 
#3 Puts each line from every file of your project with the tag #TODO into a file todo.log
#4 Checks all haskell files for syntax errors and puts the results into error.log

The discriptions of functions in th shell:
#1.Basically I use the "git remote show origin" command to track the remote repo. At the bottom of the feedback, there is a line telling you if the current working directory is up-to-date or out-of-date. To extract this piece of information, I direct the feedback into a file and grep the key words from it and echo the corresonding update status based on the key words.


#2.git diff : showing the differences of the working directory with the index.
   git diff --staged :showing the differences of the staging index and the remote repo.This command will tell you the git added but not commited yet changes.


#3.grep -r "#TODO" . :grepping each line with "#TUDO" tag from the files of current directory and subdirectories
   > todo.log : directing the results in the todo.log

#4. At the beginning of this part, I put "shopt -s nullglob" for using glob pattern properly. In the main part, I use a for loop to iterate all of the haskell files and direct the stderr of syntax in error.log.



