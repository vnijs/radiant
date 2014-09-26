# title: "Usefull links and commands for Shiny and Git"
# author: "Vincent Nijs"
# date: "September 26, 2014"

# getting started with git
https://www.codeschool.com/courses/try-git

# the above is part of the Data Analysis learning path on sliderule
http://www.mysliderule.com/learning-paths/data-analysis/learn

# for windows you need to get git before you can do stuff
http://git-scm.com/download/win

# Set editor to vim
git config --global core.editor /usr/bin/vim

# if you are 'behind the remote'
git pull origin master

# force the push
git push origin master --force

# getting a new repo from GitHub
# may be needed if you get 'rpostback-askpass' errors
git config --global user.name mostly-harmless
git config --global user.email vnijs@ucsd.edu
git clone https://github.com/mostly-harmless/radiant.git
git pull

# generating ssh keys for mac
https://help.github.com/articles/generating-ssh-keys

# generating ssh keys for windows
https://help.github.com/articles/generating-ssh-keys#platform-windows

# getting started with Shiny
http://shiny.rstudio.com/

# getting started with dplyr
http://www.dataschool.io/dplyr-tutorial-for-faster-data-manipulation-in-r/
