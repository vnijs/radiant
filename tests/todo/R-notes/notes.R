##############################
# Getting and cleaning data
##############################
https://github.com/benjamin-chan/GettingAndCleaningData

####################################
# Lessons learned from teaching DS
####################################
http://www.dataschool.io/teaching-data-science/


#############
# ifelse
#############
x <- 1:10

ifelse(is.numeric(x), x^2, x)  			# only return 1st number (i.e, length of input)
if(is.numeric(x)) { x^2 } else { x }     # normal if, returns all values
foo <- function(x) ifelse(is.numeric(x), # will also work
                          return(x^2),
                          return(x))
foo(x)


############################################################
# transforming character variables to integer and numeric
############################################################

# This produces the wrong result
x <- as.factor(rep(c('2','3'), 8))
as.numeric(x)
as.integer(x)

as_int <- function(x) {
	if(x %>% is.factor) {
		x %>% levels %>% as.integer %>% extract(x)
	} else {
		x %>% as.integer
	}
}

as_num <- function(x) {
	if(x %>% is.factor) {
		levels(x) %>% as.numeric %>% extract(x)
	} else {
    x %>% as.numeric
	}
}

# This does work
as_num(x)
as_int(x)


###################
# R and databases
###################
http://www.jason-french.com/blog/2014/07/03/using-r-with-mysql-databases/
https://github.com/rstats-db/RSQLite
https://github.com/rstats-db/RMySQL
http://joshualande.com/data-science-sql/

"I have used fastread or data.table’s fread. Both are very fast but will fail if you run out of memory. I believe you can do what you are asking with MySql or RSQLite but I haven’t tried it yet. I have read in files with over 50 million rows in memory without problems but I probably couldn’t do that on my laptop."

###############################
# Math and programming links
###############################
http://www.bitbootcamp.com/resources.html



###############################
# ggvis plotting
###############################

http://jimhester.github.io/ggplot2ToGgvis/



##################################
# From NC state - replicate in R
##################################
http://www.csc.ncsu.edu/faculty/healey/maa-14/python/


##################################
# CSS
##################################
https://github.com/gammarama/intRo/compare/shiny11
http://www.codecademy.com/courses/web-beginner-en-TlhFi/0/1?curriculum_id=50579fb998b470000202dc8b
