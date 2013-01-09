file.path.ci <- function(dir, name) {
  default <- file.path(dir, name)
  if (file.exists(default))
    return(default)
  if (!file.exists(dir))
    return(default)
  
  matches <- list.files(dir, name, ignore.case=TRUE, full.names=TRUE,
                        include.dirs=TRUE)
  if (length(matches) == 0)
    return(default)
  # return(matches[[1]])
  return(matches)
}

# Instantiates the app in the current working directory.
# port - The TCP port that the application should listen on.
 globalR <- file.path.ci(getwd(), 'global.R')
 uiR <- file.path.ci(getwd(), 'ui.R')
 serverR <- file.path.ci(getwd(), 'server.R')
 dirR <- file.path.ci(getwd(),'')

 serverR
  
if (file.exists(globalR))
  source(globalR, local=FALSE)
  
?file.info
serverFileTimestamp <<- file.info(serverR)$mtime
print(serverFileTimestamp)

dirR
serverFileTimestamp <<- file.info(dirR)$mtime
print(serverFileTimestamp)

max(file.info(dir())$mtime)



