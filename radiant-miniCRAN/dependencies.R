if (.Platform$OS.type == 'windows') {
  Sys.setlocale(category = 'LC_ALL','English_United States.1252')
} else {
  Sys.setlocale(category = 'LC_ALL','en_US.UTF-8')
}

# install packages required to run radiant
# next line checks if Radiant is being run locally
if(file.exists("~/Desktop/GitHub/radiant_dev/") || file.exists("~/../Desktop/GitHub/radiant_dev/")) {

  # for running Radiant from the dev directory
  pth <- normalizePath("~/Desktop/GitHub/radiant_dev/radiant-miniCRAN", winslash = "/")

} else {

    # setting the location of the miniCRAN
    if(file.exists("radiant-miniCRAN")) {
      # This line is only here to deal with the old launchers on windows
      # Remove after Fall 2014
      pth <- normalizePath("./radiant-miniCRAN/", winslash = "/")
    } else {
      pth <- normalizePath("../../radiant-miniCRAN", winslash = "/")
    }
}

# remove shinyAce if old
l_inst <- installed.packages()
if('shinyAce' %in% rownames(l_inst) && l_inst['shinyAce', 'Version'] < "0.1.1")
  remove.packages('shinyAce')

# if(!'httpuv' %in% rownames(l_inst))
#   install.packages('httpuv')

# install to user directory
local_dir <- Sys.getenv("R_LIBS_USER")
if(!file.exists(local_dir)) dir.create(local_dir, recursive = TRUE)

# loading the list of pkgs needed to run radiant
source(paste0(pth,"/pkgs.R"))

# setting the url for the miniCRAN
mcran <- paste0("file:///",pth)

# look locally first and then in the Rstudion CRAN
# options(repos = c(CRAN = c(mcran,"http://cran.rstudio.com")))
options(repos = c(CRAN = mcran))

# udpate old-packages
update.packages(lib.loc = local_dir, ask = FALSE)
# pkgs <- installed.packages()[,'Package']

# install packages that are available but were not installed before
to_inp <- new.packages(lib.loc = local_dir)

#   if(length(to_inp) != 0) install.packages(to_inp, lib.loc = local_dir)
if(length(to_inp) != 0) install.packages(to_inp, local_dir, dependencies = TRUE)

# add function to remove packages that are not used by Radiant?
# might mess up someones stuff. maybe better to do this like packRat does it
# i.e., install into a non-standard directory

# load/attach packages
suppressWarnings(sapply(pkgs, require, lib.loc = local_dir, character.only=TRUE))
