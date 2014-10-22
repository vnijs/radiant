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

# install to user directory
local_dir <- Sys.getenv("R_LIBS_USER")
if(!file.exists(local_dir)) dir.create(local_dir, recursive = TRUE)

# loading the list of pkgs needed to run radiant
options(repos = c(CRAN = "http://cran.rstudio.com"))
source(paste0(pth,"/pkgs.R"))
install.packages(pkgs, local_dir)
