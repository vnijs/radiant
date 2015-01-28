local_dir <- Sys.getenv("R_LIBS_USER")
if(!file.exists(local_dir)) dir.create(local_dir, recursive = TRUE)

filename <- paste0(Sys.getenv("USERPROFILE") ,"/Desktop/radiant.bat")
launch_string <- paste0(Sys.which('R'), " -e \"if(!require(radiant)) { options(repos = c(XRAN = 'http://mostly-harmless.github.io/radiant_miniCRAN/')); install.packages('radiant'); }; shiny::runApp(system.file('marketing', package='radiant'), port = 4475, launch.browser = TRUE)\"")
cat(launch_string,file=filename,sep="\n")
