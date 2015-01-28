local_dir <- Sys.getenv("R_LIBS_USER")
if(!file.exists(local_dir)) dir.create(local_dir, recursive = TRUE)

filename <- paste0("/Users/",Sys.getenv("USER"),"/Desktop/radiant.command")
launch_string <- paste0("#!/usr/bin/env Rscript\n if(!require(radiant)) {\n options(repos = c(XRAN = 'http://mostly-harmless.github.io/radiant_miniCRAN/'))\n install.packages('radiant')\n }\n\n shiny::runApp(system.file('marketing', package='radiant'), port = 4475, launch.browser = TRUE)\n")
cat(launch_string,file=filename,sep="\n")
Sys.chmod(filename, mode = "0755")
