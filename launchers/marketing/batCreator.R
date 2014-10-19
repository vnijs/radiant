# commands to run Radiant
args <- commandArgs(trailingOnly = TRUE)

if(!'shiny' %in% installed.packages()[,'Package']) {
  # setting the url for the miniCRAN
  local_dir <- Sys.getenv("R_LIBS_USER")
  if(!file.exists(local_dir)) dir.create(local_dir, recursive = TRUE)
#   args <- normalizePath(paste0(getwd(),"/launchers/marketing"))
  pth <- normalizePath(paste0(args,'/../../radiant-miniCRAN'),winslash='/')
  mcran <- paste0("file:///",pth)
  options(repos = c(CRAN = mcran))
  install.packages('shiny', local_dir)
}

com_string <- paste0("cd ",args,"\n",Sys.which('R'), " -e ",
	"\"shiny::runApp('../../inst/marketing',port = 4475, launch.browser=TRUE)\"\npause")

# pth <- shQuote(normalizePath("~/radiant_tmp"))
pth <- normalizePath("~/radiant_temp", winslash="/")
if(!file.exists(pth)) dir.create(pth, recursive = TRUE)
filename <- paste0(pth,"/radiant_marketing.bat")
cat(com_string,file=filename,sep="\n")
edit(file = filename)

# keeps the editor open for a little while
Sys.sleep(30)
