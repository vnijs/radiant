# commands to run Radiant
args <- commandArgs(trailingOnly = TRUE)
apppth <- "radiant/inst/quant"


if(!'shiny' %in% installed.packages()[,'Package']) {
  # setting the url for the miniCRAN
  local_dir <- Sys.getenv("R_LIBS_USER")
  if(!file.exists(local_dir)) dir.create(local_dir, recursive = TRUE)
#   pth <- normalizePath(getwd())
  pth <- normalizePath(args)
  mcran <- paste0("file:///",pth)
  options(repos = c(CRAN = mcran))
  update.packages(lib.loc = local_dir, ask = FALSE)
}

com_string <- paste0("cd ",args,"\n",Sys.which('R'), " -e ",
	"\"shiny::runApp('",apppth,"',port = 4403, launch.browser=TRUE)\"\npause")

pth <- shQuote(normalizePath("~/radiant_tmp"))
if(!file.exists(pth) dir.create(pth), recursive = TRUE)
filename <- shQuote(normalizePath("~/radiant_tmp/radiant_quant.bat"))
cat(com_string,file=filename,sep="\n")
edit(file = filename)

# keeps the editor open for a little while
Sys.sleep(30)
