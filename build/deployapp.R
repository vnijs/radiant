#' Deploy an Application
#'
#' Deploy a \link[shiny:shiny-package]{shiny} application to the ShinyApps
#' service.
#' @details Prior to deploying an application you should call the
#'   \code{\link{setAccountInfo}} function to register your ShinyApps account on
#'   the local system.
#'
#'   After the initial deployment of an application from a given \code{appDir},
#'   subsequent deployments will automatically use the \code{appName} and
#'   \code{account} parameters of the initial deployment (unless overriden
#'   explicitly).
#'
#'   For details on options that affect the behavior of \code{deployApp} see the
#'   article on \link[shinyapps:shinyappsOptions]{package options}.
#' @param appDir Directory containing application. Defaults to
#'   current working directory.
#' @param appName Name of application (names must be unique with ShinyApps
#'   accounts). Defaults to the base name of the specified \code{appDir}.
#' @param account ShinyApps account to deploy application to. This parameter is
#'   only required for the initial deployment of an application when there are
#'   multiple accounts configured on the system (see \link{accounts}).
#' @param upload If \code{TRUE} (the default) then the application is uploaded
#'   from the local system prior to deployment. If \code{FALSE} then it is
#'   re-deployed using the last version that was uploaded.
#' @param launch.browser If true, the system's default web browser will be
#'   launched automatically after the app is started. Defaults to \code{TRUE} in
#'   interactive sessions only.
#' @param quiet Request that no status information be printed to the console
#'   during the deployment.
#' @param lint Lint the project before initiating deployment, to identify
#'   potentially problematic code?
#' @examples
#' \dontrun{
#'
#' # deploy the application in the current working dir
#' deployApp()
#'
#' # deploy an application in another directory
#' deployApp("~/projects/shiny/app1")
#'
#' # deploy using an alternative application name
#' deployApp("~/projects/shiny/app1", appName = "myapp")
#'
#' # deploy specifying an explicit account name, then
#' # redeploy with no arguments (will automatically use
#' # the previously specified account)
#' deployApp(account = "jsmith")
#' deployApp()
#'
#' # deploy but don't launch a browser when completed
#' deployApp(launch.browser = FALSE)
#' }
#' @seealso \code{\link{applications}}, \code{\link{terminateApp}}, and
#'   \code{\link{restartApp}}
#' @export
deployApp <- function(appDir = getwd(),
                      appName = NULL,
                      account = NULL,
                      upload = TRUE,
                      launch.browser = getOption("shinyapps.launch.browser",
                                                 interactive()),
                      quiet = FALSE,
                      lint = TRUE) {

  if (!isStringParam(appDir))
    stop(stringParamErrorMessage("appDir"))

  if (!is.null(appName) && !isStringParam(appName))
    stop(stringParamErrorMessage("appName"))

  # normalize appDir path and ensure it exists
  appDir <- normalizePath(appDir, mustWork = FALSE)
  if (!file.exists(appDir) || !file.info(appDir)$isdir)
    stop(appDir, " is not a valid directory")

  # try to detect encoding from the RStudio project file
  .globals$encoding <- rstudioEncoding(appDir)
  on.exit(.globals$encoding <- NULL, add = TRUE)

  # functions to show status (respects quiet param)
  displayStatus <- displayStatus(quiet)
  withStatus <- withStatus(quiet)

  # initialize lucid client
  lucid <- lucidClient(accountInfo)

  # determine the deployment target and target account info
  target <- deploymentTarget(appDir, appName, account)
  accountInfo <- accountInfo(target$account)

  # get the application to deploy (creates a new app on demand)
  withStatus("Preparing to deploy application", {
    application <- applicationForTarget(lucid, accountInfo, target)
  })

  if (upload) {
    # create, and upload the bundle
    withStatus("Uploading application bundle", {
      bundlePath <- bundleApp(appDir)
      bundle <- lucid$uploadApplication(application$id, bundlePath)
    })
  } else {
    # redeploy current bundle
    bundle <- application$deployment$bundle
  }

  # wait for the deployment to complete (will raise an error if it can't)
  displayStatus(paste("Deploying application: ",
                      application$id,
                      "...\n", sep=""))
  task <- lucid$deployApplication(application$id, bundle$id)
  lucid$waitForTask(task$task_id, quiet)
  displayStatus(paste("Application successfully deployed to ",
                      application$url,
                      "\n", sep=""))

  # save the deployment info for subsequent updates
  saveDeployment(appDir,
                 target$appName,
                 target$account,
                 bundle$id,
                 application$url)


  # append the file to be launched to the URL if necessary
  amendedUrl <- application$url

  # check for a launch file (i.e. an Rmd file)
  launchFile <- guessLaunchFile(appDir)
  if (nchar(launchFile) > 0) {
    if (substr(amendedUrl, nchar(amendedUrl), nchar(amendedUrl)) != "/")
      amendedUrl = paste(amendedUrl, "/", sep = "")
    amendedUrl = paste(amendedUrl, launchFile, sep = "")
  }

  # launch the browser if requested
  if (isTRUE(launch.browser))
    utils::browseURL(amendedUrl)
  else if (is.function(launch.browser))
    launch.browser(amendedUrl)

  # successful deployment!
  invisible(TRUE)
}
