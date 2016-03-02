#' Set initial value for shiny input
#'
#' @details Useful for radio button or checkbox
#'
#' @param inputvar Name shiny input
#' @param init Initial value to use if state value for input not set
#'
#' @return value for inputvar
#'
#' @examples
#'
#' r_state <<- list()
#' state_init("test")
#' state_init("test",0)
#' r_state$test <- c("a","b")
#' state_init("test",0)
#' shiny::radioButtons("rb", label = "Button:", c("a","b"), selected = state_init("rb", "a"))
#' r_state$rb <- "b"
#' shiny::radioButtons("rb", label = "Button:", c("a","b"), selected = state_init("rb", "a"))
#' rm(r_state)
#'
#' @seealso \code{\link{state_single}}
#' @seealso \code{\link{state_multiple}}
#' @seealso \code{\link{copy_from}}
#'
#' @export
ostate_init <- function(inputvar, init = "") {
  if (!exists("r_state")) stop("Make sure to use copy_from inside shinyServer for the state_* functions")
  if (is_empty(r_state[[inputvar]])) init else r_state[[inputvar]]

  ## perhaps use this for "input_init"
  # isolate({
  #   print("##########")
  #   print(r_state[["viz_xvar"]])
  #   print(input[["viz_xvar"]])
  #   print("viz_combx")
  #   print(r_state[["viz_combx"]])
  #   print(input[["viz_combx"]])
  #   print("----")
  #   print(inputvar)
  #   print(inputvar %in% names(input))


  #   # cat("r_state", r_state[["viz_xvar"]], sep=' ', file=stderr(), "\n")
  #   # cat("input", input[["viz_xvar"]], sep=' ', file=stderr(), "\n")
  #   # cat("viz_combx", as.character(input[["viz_combx"]]), sep=' ', file=stderr(), "\n")
  #   # ninp <- names(input)
  #   # cat(r_state[[inputvar]])
  #   # cat(sprintf(r_state[[inputvar]]), sep='', file=stderr())
  #   # cat(sprintf(input[[inputvar]]), sep='', file=stderr())
  #   if (is_empty(r_state[[inputvar]]) && !inputvar %in% names(input)) {
  #     print("init")
  #     init
  #   } else if (inputvar %in% names(input)) {
  #     print("input")
  #     r_state[[inputvar]] <<- init
  #     input[[inputvar]]
  #   } else {
  #     print("state")
  #     r_state[[inputvar]]
  #   }
  # })
}

#' Set initial value for shiny input from a list of values
#'
#' @details Useful for select input with multiple = FALSE
#'
#' @param inputvar Name shiny input
#' @param vals Possible values for inputvar
#' @param init Initial value to use if state value for input not set
#'
#' @return value for inputvar
#'
#' @examples
#'
#' r_state <- list()
#' state_single("test",1:10,1)
#' r_state$test <- 8
#' state_single("test",1:10,1)
#' shiny::selectInput("si", label = "Select:", c("a","b"), selected = state_single("si"))
#' r_state$si <- "b"
#' shiny::selectInput("si", label = "Select:", c("a","b"), selected = state_single("si", "b"))
#'
#' @seealso \code{\link{state_init}}
#' @seealso \code{\link{state_multiple}}
#' @seealso \code{\link{copy_from}}
#'
#' @export
ostate_single <- function(inputvar, vals, init = character(0)) {
  if (!exists("r_state")) stop("Make sure to use copy_from inside shinyServer for the state_* functions")
  r_state %>% { if (is_empty(.[[inputvar]])) init else vals[vals == .[[inputvar]]] }
}

#' Set initial values for shiny input from a list of values
#'
#' @details Useful for select input with multiple = TRUE and when you want to use inputs selected for another tool (e.g., pre_factor and full_factor or hier_clus and kmeans_clus in Radiant)
#'
#' @param inputvar Name shiny input
#' @param vals Possible values for inputvar
#' @param init Initial value to use if state value for input not set
#'
#' @return value for inputvar
#'
#' @examples
#'
#' r_state <- list()
#' state_multiple("test",1:10,1:3)
#' r_state$test <- 8:10
#' state_multiple("test",1:10,1:3)
#' shiny::selectInput("sim", label = "Select:", c("a","b"),
#'   selected = state_multiple("sim", c("a","b")),  multiple = TRUE)
#' r_state$sim <- c("a","b")
#' shiny::selectInput("sim", label = "Select:", c("a","b"),
#'   selected = state_single("sim", c("a","b")),  multiple = TRUE)
#'
#' @seealso \code{\link{state_init}}
#' @seealso \code{\link{state_single}}
#' @seealso \code{\link{copy_from}}
#'
#' @export
ostate_multiple <- function(inputvar, vals, init = character(0)) {
  if (!exists("r_state")) stop("Make sure to use copy_from inside shinyServer for the state_* functions")
  r_state %>%
    { if (is_empty(.[[inputvar]]))
        ## "a" %in% character(0) --> FALSE, letters[FALSE] --> character(0)
        vals[vals %in% init]
      else
        vals[vals %in% .[[inputvar]]]
    }
}

test_that("state_init", {
 r_state <<- list()
 expect_equal(state_init("test"),"")
 expect_equal(state_init("test",0),0)
 # outside of tests use <- not <<-
 r_state$test <<- c("a","b")
 expect_equal(state_init("test",0),c("a","b"))
 expect_equal(state_init("rb", "a"),"a")
 r_state$rb <<- "b"
 expect_equal(state_init("rb", "a"),"b")
 rm(r_state, envir = .GlobalEnv)
 expect_error(state_init("rb", "a"),"Error in state_init.*")
 cat("\n")
})

test_that("state_single", {
 r_state <<- list()
 expect_equal(state_single("test",1:10,1),1)
 # outside of tests use <- not <<-
 r_state$test <<- 8
 expect_equal(state_single("test",1:10,1),8)
 expect_equal(state_single("si"),character(0))
 r_state$si <<- "b"
 expect_equal(state_single("si",c("a","b"),"a"),"b")
 rm(r_state, envir = .GlobalEnv)
 expect_error(state_single("si"),"Error in state_single.*")
 cat("\n")
})

test_that("state_multiple", {
 r_state <<- list()
 expect_equal(state_multiple("test",1:10,1:3),1:3)
 # outside of tests use <- not <<-
 r_state$test <<- 8:10
 expect_equal(state_multiple("test",1:10,1:3),8:10)
 expect_equal(state_multiple("sim", c("a","b")),character(0))
 r_state$sim <<- c("a","b")
 expect_equal(state_multiple("sim", c("a","b")),c("a","b"))
 rm(r_state, envir = .GlobalEnv)
 expect_error(state_multiple("sim", c("a","b")),"Error in state_multiple.*")
})
