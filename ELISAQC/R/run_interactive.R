#' @title Launch interactive Shiny app
#'
#' @author Amy Mikhail \email{Amy.Mikhail@@phe.gov.uk}
#'
#' @description
#' This function will launch an interactive Shiny app that
#' allows the user to set the parameters for the ELISA plate.
#'
#' @return QC report, lab results for validation, case line list
#'
#' @import shiny rmarkdown knitr
#'
#' @examples
#' \dontrun{
#' # Launch interactive dialogue box:
#' run_interactive()
#' }
#'
#'
#' @export
run_interactive <- function() {

  # List packages to load:
  pkgs2load = c("shiny", "rmarkdown", "knitr")

  # Load required packages:
  lapply(c(pkgs2load), requireNamespace, quietly = TRUE)

  # Replace original knit_params_ask function with new conditional one:

  # Untrace the original function
  untrace(rmarkdown::knit_params_ask)

  # Overwrite with the new conditional funciton:
  trace(what = rmarkdown::knit_params_ask,
        edit = function(name = name, file = file, title = title){ELISAQC::knit_params_conditional})


  # Locate the app in the package directory:
  appDir = system.file("interactive/MOLDataHub.Rmd",
                       package = "ELISAQC",
                       mustWork = TRUE)

  # Generate an error warning if the app is not found:
  if (appDir == "") {

    stop("Could not find R markdown file. Try re-installing `ELISAQC`.", call. = FALSE)

  } else {

    # Otherwise launch the app:
    rmarkdown::render(appDir, params = "ask", clean = FALSE)

    # Untrace the conditional knit params:
    untrace(what = rmarkdown::knit_params_ask)

  }

}
