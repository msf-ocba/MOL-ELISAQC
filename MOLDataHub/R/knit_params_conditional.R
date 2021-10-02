#' @title Conditional knit params ask
#'
#' @author Cong Chen (Cong.Chen@@phe.gov.uk)
#'
#' @description
#' This function replaces the native \code{rmarkdown::knit_params_ask()} function with a
#' conditional version that will hide some parameters if not needed based on the user's response.
#'
#' @details
#' For full details, see the help files for the native knit_params_ask function in the
#' rmarkdown package.  To use, add the term 'condition' to a parameter in the YAML header
#' of an R markdown file and then specify what the condition is (response from another parameter).
#'
#' @param file Path to the R Markdown document with configurable parameters.
#' @param input_lines Content of the R Markdown document. If \code{NULL}, the contents of \code{file} will be read.
#' @param params A named list of optional parameter overrides used in place of the document defaults.
#' @param shiny_args Additional arguments to \code{\link[shiny:runApp]{runApp}}.
#' @param save_caption Caption to use use for button that saves/confirms parameters.
#' @inheritParams rmarkdown::render
#'
#' @return named list with overridden parameter names and value.
#'
#' @import shiny rmarkdown knitr
#'
#' @examples
#' \dontrun{
#' # Create YAML header with conditions:
#' --
#' params:
#'   datasource:
#'     input: radio
#'     label: "Select data source type:"
#'     choices: [file, SQL]
#'     value: "SQL"
#'  rawdata:
#'    input: file
#'    label: "Select raw data file:"
#'    multiple: FALSE
#'    accept: ".xlsx"
#'    placeholder: "Choose file"
#'    value: ""
#'    condition: "input.datasource == 'file'"
#'--
#'
#' # Replace rmarkdown::knit_params_ask with knit_params_conditional:
#' untrace(rmarkdown::knit_params_ask)
#' trace(rmarkdown::knit_params_ask,
#'       edit = function(name = name,
#'                       file = file,
#'                       title = title){fingertipsHPI::knit_params_conditional})
#'
#' # Make a call to render with the revised function:
#' rmarkdown::render(appDir, params = "ask", clean = FALSE)
#'
#' # Untrace the conditional function to return to the original:
#' untrace(rmarkdown::knit_params_conditional)
#' }
#' @export
knit_params_conditional <- function(file = NULL,
                                    input_lines = NULL,
                                    params = NULL,
                                    shiny_args = NULL,
                                    save_caption = "Save",
                                    encoding = "UTF-8")
{
  # List packages to load:
  pkgs2load = c("shiny", "rmarkdown", "knitr")

  # Load required packages:
  lapply(c(pkgs2load), requireNamespace, quietly = TRUE)

  # Conditional function:
  if (is.null(input_lines)) {
    if (is.null(file)) {
      stop("knit_params_ask must have a non-NULL file or input_lines parameter")
    }
    input_lines <- read_utf8(file, encoding)
  }
  knit_params <- knitr::knit_params(input_lines)
  if (!is.null(params)) {
    if (!is.list(params) || (length(names(params)) != length(params))) {
      stop("knit_params_ask params argument must be a named list")
    }
  }
  if (length(knit_params) == 0) {
    return(params_namedList())
  }
  configurable <- Filter(params_configurable, knit_params)
  unconfigurable <- Filter(Negate(params_configurable), knit_params)
  values <- params_namedList()
  server <- function(input, output, session) {
    param.ui <- function(param) {
      inputControlFn <- params_get_control(param)
      inputControlFnFormals <- names(formals(inputControlFn))
      inputId <- param$name
      label <- params_label(inputControlFn, param)
      arguments = list(inputId = inputId, label = label)
      attrib_names <- unique(c(names(param), "value"))
      hasCondition <- FALSE
      condition <- NULL
      lapply(attrib_names, function(name) {
        if (name %in% c("name", "input", "expr")) {
        }
        else if (name == "label") {
          arguments$label <<- label
        }
        else if (name == "condition") {
          hasCondition <<- TRUE
          condition <<- param[["condition"]]
        }
        else if (name == "value") {
          current_value <- param$value
          if (!is.null(params)) {
            override <- params[[param$name]]
            if (!is.null(override)) {
              current_value <- override
            }
          }
          current_value <- params_value_to_ui(inputControlFn,
                                              current_value, param$show_default)
          if ("value" %in% inputControlFnFormals) {
            arguments$value <<- current_value
          }
          else if ("selected" %in% inputControlFnFormals) {
            arguments$selected <<- current_value
          }
        }
        else if (name == "show_default") {
        }
        else {
          arguments[[name]] <<- if (inherits(param[[name]],
                                             "knit_param_expr")) {
            param[[name]][["value"]]
          }
          else param[[name]]
        }
      })
      uidefault <- params_value_to_ui(inputControlFn,
                                      param$value, param$show_default)
      hasDefaultValue <- function(value) {
        identical(uidefault, value)
      }
      inputControl <- NULL
      unsupported <- setdiff(names(arguments), inputControlFnFormals)
      if (length(unsupported) > 0) {
        inputControl <- shiny::div(class = "form-group",
                                   tags$label(class = "control-label", param$name),
                                   shiny::div(paste("Cannot customize the parameter \"",
                                                    param$name, "\" ", "because the \"", params_get_input(param),
                                                    "\" ", "Shiny control does not support: ",
                                                    paste(unsupported, collapse = ", "), sep = "")))
      }
      else {
        inputControl <- do.call(inputControlFn, arguments)
      }
      showSelectControl <- NULL
      selectControl <- NULL
      selectInputId <- paste0("select_", param$name)
      makeSelectControl <- function(default_name, custom_name) {
        showSelectControl <<- function(current) {
          (is.null(current) || identical(current, "default"))
        }
        hasDefaultValue <<- function(value) {
          FALSE
        }
        choices <- list()
        choices[[default_name]] <- "default"
        choices[[custom_name]] <- "custom"
        selectControl <<- shiny::selectInput(inputId = selectInputId,
                                             label = label, choices = choices)
      }
      if (is.null(params[[param$name]])) {
        if (identical("Sys.time()", param$expr)) {
          makeSelectControl(paste0("now (", param$value,
                                   ")"), "Use a custom time")
        }
        else if (identical("Sys.Date()", param$expr)) {
          makeSelectControl(paste0("today (", param$value,
                                   ")"), "Use a custom date")
        }
        else if (is.null(param$value)) {
          if (!identical(inputControlFn, shiny::fileInput)) {
            makeSelectControl("Unspecified (NULL)",
                              "Use a custom value")
          }
        }
      }
      wrapConditionalPanel <- function(hasCondition, condition,
                                       shinyItem) {
        output <- if (hasCondition) {
          conditionalPanel(condition, shinyItem)
        }
        else {
          shinyItem
        }
        output
      }
      output[[paste0("ui_", param$name)]] <- shiny::renderUI({
        pre_render <- wrapConditionalPanel(hasCondition,
                                           condition, if (!is.null(showSelectControl) &&
                                                          showSelectControl(input[[selectInputId]])) {
                                             selectControl
                                           }
                                           else {
                                             inputControl
                                           })
        pre_render
      })
      shiny::observe({
        uivalue <- input[[param$name]]
        if (is.null(uivalue) || hasDefaultValue(uivalue)) {
          values[[param$name]] <<- NULL
        }
        else {
          values[[param$name]] <<- params_value_from_ui(inputControlFn,
                                                        param$value, uivalue)
        }
      })
    }
    lapply(configurable, function(param) {
      param.ui(param)
    })
    shiny::observeEvent(input$save, {
      session$onFlushed(function() {
        session$close()
        shiny::stopApp(values)
      })
    })
    shiny::observeEvent(input$cancel, {
      session$onFlushed(function() {
        session$close()
        shiny::stopApp(NULL)
      })
    })
  }
  contents <- tags$div(shiny::fluidRow(shiny::column(12, lapply(configurable,
                                                                function(param) {
                                                                  shiny::uiOutput(paste0("ui_", param$name))
                                                                }))), class = "container-fluid")
  if (length(unconfigurable) > 0) {
    skipped <- tags$div(tags$strong("Note:"), "The following parameters cannot be customized:",
                        paste(lapply(unconfigurable, function(param) param$name),
                              collapse = ", "))
    contents <- shiny::tagAppendChildren(contents, shiny::fluidRow(shiny::column(12,
                                                                                 skipped)))
  }
  footer <- tags$div(tags$div(shiny::fluidRow(shiny::column(12,
                                                            shiny::actionButton("save", save_caption, class = "btn-primary navbar-btn pull-right"),
                                                            shiny::actionButton("cancel", "Cancel", class = "navbar-btn pull-right"))),
                              class = "container-fluid"), class = "navbar navbar-default navbar-fixed-bottom")
  style <- tags$style(".container-fluid .shiny-input-container { width: auto; }",
                      ".navbar button { margin-left: 10px; }", "body { padding-bottom: 70px; }")
  script <- tags$script(HTML("$(document).keyup(function(e) {\n",
                             "if (e.which == 13) { $('#save').click(); } // enter\n",
                             "if (e.which == 27) { $('#cancel').click(); } // esc\n",
                             "});"))
  ui <- shiny::bootstrapPage(tags$head(style, script), contents,
                             footer)
  shiny_app <- shiny::shinyApp(ui = ui, server = server)
  shiny_args <- merge_lists(list(appDir = shiny_app), shiny_args)
  do.call(shiny::runApp, shiny_args)
}
