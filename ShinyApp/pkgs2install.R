cmirror <- getOption("repos")
cmirror["CRAN"] <- "https://cran.ma.imperial.ac.uk/"
options(repos = cmirror)


pkgs2install <- c("devtools", 
                  "shiny",
                  "knitr", 
                  "rmarkdown",
                  "rhandsontable",
                  "tinytex",
                  "here", 
                  "data.table",
                  "dplyr",
                  "tidyr", 
                  "ggplot2",
                  "openxlsx", 
                  "flextable", 
                  "officer")

### Install packages (if required):
if (!requireNamespace(c(pkgs2install), quietly = TRUE)){
  install.packages(c(pkgs2install), dependencies = TRUE)
} 
