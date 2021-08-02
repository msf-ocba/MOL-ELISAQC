###############################################################################
### Set CRAN mirror:

cmirror <- getOption("repos")
cmirror["CRAN"] <- "https://cran.ma.imperial.ac.uk/"
options(repos = cmirror)

###############################################################################
### Function to install packages if required:

pkginstall <- function(..., repository = c("cran", "gitlab", "github", "bioc"), subdir = NULL, version = NULL, load = TRUE) {
  
  # Create list of libraries to check and install:
  libs = unlist(list(...))
  need = subset(libs, sapply(libs, function(x) !requireNamespace(x, quietly = TRUE)))
  
  # Check that user has supplied the correct input for the relevant repository and specify errors otherwise:
  
  if (length(libs) < 1) {
    stop('Please supply the name of at least one package to install and/or load')
  } else if (length(libs) > 1 & repository %in% c("gitlab", "github", "bioc")) {
    stop('Only one package at a time can be installed from git or Bioconductor repositories.  Please select a single pacakge and try again.')
  } else if (length(libs) == 1 & repository %in% c("gitlab", "github") & is.null(subdir)) {
    stop('Please specify a subdirectory if installing from git repositories')
  } else if (length(libs) == 1 & repository == "bioc" & is.null(version)) {
    stop('Please specify a package version to install if installing from Bioconductor')
  }
  
  # For git repositories, load devtools or if missing, install it:
  
  if (repository %in% c("gitlab", "github")) {
    
    getdevtools = !requireNamespace("devtools", quietly = TRUE)
    
    if (getdevtools == TRUE) {
      
      install.packages("devtools", dependencies = TRUE)
    }  
  }
  
  # For Bioconductor packages, load BiocManager or if missing, install it:
  
  if (repository == "bioc") {
    
    getbiocmanager = !requireNamespace("BiocManager", quietly = TRUE)
    
    if (getbiocmanager == TRUE) {
      
      install.packages("BiocManager")
    }
  }
  
  # If packages are missing, install them from the relevant repository:
  
  if (repository == "gitlab" & length(need) == 1) {
    
    devtools::install_git(paste0('https://gitlab.phe.gov.uk/', subdir, "/", need, ".git"))
    
  } else if (repository == "github" & length(need) == 1) {
    
    devtools::install_github(paste0(subdir, "/", need))
    
  } else if (repository == "bioc" & length(need) == 1) {
    
    BiocManager::install(need, version = as.character(version))
    
  } else if (repository == "cran" & length(need) >= 1) {
    
    install.packages(need, dependencies = TRUE, repos = 'https://cran.ma.imperial.ac.uk/')
    
  }
  
  # Load required packages if desired:
  
  if (load == TRUE) {
    
    lapply(libs, require, character.only = TRUE)
    
  }
  
  
  # Suppress 'no package' warning if install was succesful:
  
  suppressWarnings(libs %in% rownames(installed.packages(noCache = TRUE)))
  
}

###############################################################################
### List packages to install:

# CRAN packages:
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

###############################################################################
### Install packages if missing:

# Install CRAN packages:
pkginstall(c(pkgs2install), repository = "cran", load = FALSE)

# Install Github package EpiFunc:
pkginstall("EpiFunc", repository = "github", subdir = "DanielGardiner", load = FALSE)

###############################################################################


