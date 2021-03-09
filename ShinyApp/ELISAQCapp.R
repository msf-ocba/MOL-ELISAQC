##########################################
# LOAD REQUIRED LIBRARIES
##########################################


library(shiny)
library(rhandsontable)
library(data.table)
library(dplyr)


##########################################
# CREATE TABLE
##########################################

# Create the vectors for the empty plate:
Rows <- c("A", "B", "C", "D", "E", "F", "G", "H")
C01 <- c("", "", "", "", "", "", "", "")
C02 <- c("", "", "", "", "", "", "", "")
C03 <- c("", "", "", "", "", "", "", "")
C04 <- c("", "", "", "", "", "", "", "")
C05 <- c("", "", "", "", "", "", "", "")
C06 <- c("", "", "", "", "", "", "", "")
C07 <- c("", "", "", "", "", "", "", "")
C08 <- c("", "", "", "", "", "", "", "")
C09 <- c("", "", "", "", "", "", "", "")
C10 <- c("", "", "", "", "", "", "", "")
C11 <- c("", "", "", "", "", "", "", "")
C12 <- c("", "", "", "", "", "", "", "")


# Create the data.table:
dt <- data.table(C01, C02, C03, C04, C05, C06, C07, C08, C09, C10, C11, C12)


# Create the dropdown menu
controls <- c("Calibrateur", 
              "Cal 1 200 IU/mL",
              "Cal 2 100 IU/mL",
              "Cal 3 25 IU/mL",
              "Cal 4 5 IU/mL",
              "Témoin négatif kit", 
              "Témoin positif kit", 
              "Témoin négatif externe", 
              "Témoin positif externe") 

##########################################
# DEFINE UI
##########################################
    
ui <- fluidPage(
    
    # Add drop-down menu of ELISA kits:
    selectInput(inputId = "kit", 
                label = "Kit ELISA", 
                choices = c("Rougeole (IgM - NP)", 
                            "Rubéole (IgM - GP)", 
                           "Coqueluche (IgG - PT)"), 
                 selected = "Rougeole (IgM - NP)", 
                 multiple = FALSE), 

    # Create an empty plate template for entering sample IDs:
    rHandsontableOutput(outputId = "platemap"),
    
    # Add file up-loader for raw ELISA results:
    fileInput(inputId = "resultats", 
              label = "Résultats bruts du lecteur ELISA:", 
              multiple = FALSE,
              buttonLabel = "Rechercher...", 
              placeholder = "Dossier pas encore selectionné"), 
    
    # Add file up-loader for reference lab control results:
    fileInput(inputId = "refmaison", 
              label = "Valeurs de référence pour controles de lab ref:",
              multiple = FALSE, 
              buttonLabel = "Rechercher...",
              placeholder = "Dossier pas encore selectionné"),
    
    # Add input for ELISA operator:
    textInput(inputId = "operateur", 
              label = "Nom de l´operateur", 
              value = "laborantin"), 
    
    # Add input for calibrator reference value:
    numericInput(inputId = "calref", 
                 label = "Calibrateur: valeur de référence", 
                 value = 0.316), 
    
    # Add input for calibrator lower limit:
    numericInput(inputId = "calinf", 
                 label = "Calibrateur: limite inférieure", 
                 value = 0.14),
    
    # Add input for internal positive control reference value:
    numericInput(inputId = "ipcref", 
                 label = "Controle positif du kit: valeur de référence", 
                 value = 2.6),
    
    # Add input for internal positive control lower limit:
    numericInput(inputId = "ipcinf", 
                 label = "Controle positif du kit: limite inférieure", 
                 value = 1.4),
    
    # Add input for internal positive control upper limit:
    numericInput(inputId = "ipcsup", 
                 label = "Controle positif du kit: limite supérieure", 
                 value = 3.8),
    
    # Add input for internal negative control reference value:
    numericInput(inputId = "incref", 
                 label = "Controle négatif du kit: valeur de référence", 
                 value = 0.1),
    
    # Add input for internal negative control lower limit:
    numericInput(inputId = "incinf", 
                 label = "Controle négatif du kit: limite inférieure", 
                 value = 0),
    
    # Add input for internal negative control upper limit:
    numericInput(inputId = "incsup", 
                 label = "Controle négatif du kit: limite supérieure", 
                 value = 0.7),

    # Add report generator button:
    downloadButton(outputId = "rapport", label = "Générer rapport CQ")
    
)

##########################################
# DEFINE SERVER
##########################################

server <- function(input, output){
    
    
    # Create a reactive space to add data:
    plate <- reactiveValues(values = dt)
    
    # Render the table:
    output$platemap <- renderRHandsontable({
        
        # Create the rhandsontable
        rhandsontable(data = plate$values, rowHeaders = Rows) %>% 
            hot_col(col = c(1:12), type = "autocomplete", source = "controls", strict = FALSE)
    })
    
    
    # Update plate() on user changes
    observeEvent(eventExpr = input$platemap, {
        
        plate$values <- hot_to_r(input$platemap)
    })
    
    output$report <- downloadHandler(filename = paste0("ELISAQCrapport_", Sys.Date(), ".html"), 
                                     content = function(file) {
                                         
                                         # Copy file to temporary directory
                                         tempReport <- file.path(tempdir(), "ELISAQCrapport.Rmd")
                                         file.copy("ELISAQCrapport.Rmd", tempReport, overwrite = TRUE)
                                         
                                         # Set up parameters to pass to Rmd document
                                         params <- list(kit = input$kit, 
                                                        ids = plate$values, 
                                                        resultats = input$resultats, 
                                                        refmaison = input$refmaison, 
                                                        operateur = input$operateur, 
                                                        calref = input$calref, 
                                                        calinf = input$calinf, 
                                                        ipcref = input$ipcref, 
                                                        ipcinf = input$ipcinf, 
                                                        ipcsup = input$ipcsup, 
                                                        incref = input$incref, 
                                                        incinf = input$incinf, 
                                                        incsup = input$incsup) 
                                         
                                         # Knit the document:
                                         rmarkdown::render(input = tempReport, 
                                                           output_file = paste0("ELISAQCrapport_", 
                                                                                Sys.Date(), 
                                                                                ".html"),
                                                           params = params,
                                                           envir = new.env(parent = globalenv()))
                                     })
}

##########################################
# RUN APPLICATION
##########################################

# Run the application 
shinyApp(ui = ui, server = server, options = list(port=7990))
