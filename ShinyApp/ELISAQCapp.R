##########################################
# LOAD REQUIRED LIBRARIES
##########################################


library(shiny)
library(rhandsontable)
library(data.table)


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
    
    # Create the handle for the empty table:
    rHandsontableOutput(outputId = "platemap")
    
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
    
}

##########################################
# RUN APPLICATION
##########################################

# Run the application 
shinyApp(ui = ui, server = server, options = list(port=7990))
