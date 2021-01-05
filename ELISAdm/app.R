##########################################
# LOAD REQUIRED LIBRARIES
##########################################


library(shiny)
library(rhandsontable)
library(data.table)


##########################################
# CREATE TABLE
##########################################

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

plate <- data.table(Rows, C01, C02, C03, C04, C05, C06, C07, C08, C09, C10, C11, C12)




##########################################
# DEFINE UI
##########################################

ui <- fluidPage(
    
    # Create the handle for the empty table:
    rHandsontableOutput(outputId = "template"),
    
    # Create the handle for the heat map of results:
    plotOutput(outputId = "platemap")
)

##########################################
# DEFINE SERVER
##########################################

server <- function(input, output){
    
    # Create a reactive space to add data:
    data.in <- reactiveValues(values = plate)
    
    # Render the table:
    output$template <- renderRHandsontable({
        rhandsontable(data.in$values)
    })
    
    # Facilitate input of values:
    observeEvent(eventExpr = input$template, {
        
        # Save values to an R object:
        data.in$values <- hot_to_r(input$template)
        output$platemap <- renderPlot({
            if(!is.null(tryCatch(plot(data.in$values), error = function(e){})))
            {plot(data.in$values)}
        })
    })
}

##########################################
# RUN APPLICATION
##########################################

# Run the application 
shinyApp(ui = ui, server = server, options = list(port=7990))
