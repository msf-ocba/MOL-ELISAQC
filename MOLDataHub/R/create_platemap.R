#' Create heat-map of ELISA plate
#' 
#' @author Amy Mikhail, \email{amy.mikhail@@gmail.com}
#' 
#' @description 
#' This function creates a heat-map of a normalised ELISA plate.  It 
#' facilitates visualisation and checking for cross-contamination.  
#' 
#' @param elisadata Data.table of calculated ELISA results
#' 
#' @returns Heat-map of ELISA plate
#' 
#' @import data.table ggplot2
#' 
#' @examples 
#'  \dontrun{
#'  # Create QC table:
#' plate <- create_platemap(elisadata = elisadt)
#'  }
#' @export
create_platemap <- function(elisadata){
  
  # Name data.table:
  elisadata = elisadata
  
  if(!is.numeric(elisadata$Value)){
    
    # Convert non-numeric elements of Value column back to numeric:
    elisadata[Value == "> 200", Value := "200"]
    
    # Convert column back to numeric:
    elisadata[, Value := as.numeric(Value)]
    
  }
  
  # Convert Row and column labels to factors:
  elisadata[, Rowlabs := factor(Rows)]
  elisadata[, Colabs := factor(Cols)]
  
  # Create heat-map of plate:
  platemap = ggplot(elisadata, aes(x = Colabs, y = ordered(Rowlabs, levels = rev(levels(Rowlabs))))) + 
    geom_tile(fill = "white", na.rm = TRUE) +
    geom_point(aes(colour = Value, size = 200)) +
    scale_size_continuous(range = c(10, 12)) +
    guides(size = FALSE) +
    scale_color_gradient(low = "lightblue", high = "darkblue", na.value = "lightgrey") + 
    ylab("") +
    xlab("") +
    scale_x_discrete(position = "top") + 
    theme(axis.title.x = element_text(family = "sans", size = 14, margin = margin(0,0,20,0)), 
          axis.title.y = element_text(family = "sans", size = 14, margin = margin(0,20,0,0)), 
          axis.text = element_text(family = "sans", size = 12),
          legend.title = element_text(family = "sans", size = 12, face = "bold", margin = margin(0,0,2.5,0)), 
          legend.text = element_text(family = "sans", size = 10),
          plot.title = element_text(family = "sans", size = 16, face = "bold", margin = margin(0,0,5,0)))
  
  
  # Return platemap:
  return(platemap)
  
}