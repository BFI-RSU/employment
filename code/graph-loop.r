# Code for generating all graphs in the Enterprise section of the IDBR report

# Load necessary libraries
library(readxl)
library(dplyr)
library(tidyr)
library(plotly)

# Define data preparation function
prepare_enterprise_data <- function(file_path, sheet_name) {
  
  # Import data from specific sheet
  enterprises <- read_xlsx(file_path, sheet = sheet_name)
  
  # Clean column names
  colnames(enterprises) <- gsub("[A-Z]\\d+\\s?:\\s?", "", colnames(enterprises))
  
  # Put into tidy format and classify by nation/region
  enterprises <- enterprises %>%
    mutate(across(-c(Year, Sector, Type), as.numeric)) %>%
    pivot_longer(cols = -c(Year, Sector, Type),
                 names_to = "Place", values_to = "Values") %>%
    mutate(Place_Type = if_else(
      Place %in% c("England", "Scotland", "Wales", "Northern Ireland", "UK"),
      "Nation",
      "Region"
    ))
  
  # Return the cleaned dataframe
  return(enterprises)
}

# Define a function to generate the plot for a specific sector
generate_sector_plot <- function(enterprises, sector_name) {
  
  # Filter the data for the specific sector
  sector_data <- enterprises %>%
    filter(
      Sector == sector_name,
      Type == "Count"
    )
  
  # Fix date for graphing purposes 
  sector_data <- sector_data %>%
    mutate(Year = as.Date(paste0(Year, "-01-01")))
  
  # Create Plotly plot
  sector_plot <- plot_ly(sector_data, 
                         x = ~Year, 
                         y = ~Values, 
                         color = ~Place,
                         type = 'scatter', 
                         mode = 'lines+markers', 
                         text = ~paste("Year:", format(Year, "%Y"),
                                       "<br>Nation/region:", Place, 
                                       "<br>Count of enterprises:", format(Values, big.mark = ",", scientific = FALSE)), 
                         hoverinfo = 'text'
  ) %>%
    layout(
      # title = paste(sector_name),  # Dynamic title for each sector
      xaxis = list(
        title = "",
        tickformat = "%Y",  # Format to show only the year
        dtick = "M12",      # Show every year on the x-axis
        tickmode = "auto"   # Dynamically adjust based on zoom level
      ),
      yaxis = list(
        title = "Count of enterprises",
        rangemode = "tozero",  # Ensure the y-axis starts at 0
        fixedrange = FALSE     # Allow dynamic zooming
      ),
      legend = list(
        orientation = "h", 
        x = 0.5, 
        xanchor = "center", 
        y = -0.2, 
        yanchor = "top",
        title = list(text = "Nation/region")
      ),
      font = list(
        family = "Source Sans Pro",  # Use "Source Sans Pro" font throughout
        size = 12,
        color = "black"
      ),
      shapes = list(
        list(
          type = "rect",
          x0 = "2020-03-23", x1 = "2021-07-19",  # Shaded region start and end dates
          y0 = 0, y1 = 1,  # Cover the entire y-axis
          xref = "x", yref = "paper",  # Refer to x as time, y as paper height
          fillcolor = "lightblue", opacity = 0.3, line = list(width = 0)
        )
      ),
      annotations = list(
        list(
          x = "2020-09-23",  # Adjust date based on preferred alignment
          y = 0.05,  # Set positioning based on proportion of y-axis height
          text = "COVID-19<br>pandemic",  # Annotation text
          showarrow = FALSE,  # No arrow pointing to the annotation
          xref = "x", yref = "paper",  # Reference the x-axis for date, y-axis for height
          font = list(color = "black", size = 12),
          align = "left"
        )
      )
    )
  
  # Return the Plotly object
  return(sector_plot)
}