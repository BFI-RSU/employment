library(readxl)
library(dplyr)
library(tidyr)
library(reactable)

# Define the function
create_employment_table <- function(file_path, sheet_name) {
  # Import data from the specified sheet and pivot to long format
  employment <- read_xlsx(file_path, sheet = sheet_name, skip = 1) %>%
    mutate(across(everything(), as.numeric)) %>%
    pivot_longer(
      !Year,
      names_to = "Sector",
      values_to = "Employment"
    )

  # Prep data for Table 1
  employment_table1 <- employment %>%
    pivot_wider(
      names_from = Year,
      values_from = Employment
    ) %>%
    select(where(~ !all(is.na(.)))) %>%
    filter(
      !Sector %in% c(
        "Film and video production self-employed",
        "Total"
      )
    )

  # Select the last three years of data
  employment_table1 <- employment_table1 %>%
    select(1, (ncol(employment_table1) - 2):ncol(employment_table1))

  # Get the names of the last three columns dynamically
  last_three_colnames <- names(employment_table1)[(ncol(employment_table1) - 2):ncol(employment_table1)]

  # Create a list to hold the column definitions
  column_defs <- list()

  # Add the Sector column definition
  column_defs[["Sector"]] <- colDef(
    footer = "Total",
    format = colFormat(separators = TRUE),
    footerStyle = list(fontWeight = "bold")
  )

  # Loop through the last three columns to apply the same styling
  for (colname in last_three_colnames) {
    column_defs[[colname]] <- colDef(
      footer = function(values) {
        format(sum(values), big.mark = ",")
      },
      format = colFormat(separators = TRUE, digits = 0),
      footerStyle = list(fontWeight = "bold")
    )
  }

  # Create and return the reactable
  reactable(
    employment_table1,
    striped = TRUE,
    highlight = TRUE,
    compact = TRUE,
    columns = column_defs
  )
}

# Usage example
# employment_table <- create_employment_table(here::here("data/APS.xlsx"), "Employment")