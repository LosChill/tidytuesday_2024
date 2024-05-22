library(tidyverse)
library(caret)

# One-hot function
one_hot_encode <- function(data, column) {
  
  # Allow for column names as char strings
  column <- sym(column)
  
  # Create segment tibble 
  data_column <- data %>%
    select(submission_id, !!column)
  
  # Split multi-value cells into rows
  data_long <- data_column %>%
    separate_rows(!!column, sep = ", ") %>%
    mutate(!!column := str_replace_all(!!column, " ", "_"))
  
  # Create dummy variables using dummyVars
  dmy <- dummyVars(paste("~", quo_name(column)), data = data_long)
  encoded_data <- predict(dmy, newdata = data_long) %>%
    as_tibble() %>%
    mutate(submission_id = data_long$submission_id)
  
  # Make column names "." separated
  colnames(encoded_data) <- colnames(encoded_data) %>%
    str_replace_all(paste0("^", quo_name(column)), paste0(quo_name(column), "."))
  
  # Aggregate one-hot encoded values by id
  encoded_data_aggregated <- encoded_data %>%
    group_by(submission_id) %>%
    summarise(across(everything(), max)) %>%
    ungroup()
  
  # Replace original column with new one-hot columns
  result <- data %>%
    select(-!!column) %>%
    left_join(encoded_data_aggregated, by = "submission_id")
  
  return(result)
}