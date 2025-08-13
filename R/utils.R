#!/usr/bin/env Rscript

library(shiny)
library(data.table)
library(dplyr)
library(stringr) # For str_extract

# ============================ Helper Functions =============================

#' Create a shiny action button for a DT table
#'
#' @param FUN The shiny function to call (e.g., actionButton)
#' @param id The base ID for the buttons
#' @param ... Other arguments to pass to the shiny function
#' @return A character vector of HTML for the buttons
shinyInput <- function(FUN, id, ...) {
  inputs <- character(length(id))
  for (i in seq_along(id)) {
    inputs[i] <- as.character(FUN(paste0(id, i), ...))
  }
  inputs
}


#' Function to calculate various metrics for a single cell's time course data
#'
#' This function takes a vector of fluorescence values and a time vector,
# ... existing code ...
