library(shiny)
require(shinydashboard)
library(ggplot2)
library(dplyr)
library(DT)

# Directory that will save the data
# I need to create these directories in the same folder as Server.R
outputDir <- "responses"
rmDir <- "rm"
asDir <- "as"
arDir <- "ar"
rcDir <- "rc"
csDir <- "cs"
eaDir <- "ea"
rn1Dir <- "rn1"

# Saving the data
saveData <- function(input, fn, folder) {
  #fn is the dataframe stored in data.R
  #folder is either responses or rm
  # put variables in a data frame
  data <- data.frame(matrix(nrow=1,ncol=0))
  for (x in fn) {
    var <- input[[x]]
    if (length(var) > 1 ) {
      # handles lists from checkboxGroup and multiple Select
      data[[x]] <- list(var)
    } else {
      # all other data types
      data[[x]] <- var
    }
  }
  data$submit_time <- date()

  # Create a unique file name
  fileName <- sprintf(
    "%s_%s.rds",
    as.integer(Sys.time()),
    digest::digest(data)
  )

  # Write the file to the local system
  saveRDS(
    object = data,
    file = file.path(folder, fileName)
  )
}


loadData <- function(fn, folder) {
  # read all the files into a list
  files <- list.files(folder, full.names = TRUE)

  if (length(files) == 0) {
    # create empty data frame with correct columns
    field_list <- c(fn, "submit_time")
    data <- data.frame(matrix(ncol = length(field_list), nrow = 0))
    names(data) <- field_list
  } else {
    data <- lapply(files, function(x) readRDS(x))

    # Concatenate all data together into one data.frame
    data <- do.call(rbind, data)
  }

  data
}

deleteData <- function(folder) {
  # Read all the files into a list
  files <- list.files(folder, full.names = TRUE)

  lapply(files, file.remove)
}
