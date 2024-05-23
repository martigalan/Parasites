library(stringr)
library(dplyr)
source("process_xml.R")

#Process generated folders for user annotations
process_generated_folders <- function(folder_path) {
  generated_folders <- list.dirs(path = folder_path, full.names = TRUE, recursive = FALSE)
  generated_folders <- generated_folders[str_detect(generated_folders, "generated$")]
  results <- list()
  for (folder in generated_folders) {
    xml_files <- list.files(path = folder, pattern = "\\.xml$", full.names = TRUE)
    users_annotations <- do.call(rbind, lapply(xml_files, process_xml_user))
    results[[folder]] <- users_annotations
  }
  return(results)
}
