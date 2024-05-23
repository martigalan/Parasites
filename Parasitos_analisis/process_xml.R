library(XML)
source("extract_info.R")

#Process experts XML
process_xml_experts <- function(xml_file) {
  doc <- xmlParse(xml_file)
  objects <- xpathApply(doc, "//object", extract_info)
  expert_corrections <- do.call(rbind, objects)
  colnames(expert_corrections) <- c("anotacion", "xmin", "xmax", "ymin", "ymax")
  expert_corrections$imagen <- basename(xml_file)
  return(expert_corrections)
}

#Process users XML
process_xml_user <- function(xml_file) {
  user <- str_extract(xml_file, "(?<=experimento_)(.*?)(?=_)")
  doc <- xmlParse(xml_file)
  objects <- xpathApply(doc, "//object", extract_info_user, user)
  users_annotations <- do.call(rbind, objects)
  colnames(users_annotations) <- c("user_name", "anotacion", "xmin", "xmax", "ymin", "ymax")
  users_annotations$imagen <- basename(xml_file)
  return(users_annotations)
}
