library(XML)

#Extract information from expert XML
extract_info <- function(xml_node) {
  annotation <- xmlValue(xml_node[["name"]])
  xmin <- as.numeric(xmlValue(xml_node[["bndbox"]][["xmin"]]))
  xmax <- as.numeric(xmlValue(xml_node[["bndbox"]][["xmax"]]))
  ymin <- as.numeric(xmlValue(xml_node[["bndbox"]][["ymin"]]))
  ymax <- as.numeric(xmlValue(xml_node[["bndbox"]][["ymax"]]))
  data.frame(annotation, xmin, xmax, ymin, ymax)
}

#Extract information from users XML
extract_info_user <- function(xml_node, user) {
  annotation <- xmlValue(xml_node[["name"]])
  xmin <- as.numeric(xmlValue(xml_node[["bndbox"]][["xmin"]]))
  xmax <- as.numeric(xmlValue(xml_node[["bndbox"]][["xmax"]]))
  ymin <- as.numeric(xmlValue(xml_node[["bndbox"]][["ymin"]]))
  ymax <- as.numeric(xmlValue(xml_node[["bndbox"]][["ymax"]]))
  data.frame(user_name = user, annotation, xmin, xmax, ymin, ymax)
}
