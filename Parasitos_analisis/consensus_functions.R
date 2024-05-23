library(tidyr)
library(purrr)
library(usedist)

#Consensus df calculation looking for clusters; each cluster will be an annotation!
calculate_clusters <- function(df) {
  unique_images <- unique(df$imagen)
  cluster_df <- data.frame(imagen = character(), clusters = integer(), xmin = numeric(), xmax = numeric(), ymin = numeric(), ymax = numeric(), anotacion = character(), stringsAsFactors = FALSE)
  for (image in unique_images) {
    image_coordinates <- df[df$imagen == image, ]
    
    coords <- as.matrix(image_coordinates[, c("xmin", "xmax", "ymin", "ymax")])
    labels_coords <- as.matrix(image_coordinates$anotacion)
    iou_distance = function(x, y) 1 - calculate_iou(x, y) #calculo distancia (al revés de IOU)
    label_distance = function(x, y) x != y #etiqueta será TRUE(1) cuando sea igual
    dist_matrix = usedist::dist_make(coords, iou_distance) #Obtengo distancia matrices
    label_matrix <- usedist::dist_make(labels_coords, label_distance) #si son distintas, TRUE(1), si son iguales (0) (etiquetas)
    dist_matrix[which(label_matrix == 1, arr.ind = TRUE)] = 1 #asigna valor 1 cuando labels distintas
    consensus_clust = hclust(dist_matrix)
    cut <- 0.60
    cluster_points <- cutree(consensus_clust, h = cut) #
    #anotaciones consenso (nº clusters por debajo del corte)
    num_clusters <- max(cluster_points)
    cluster_df$clusters[cluster_df$imagen == image] <- num_clusters
    
    #Añade las filas correspondientes por cada cluster
    for (i in 1:num_clusters) {
      clust_index <- which(cluster_points == i)
      if(length(clust_index) > 0) {
        #Encuentra la etiqueta más frecuente para el cluster
        cluster_labels <- labels_coords[clust_index]
        most_frequent_label <- names(which.max(table(cluster_labels)))
        #Añade cada punto del cluster con la etiqueta más frecuente
        for (index in clust_index) {
          one_coord <- coords[index, ]
          cluster_df <- rbind(cluster_df, data.frame(imagen = image, clusters = i, xmin = one_coord["xmin"], xmax = one_coord["xmax"], ymin = one_coord["ymin"], ymax = one_coord["ymax"], anotacion = most_frequent_label))
        }
      }
    }
    plot(consensus_clust, main = paste("Dendrogram for the image", image))
    abline(h = cut, col = "red")
  }
  names(cluster_df) <- c("imagen", "cluster", "xmin", "xmax", "ymin", "ymax", "anotacion")
  return(cluster_df)
}

#Calculate matrix IOU with cluster_df 
calculate_matrixIOU <- function(df) {
  max_values <- data.frame(imagen = character(), cluster = numeric(), max_iou = numeric(), xmin = numeric(), xmax = numeric(), ymin = numeric(), ymax = numeric(), anotacion = character(), number_users = numeric(), stringsAsFactors = FALSE)
  unique_images <- unique(df$imagen)
  
  for (image in unique_images) {
    image_annotations <- df[df$imagen == image, ]
    for (cluster_id in unique(image_annotations$cluster)){
      cluster_annotations = image_annotations[image_annotations$cluster == cluster_id, ]
      n_annotations <- nrow(cluster_annotations)
      
      #Si sólo hay una anotación, no calcular IoU (añado NA)
      if (n_annotations <= 1) {
        if (n_annotations == 0 || is.na(cluster_annotations$xmin[1])) {
          next  
        } else {
          result_row <- data.frame(imagen = image, cluster = cluster_id, max_iou = NA, xmin = cluster_annotations$xmin, xmax = cluster_annotations$xmax, ymin = cluster_annotations$ymin, ymax = cluster_annotations$ymax, anotacion = cluster_annotations$anotacion, number_users = n_annotations, stringsAsFactors = FALSE)
          max_values <- rbind(max_values, result_row)
          next
        }
      }
      
      annotation_subset <- as.matrix(cluster_annotations[, c("xmin", "xmax", "ymin", "ymax")])
      iou_matrix <- matrix(0, nrow = n_annotations, ncol = n_annotations)
      
      for (i in 1:n_annotations) {
        for (j in 1:n_annotations) {
          if (i != j) {
            box1 <- annotation_subset[i, ]
            box2 <- annotation_subset[j, ]
            iou_matrix[i, j] <- calculate_iou(box1, box2)
          } else {
            iou_matrix[i, j] <- 1  #IoU consigo mismo es 1
          }
        }
      }
      
      row_sums <- rowSums(iou_matrix)
      row_changed <- row_sums - 1
      iou_means <- row_changed / (n_annotations - 1)
      max_iou <- max(iou_means)
      max_iou_index <- which(iou_means == max_iou)[1]  
      
      representative_annotation <- cluster_annotations[max_iou_index, ]
      result_row <- data.frame(imagen = image, cluster = cluster_id, max_iou = max_iou, xmin = representative_annotation$xmin, xmax = representative_annotation$xmax, ymin = representative_annotation$ymin, ymax = representative_annotation$ymax, anotacion = representative_annotation$anotacion, number_users = n_annotations, stringsAsFactors = FALSE)
      max_values <- rbind(max_values, result_row)
    }
  }
  return(max_values)
}

#1st consensus strategy
min_person = 4; 
generate_consensus_df <- function(max_values, min_person) {
  #Filtrar por nº min de personas
  max_values_filtered <- max_values[max_values$number_users >= min_person, ]
  
  consensus_df <- data.frame(imagen = character(), max_iou = numeric(), xmin = numeric(), 
                             xmax = numeric(), ymin = numeric(), 
                             ymax = numeric(), number_users = numeric(),
                             stringsAsFactors = FALSE)
  
  unique_images <- unique(max_values_filtered$imagen)
  for (image in unique_images) {
    image_data <- max_values_filtered[max_values_filtered$imagen == image, ]
    #calcular consenso
    if (nrow(image_data) > 1) {
      consensus_row <- calculate_image_consensus(image_data, min_person)
      consensus_df <- rbind(consensus_df, consensus_row)
    } else {
      consensus_df <- rbind(consensus_df, image_data)
    }
  }
  
  return(consensus_df)
}

calculate_image_consensus <- function(image_data, min_person) {
  valid_clusters <- image_data[image_data$number_users >= min_person & image_data$max_iou > 0.4, ]
  if (nrow(valid_clusters) == 0) {
    next
  }
  #Agrupar por tipo de anotación
  grouped_clusters <- valid_clusters %>% 
    group_by(anotacion)
  #Selecciono, cuando tienen la misma anotacion, el que tenga el max_iou
  consensus_clusters <- grouped_clusters %>% 
    slice(which.max(max_iou))
  
  return(consensus_clusters)
}

#2nd consensus strategy
process_annotations <- function(df) {
  processed_df <- df[0, ]
  grouped_data <- split(df, df$imagen)
  
  for (image_data in grouped_data) {
    if (nrow(image_data) > 1) {
      #Inicializar un vector para marcar qué anotaciones se incluirán
      include <- rep(TRUE, nrow(image_data))
      #Crear matriz de solapamientos
      for (i in 1:(nrow(image_data) - 1)) {
        for (j in (i + 1):nrow(image_data)) {
          if (do_overlap(image_data[i,], image_data[j,])) {
            #Hay solapamientos, marcar las anotaciones con menos usuarios para no incluir
            if (image_data[i, "number_users"] < image_data[j, "number_users"]) {
              include[i] <- FALSE
            } else {
              include[j] <- FALSE
            }
          }
        }
      }
      #Incluir solo las anotaciones marcadas como 'TRUE' en 'include'
      processed_df <- rbind(processed_df, image_data[include, ])
    } else {
      #Solo una fila, añadir directamente
      processed_df <- rbind(processed_df, image_data)
    }
  }
  return(processed_df)
}

do_overlap <- function(box1, box2) {
  #Comprobar si hay NAs en las coordenadas
  if (any(is.na(c(box1$xmin, box1$xmax, box1$ymin, box1$ymax, box2$xmin, box2$xmax, box2$ymin, box2$ymax)))) {
    return(FALSE)  #O considerar otra lógica según el contexto
  }
  #Comprobar solapamiento
  if (box1$xmin > box2$xmax || box2$xmin > box1$xmax) {
    return(FALSE)
  }
  if (box1$ymin > box2$ymax || box2$ymin > box1$ymax) {
    return(FALSE)
  }
  return(TRUE)
}