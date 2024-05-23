library(XML)
library(stringr)
library(dplyr)
library(e1071)
library(tidyr)
library(purrr)
library(usedist)
library(ggplot2)

setwd("C:\\Users\\marty\\Documents\\CEU 4º AÑO\\2º CUATRI\\Parasite_Analysis")
source("extract_info.R")
source("process_xml.R")
source("process_generated_folders.R")
source("calculate_iou.R")
source("consensus_functions.R")


#Process users anotations
program_directory <- "D:/ParasitosLocal"
generated_results <- process_generated_folders(program_directory)
users_df <- do.call(rbind, generated_results)

#Process experts anotations
experts_directory <- "C:/Users/marty/eclipse-workspace/Parasitos_Local/resources"
xml_files_experts <- list.files(path = experts_directory, pattern = "\\.xml$", full.names = TRUE)
experts_annotations <- do.call(rbind, lapply(xml_files_experts, process_xml_experts))

#Compare anotations experts vs users
users_df$Validez <- "No Valida"
for (i in 1:nrow(users_df)) {
  image_users <- users_df$imagen[i]
  anotac_users <- trimws(users_df$anotacion[i])
  actual_annotation <- tolower(anotac_users)
  anotac_experts <- experts_annotations$anotacion[experts_annotations$imagen == image_users]
  actual_experts_annotation <- tolower(anotac_experts)
  if (actual_annotation %in% actual_experts_annotation) {
    users_df$Validez[i] <- "Valida"
  }
}

#Calculate IoU for only good annotate images
users_df$IOU <- NA
for (i in 1:nrow(users_df)) {
  if (users_df$Validez[i] == "Valida") {
    actual_image <- users_df$imagen[i]
    anotac_experts <- experts_annotations[experts_annotations$imagen == actual_image, ]
    anotac_users <- users_df[i, c("xmin", "xmax", "ymin", "ymax")]
    iou_list <- list()
    for (j in 1:nrow(anotac_experts)) {
      iou <- calculate_iou(anotac_experts[j, c("xmin", "xmax", "ymin", "ymax")], anotac_users)
      iou_list[[j]] <- iou
    }
    iou_vector <- unlist(iou_list)
    max_iou <- max(iou_vector)
    users_df$IOU[i] <- max_iou
  }
}

# Histograma para IoU
# hist(users_df$IOU, main = "Histogram: IOU", xlab = "IOU", ylab = "Frequency", breaks = 15)
threshold_IOU <- 0.4

# Calcular aciertos y fallos para cada usuario
users_df$Aciertos <- 0
users_df$Fallos <- 0
for (i in 1:nrow(users_df)) {
  if (users_df$Validez[i] == "Valida" && users_df$IOU[i] >= threshold_IOU) {
    users_df$Aciertos[i] <- 1
  } else {
    users_df$Fallos[i] <- 1
  }
}

individual_summary <- users_df %>%
  group_by(user_name) %>%
  summarise(total_hits = sum(Aciertos), total_failures = sum(Fallos), IoU = mean(IOU[Aciertos > 0], na.rm = TRUE)) %>%
  mutate(accuracy = total_hits / (total_hits + total_failures)) %>%
  arrange(desc(accuracy))

summary_byImage <- users_df %>%
  group_by(imagen) %>%
  summarise(total_hits = sum(Aciertos), total_failures = sum(Fallos), IoU = mean(IOU[Aciertos > 0], na.rm = TRUE)) %>%
  mutate(accuracy = total_hits / (total_hits + total_failures)) %>%
  arrange(desc(accuracy))

#Calculate consensus
allLabels_df <- users_df %>%
  group_by(imagen, anotacion) %>% 
  summarise(
    xmin = list(xmin), #saco una lista para tener todos los valores
    xmax = list(xmax),
    ymin = list(ymin),
    ymax = list(ymax),
    .groups = 'drop'
  ) %>%
  unnest(cols = c(xmin, xmax, ymin, ymax))
cluster_df <- calculate_clusters(allLabels_df)
max_values <- calculate_matrixIOU(cluster_df)

#Paint clusters
plot_df = cluster_df[cluster_df$imagen == "00000086.xml",]
annotation_colors <- c("Amebae" = "brown", "Ascaris lumbricoides" = "yellow", "Giardia Duodenalis" = "purple", "Hymenolepsis Nana" = "gray", "Trichuris trichiura" = "cyan")
ggplot(plot_df, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, col=factor(cluster), fill=anotacion)) +
  geom_rect(linewidth=1.5, alpha=0.2) + scale_fill_manual(values = annotation_colors) +  labs(color = "Cluster", fill = "Anotacion") 
ggplot( max_values[max_values$imagen == "00000074.xml",], aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, col=factor(cluster), fill=anotacion)) +
  geom_rect(linewidth=1.5, alpha=0.2) + scale_fill_manual(values = annotation_colors) +  labs(color = "Cluster", fill = "Anotacion") 

#1st consensus strategy
consensus_df <- generate_consensus_df(max_values, min_person)

#Calculate accuracy
#On the one hand, calculate successes 1 at a time -> accuracy -> extract it separately (hits and failures)
consensus_df$Validez <- "No Valida" 
for (i in 1:nrow(consensus_df)) {
  image_consensus <- consensus_df$imagen[i]
  anotac_consensus <- trimws(consensus_df$anotacion[i])
  actual_annotation <- tolower(anotac_consensus)
  anotac_experts <- experts_annotations$anotacion[experts_annotations$imagen == image_consensus]
  actual_experts_annotation <- tolower(anotac_experts)
  
  if (actual_annotation %in% actual_experts_annotation) {
    consensus_df$Validez[i] <- "Valida"
  }
}

hits_failures_consensus <- consensus_df %>%
  group_by(imagen) %>%
  summarise(
    Aciertos = sum(Validez == "Valida"),  
    Fallos = sum(Validez == "No Valida")
  ) %>% 
  mutate(accuracy_1 = Aciertos / (Aciertos + Fallos))

hits_consensus <- hits_failures_consensus$Aciertos
failures_consensus <- hits_failures_consensus$Fallos
total_aciertos <- sum(hits_consensus)
total_fallos <- sum(failures_consensus)

#2nd consensus strategy
solaped_consensus_df <- process_annotations(consensus_df)

#I check that the overlaps are taken into account and I get more than 1 annotation when they are not
plot_solaped_df = solaped_consensus_df[solaped_consensus_df$imagen == "00000074.xml",]
annotation_colors <- c("Amebae" = "brown", "Ascaris lumbricoides" = "yellow", "Giardia Duodenalis" = "purple", "Hymenolepsis Nana" = "gray", "Trichuris trichiura" = "cyan")
ggplot(plot_solaped_df, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, col=factor(cluster), fill=anotacion)) +
  geom_rect(linewidth=1.5, alpha=0.2) + scale_fill_manual(values = annotation_colors) +  labs(color = "Cluster", fill = "Anotacion") 

solaped_consensus_df$Validez <- "No Valida" 
for (i in 1:nrow(solaped_consensus_df)) {
  image_consensus <- solaped_consensus_df$imagen[i]
  anotac_consensus <- trimws(solaped_consensus_df$anotacion[i])
  actual_annotation <- tolower(anotac_consensus)
  anotac_experts <- experts_annotations$anotacion[experts_annotations$imagen == image_consensus]
  actual_experts_annotation <- tolower(anotac_experts)
  
  if (actual_annotation %in% actual_experts_annotation) {
    solaped_consensus_df$Validez[i] <- "Valida"
  }
}

hits_failures_consensus_solaped <- solaped_consensus_df %>%
  group_by(imagen) %>%
  summarise(
    Aciertos = sum(Validez == "Valida"),  
    Fallos = sum(Validez == "No Valida")
  ) %>% 
  mutate(accuracy_2 = Aciertos / (Aciertos + Fallos))

hits_consensus_solaped <- hits_failures_consensus_solaped$Aciertos
failures_consensus_solaped <- hits_failures_consensus_solaped$Fallos
total_aciertos_solaped <- sum(hits_consensus_solaped)
total_fallos_solaped <- sum(failures_consensus_solaped)

#----------------------------------------------------

#Individual mean accuracy and both consensus strategies
accuracy_medio_users <- mean(individual_summary$accuracy, na.rm = TRUE) #0.5477089
accuracy_medio_consensus <- total_aciertos / (total_aciertos + total_fallos) #0.5789474
accuracy_medio_solaped <- total_aciertos_solaped / (total_aciertos_solaped + total_fallos_solaped) #0.5952381

#I calculate average accuracy of both consensus strategies (paper table)
combined_accuracies <- merge(hits_failures_consensus, hits_failures_consensus_solaped, by = "imagen", suffixes = c("_1", "_2"))

combined_accuracies <- combined_accuracies %>%
  mutate(mean_accuracy = (accuracy_1 + accuracy_2) / 2)
