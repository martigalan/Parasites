library(tidyverse)

calculate_iou <- function(box1, box2) {
  #Calcular coordenadas del Area de interseccion
  x_left <- max(box1[1], box2[1])
  y_top <- max(box1[3], box2[3])
  x_right <- min(box1[2], box2[2])
  y_bottom <- min(box1[4], box2[4])
  #Calcular el Area de interseccion
  intersection_area <- max(0, x_right - x_left + 1) * max(0, y_bottom - y_top + 1)
  #Calcular el Area de las bounding boxes individuales
  box1_area <- (box1[2] - box1[1] + 1) * (box1[4] - box1[3] + 1)
  box2_area <- (box2[2] - box2[1] + 1) * (box2[4] - box2[3] + 1)
  #Calcular el Area de union
  union_area <- box1_area + box2_area - intersection_area
  #Calcular IoU
  iou <- intersection_area / union_area

  return(iou)
}

evaluate_annotations <- function(ground_truth, user_predictions) {
  iou_threshold  <- 0.5
  # Initialize counters
  tp <- 0 #true prediction
  fp <- 0 #false prediction
  fn <- 0 #false negative (not gt detected)

  unmatched_predictions <- rep(TRUE, nrow(user_predictions))
  # Iterate over ground truth annotations
  for (i in seq_len(nrow(ground_truth))) {
    gt_box <- ground_truth[i, c("xmin", "xmax", "ymin", "ymax")]  # Extract ground truth bounding box
    gt_annotation <- ground_truth[i, "anotacion"]  # Extract ground truth annotation

    # Initialize a flag to track if the ground truth object was detected
    detected <- FALSE

    # Iterate over predicted bounding boxes
    for (j in seq_len(nrow(user_predictions))) {
      if (!unmatched_predictions[j]) {
        next
      }
      pred_box <- user_predictions[j, c("xmin", "xmax", "ymin", "ymax")]  # Extract predicted bounding box
      pred_annotation <- user_predictions[j, "anotacion"]  # Extract predicted annotation

      # Calculate IoU between ground truth and predicted bounding boxes
      iou <- calculate_iou(gt_box, pred_box)

      # Check if the IoU exceeds a threshold (e.g., 0.5)
      if (iou >= iou_threshold) {
        detected <- TRUE  # Set the flag to indicate the ground truth object was detected
        unmatched_predictions[j] <- FALSE  # Mark the prediction as matched

        # Check if the annotations match
        if (gt_annotation == pred_annotation) {
          tp <- tp + 1  # Increment true positive count
          break  # Exit the inner loop since the object was detected
        } else {
          fp <- fp + 1  # Increment false positive count for mismatched label
        }
      }
    }

    # If the ground truth object was not detected, increment false negative count
    if (!detected) {
      fn <- fn + 1
    }
  }

  # Increment false positive count for unmatched predictions
  fp <- fp + sum(unmatched_predictions)
  list("tp" = tp, "fp" = fp, "fn" = fn)
}


run_test <- function(imagename, username) {
  ground_truth <- dplyr::filter(experts, imagen == imagename)
  user_predictions <- dplyr::filter(users, imagen == imagename, user_name == username)
  scores <- evaluate_annotations(ground_truth = ground_truth, user_predictions = user_predictions)

  print(ground_truth)
  print(user_predictions)
  print(scores)

  ground_truth$type = "GT"
  user_predictions$type = "user"

  common_cols <- intersect(colnames(ground_truth), colnames(user_predictions))
  print(ggplot(
    rbind(ground_truth[, common_cols], user_predictions[, common_cols]),
    aes(xmin = xmin, xmax=xmax, ymin=ymin, ymax=ymax, color=anotacion, linetype=type)
  ) + geom_rect(alpha=0.2)
  )

  # just to prepare for the next test
  print(unique(users$imagen))
  print(unique(users$user_name))
}



# -------------------------------------------------------------------------
experts <- experts_annotations
users <- users_df

scores <- list()
count <- 1
for (imagename in unique(experts$imagen)) {
  ground_truth <- dplyr::filter(experts, imagen == imagename)
  for (username in unique(users$user_name)) {
    user_predictions <- dplyr::filter(users, imagen == imagename, user_name == username)
    current_scores <- evaluate_annotations(ground_truth, user_predictions)
    current_scores$user_name = username
    current_scores$imagen = imagename
    scores[[count]] <- as.data.frame(current_scores)
    count <- count + 1
  }
}

scores_df <- as_tibble(do.call(rbind, scores))
scores_by_user <- scores_df %>% group_by(user_name) %>%
  summarise(
    precision = sum(tp) / (sum(tp) +sum(fp)),
    recall = sum(tp) / (sum(tp) +sum(fn)),
    f1 = 2 * (precision * recall) / (precision + recall)
  )

cat("***** scores by user *****\n")
print(scores_by_user)
cat("***** mean scores *****\n")
print(
  scores_by_user %>% summarise(across(precision:f1, list("mean" = mean, "sd" = sd)))
)
cat("************\n")


# -------------------------------------------------------------------------

consensus_scores <- list()
count1 <- 1
for (imagename in unique(consensus_df$imagen)) {
  ground_truth <- dplyr::filter(experts, imagen == imagename)
  consensus_predictions <- dplyr::filter(consensus_df, imagen == imagename)
  current_scores <- evaluate_annotations(ground_truth, consensus_predictions)
  current_scores$imagen = imagename
  consensus_scores[[count1]] <- as.data.frame(current_scores)
  count1 <- count1 + 1
}

consensus_scores_df <- as_tibble(do.call(rbind, consensus_scores))
scores_by_consensus <- consensus_scores_df %>%
  summarise(
    precision = sum(tp) / (sum(tp) +sum(fp)),
    recall = sum(tp) / (sum(tp) +sum(fn)),
    f1 = 2 * (precision * recall) / (precision + recall)
  ) %>%
  mutate(strategy = "first_strategy")

cat("***** scores by consensus *****\n")
print(scores_by_consensus)

# -------------------------------------------------------------------------

consensus_solaped_scores <- list()
count2 <- 1
for (imagename in unique(solaped_consensus_df$imagen)) {
  ground_truth <- dplyr::filter(experts, imagen == imagename)
  consensus_solaped_predictions <- dplyr::filter(solaped_consensus_df, imagen == imagename)
  current_solaped_scores <- evaluate_annotations(ground_truth, consensus_solaped_predictions)
  current_solaped_scores$imagen = imagename
  consensus_solaped_scores[[count2]] <- as.data.frame(current_solaped_scores)
  count2 <- count2 + 1
}

consensus_solaped_scores_df <- as_tibble(do.call(rbind, consensus_solaped_scores))
scores_by_consensus_solaped <- consensus_solaped_scores_df %>%
  summarise(
    precision = sum(tp) / (sum(tp) +sum(fp)),
    recall = sum(tp) / (sum(tp) +sum(fn)),
    f1 = 2 * (precision * recall) / (precision + recall)
  ) %>%
  mutate(strategy = "second_strategy")

cat("***** scores by solaped consensus *****\n")
print(scores_by_consensus_solaped)

# -------------------------------------------------------------------------

combined_scores <- bind_rows(scores_by_consensus, scores_by_consensus_solaped)
combined_scores <- combined_scores %>%
  select(strategy, everything())

cat("***** scores of both consensus strategies *****\n")
print(combined_scores)

# -------------------------------------------------------------------------

# just test that the metrics make sense with some random images and users
# run_test(sample(users$imagen, 1), sample(users$user_name, 1))
