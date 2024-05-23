#Calculate IoU
calculate_iou <- function(box1, box2) {
  x_left <- max(box1[1], box2[1])
  y_top <- max(box1[3], box2[3])
  x_right <- min(box1[2], box2[2])
  y_bottom <- min(box1[4], box2[4])
  intersection_area <- max(0, x_right - x_left + 1) * max(0, y_bottom - y_top + 1)
  box1_area <- (box1[2] - box1[1] + 1) * (box1[4] - box1[3] + 1)
  box2_area <- (box2[2] - box2[1] + 1) * (box2[4] - box2[3] + 1)
  union_area <- box1_area + box2_area - intersection_area
  iou <- intersection_area / union_area
  return(iou)
}
