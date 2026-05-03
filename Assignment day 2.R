# Step 1: Create the Generic Method
# This serves as the dispatcher for our S3 classes
point <- function(x, ...) {
  UseMethod("point")
}

# Step 2: Create the Three Regular Methods
# 1D Method: Absolute difference
point.Point1D <- function(p1, p2) {
  abs(p1$x - p2$x)
}

# 2D Method: Pythagorean theorem
point.Point2D <- function(p1, p2) {
  sqrt((p1$x - p2$x)^2 + (p1$y - p2$y)^2)
}

# 3D Method: 3D Pythagorean theorem
point.Point3D <- function(p1, p2) {
  sqrt((p1$x - p2$x)^2 + (p1$y - p2$y)^2 + (p1$z - p2$z)^2)
}

# Step 3: Initialize Lists
# Create three lists with 10 empty (NULL) elements
my_1D <- vector("list", 10)
my_2D <- vector("list", 10)
my_3D <- vector("list", 10)

# Step 4: Populate Lists with Random Points
# Populate 1D points
for (a in 1:length(my_1D)) {
  my_1D[[a]] <- list(x = sample(1:100, 1))
  class(my_1D[[a]]) <- "Point1D"
}

# Populate 2D points
for (a in 1:length(my_2D)) {
  my_2D[[a]] <- list(x = sample(1:100, 1), y = sample(1:100, 1))
  class(my_2D[[a]]) <- "Point2D"
}

# Populate 3D points
for (a in 1:length(my_3D)) {
  my_3D[[a]] <- list(x = sample(1:100, 1), y = sample(1:100, 1), z = sample(1:100, 1))
  class(my_3D[[a]]) <- "Point3D"
}

# Step 5: Create the point_distance function
# This function iterates through endpoints and calculates distance from origin
point_distance <- function(origin, end_points) {
  distance <- c()
  for (a in 1:length(end_points)) {
    # The generic 'point' method handles the math based on the origin's class
    distance <- c(distance, point(origin, (end_points[[a]])))
  }
  return(distance)
}

# Step 6: Make Function Calls
# First, define our origin objects for each class
origin_1D_obj <- list(x = 0); class(origin_1D_obj) <- "Point1D"
origin_2D_obj <- list(x = 0, y = 0); class(origin_2D_obj) <- "Point2D"
origin_3D_obj <- list(x = 0, y = 0, z = 0); class(origin_3D_obj) <- "Point3D"

# Call the function and assign results to new objects
distance_1D <- point_distance(origin_1D_obj, my_1D)
distance_2D <- point_distance(origin_2D_obj, my_2D)
distance_3D <- point_distance(origin_3D_obj, my_3D)

# (Optional) Print results to check the output
print("Distances for 1D:")
print(distance_1D)

print("Distances for 2D:")
print(distance_2D)

print("Distances for 3D:")
print(distance_3D)
