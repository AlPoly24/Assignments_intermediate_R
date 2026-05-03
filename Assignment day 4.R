# Selection sort

# Define the selection_sort function
selection_sort <- function(arr) {
  # 1. Get the length of the input vector
  n <- length(arr)
  # 2. Outer loop: Iterate through the vector (except the last element)
  for (i in 1:(n - 1)) {
    # 3. Assume the current position 'i' holds the minimum value
    min_index <- i
    # 4. Inner loop: Look for a smaller value in the rest of the vector
    for (j in (i + 1):n) {
      if (arr[j] < arr[min_index]) {
        # Update min_index if a smaller value is found
        min_index <- j
      }
    }
    # 5. Swap: If the found minimum isn't at the current position 'i', swap them
    if (min_index != i) {
      temp <- arr[i]
      arr[i] <- arr[min_index]
      arr[min_index] <- temp
    }
  }
  # 6. Return the fully sorted vector
  return(arr)
}

# Testing

# Generate a random set of letters
unsorted_letters <- sample(letters)
cat("Unsorted:", unsorted_letters, "\n")
# Call the function and assign it to sorted_vector
sorted_vector <- selection_sort(unsorted_letters)
# Print the result
cat("Sorted:  ", sorted_vector, "\n")

# Comparing slgorithms
# 1. Bubble Sort
bubble_sort <- function(arr) {
  n <- length(arr)
  for (i in 1:(n - 1)) {
    for (j in 1:(n - i)) {
      if (arr[j] > arr[j + 1]) {
        temp <- arr[j]
        arr[j] <- arr[j + 1]
        arr[j + 1] <- temp
      }
    }
  }
  return(arr)
}

# 2. Insertion Sort
insertion_sort <- function(arr) {
  n <- length(arr)
  for (i in 2:n) {
    key <- arr[i]
    j <- i - 1
    while (j >= 1 && arr[j] > key) {
      arr[j + 1] <- arr[j]
      j <- j - 1
    }
    arr[j + 1] <- key
  }
  return(arr)
}

# 3. Quick Sort
quick_sort <- function(arr) {
  if (length(arr) <= 1) {
    return(arr)
  }
  pivot <- arr[length(arr) %/% 2]
  left <- arr[arr < pivot]
  middle <- arr[arr == pivot]
  right <- arr[arr > pivot]
  return(c(quick_sort(left), middle, quick_sort(right)))
}

# 4. Performance Comparison Function
check_algorithm <- function(algorithm, arr, number) {
  # Create an empty list to store results for each algorithm
  time <- list()
  # Outer loop
  for (a in 1:length(algorithm)) {
    # Create an empty vector to store the times for the repeats
    tmp_result <- c()
    # Inner loop
    for (b in 1:number) {
      # Capture start time
      tmp_time <- Sys.time()
      # Execute the specific algorithm
      algorithm[[a]](arr)
      # Calculate time difference
      time_diff <- Sys.time() - tmp_time
      # Append the time to the results vector
      tmp_result <- c(tmp_result, time_diff)
    }
    # Store the vector of times in the main list
    time[[a]] <- tmp_result
  }
  return(time)
}
# 5. Execution Call
# Set parameters: 3 algorithms, 1000 random numbers, 10 repetitions each
results <- check_algorithm(
  algorithm = list(bubble_sort, insertion_sort, quick_sort),
  arr = sample(1:1000),
  number = 10
)
# Naming the list for better readability
names(results) <- c("Bubble Sort", "Insertion Sort", "Quick Sort")
print(results)
