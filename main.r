library(smoof)
library(plot3D)

save_results <- function(results, function_name, dimension) {
  ms_file <- paste("./data/ms-data-", function_name, "-", dimension, "d.data", sep = "")
  prs_file <- paste("./data/prs-data-", function_name, "-", dimension, "d.data", sep = "")
  
  write.table(results[[1]], file = ms_file, row.names = FALSE, col.names = FALSE)
  write.table(results[[2]], file = prs_file, row.names = FALSE, col.names = FALSE)
}

ms_algo <- function(dim, lower_b, upper_b, fun, number_of_points) {
  min_val <- Inf
  num_of_cals <- 0
  for (i in 1:number_of_points) {
    point <- runif(dim, min = lower_b, max = upper_b)
    ms <- optim(
      point, fun,
      method = "L-BFGS-B",
      lower = rep(lower_b, dim), upper = rep(upper_b, dim)
    )
    num_of_cals <- num_of_cals + ms$counts[1]
    if (min_val > ms$value) {
      min_val <- ms$value
    }
  }
  return(c(min_val, num_of_cals))
}

prs_algo <- function(dim, lower_b, upper_b, fun, number_of_points) {
  min_val <- Inf
  for (i in 1:number_of_points) {
    rand_point <- runif(dim, min = lower_b, max = upper_b)
    curr <- fun(rand_point)
    if (min_val > curr) {
      min_val <- curr
    }
  }
  return(min_val)
}

compare <- function(fun, dim) {
  lower_b <- as.numeric(getLowerBoxConstraints(fun))[1]
  upper_b <- as.numeric(getUpperBoxConstraints(fun))[1]
  
  print("MS:")
  res_1 <- ms_algo(dim, lower_b, upper_b, fun, 100)
  budget <- round(res_1[2] / 100)
  res_2 <- replicate(49, ms_algo(dim, lower_b, upper_b, fun, 50))
  res_ms <- c(res_1[1], res_2[1, ])
  avg_min_ms <- mean(res_ms)
  print(paste("Wartość średnia:", avg_min_ms))
  
  print("PRS:")
  res_prs <- replicate(50, prs_algo(dim, lower_b, upper_b, fun, 50 * budget))
  avg_min_prs <- mean(res_prs)
  print(paste("Wartość średnia:", avg_min_prs))
  print("-------")
  return(list(res_ms, res_prs))
}

# Generate histograms
generate_hist <- function(values, title) {
  hist(values,
       main = title,
       xlab = "Wartości minimum",
       ylab = "Częstość",
       col = "lightblue",
       border = "black",
       labels = TRUE
  )
}

# Generate boxplots
generate_boxplot <- function(values_ms, values_prs, title) {
  boxplot(
    values_ms,
    values_prs,
    main = title,
    names = c("MS", "PRS"),
    ylab = "Wartość"
  )
}

# Wszystkie wykresy
generate_all_plots <- function(results, function_name, dimension) {
  # Histogramy
  generate_hist(results[[1]], paste("Histogram dla", function_name, dimension, "D (MS)"))
  generate_hist(results[[2]], paste("Histogram dla", function_name, dimension, "D (PRS)"))
  
  # Pudełkowe
  generate_boxplot(
    results[[1]],
    results[[2]],
    paste("Wykres pudełkowy dla", function_name, dimension, "D")
  )
  save_results(results, function_name, dimension)
}


print("----------------------")
print("Funkcja Ackleya:")
print("--------------")

# 2D Ackley
print("Wartości minimalne dla 2D")
fn <- makeAckleyFunction(2)
results <- compare(fn, 2)
generate_all_plots(results, "Ackley", 2)

# 10D Ackley
print("---------")
print("Wartości minimalne dla 10D")
fn <- makeAckleyFunction(10)
results <- compare(fn, 10)
generate_all_plots(results, "Ackley", 10)

# 20D Ackley
print("---------")
print("Wartości minimalne dla 20D")
fn <- makeAckleyFunction(20)
results <- compare(fn, 20)
generate_all_plots(results, "Ackley", 20)


print("---------")
print("Funkcja Schwefela:")
print("--------------")

# 2D Schwefel
print("Wartości minimalne dla 2D")
fn <- makeSchwefelFunction(2)
results <- compare(fn, 2)
generate_all_plots(results, "Schwefel", 2)

# 10D Schwefel
print("---------")
print("Wartości minimalne dla 10D")
fn <- makeSchwefelFunction(10)
results <- compare(fn, 10)
generate_all_plots(results, "Schwefel", 10)

# 20D Schwefel
print("---------")
print("Wartości minimalne dla 20D")
fn <- makeSchwefelFunction(20)
results <- compare(fn, 20)
generate_all_plots(results, "Schwefel", 20)
