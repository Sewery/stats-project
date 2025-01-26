library(smoof)
library(plot3D)

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
  # print(min_point)
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

  # Ms
  print("MS:")
  res_1 <- ms_algo(dim, lower_b, upper_b, fun, 100)
  budget <- round(res_1[2] / 100)
  res_2 <- replicate(49, ms_algo(dim, lower_b, upper_b, fun, 50))
  res_ms <- c(res_1[1], res_2[1, ])
  avg_min_ms <- mean(res_ms)
  print(paste("Wartość średnia:", avg_min_ms))

  # Prs
  print("PRS:")
  res_prs <- replicate(50, prs_algo(dim, lower_b, upper_b, fun, 50 * budget))
  avg_min_prs <- mean(res_prs)
  print(paste("Wartość średnia:", avg_min_prs))
  print("-------")
  return(list(res_ms, res_prs))
}
generate_hist <- function(values, title) {
  h <- hist(values,
    main = title,
    xlab = "Wartości minimum",
    ylab = "Częstość",
    col = "lightblue",
    border = "black",
    labels = TRUE
  )
}
generate_hist <- function(values, title) {
  h <- hist(values,
    main = title,
    xlab = "Wartości minimów",
    ylab = "Częstość",
    col = "lightblue",
    border = "black",
    labels = TRUE
  )
}
generate_boxplot <- function(values_ms, values_prs, title) {
  b <- boxplot(
    values_ms,
    values_prs,
    main = title,
    names = c("MS", "PRS"),
    ylab = "Wartość"
  )
}
## 2D for Ackley function
print("----------------------")
print("Funkcja Ackleya:")
print("--------------")
print("Wartości minimalne dla 2D")
fn <- makeAckleyFunction(2)
results <- compare(fn, 2)
generate_hist(results[[1]], "Wykres czestości dla funkcji Ackleya 2D dla MS")
generate_hist(results[[2]], "Wykres czestości dla funkcji Ackleya 2D dla PS")
generate_boxplot(
  results[[1]],
  results[[2]],
  "Wykres pudelkowy dla funkcji Ackleya 2D"
)
#Mozesz podmienic na csc
dir.create("./data", showWarnings = FALSE)
write.table(results[[1]],file="./data/ms-data-ackley-2d.data")
write.table(results[[2]],file="./data/ps-data-ackley-2d.data")
# res_read<-read.table(file="ms-data-ackley-2d.data")
# print(res_read)

# 10D for Ackley function
print("---------")
print("Wartości minimalne dla 10D")
fn <- makeAckleyFunction(10)
results <- compare(fn, 10)
# generate_hist(results[[1]], "Wykres czestości dla funkcji Ackleya 10D dla MS")
# generate_hist(results[[2]], "Wykres czestości dla funkcji Ackleya 10D dla PS")
write.table(results[[1]],file="./data/ms-data-ackley-10d.data")
write.table(results[[2]],file="./data/ps-data-ackley-10d.data")

## 20D for Ackley function
print("---------")
print("Wartości minimalne dla 20D")
fn <- makeAckleyFunction(20)
results <- compare(fn, 20)
write.table(results[[1]],file="./data/ms-data-ackley-20d.data")
write.table(results[[2]],file="./data/ps-data-ackley-20d.data")

## 2D for Schwafel function
print("---------")
print("Wartości minimalne dla 2D")
fn <- makeSchwefelFunction(2)
results <- compare(fn, 2)
write.table(results[[1]],file="./data/ms-data-schwafel-2d.data")
write.table(results[[2]],file="./data/ps-data-schwafel-2d.data")

## 10D for Schwafel function
print("---------")
print("Wartości minimalne dla 10D")
fn <- makeSchwefelFunction(10)
results <- compare(fn, 10)
write.table(results[[1]],file="./data/ms-data-schwafel-10d.data")
write.table(results[[2]],file="./data/ps-data-schwafel-10d.data")

## 20D for Schwafel function
print("---------")
print("Wartości minimalne dla 20D")
fn <- makeSchwefelFunction(20)
results <- compare(fn, 20)
write.table(results[[1]],file="./data/ms-data-schwafel-20d.data")
write.table(results[[2]],file="./data/ps-data-schwafel-20d.data")