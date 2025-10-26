### PROBLEM 1 ---------------------------------------------------------------
library(ggplot2)
## Seed
rm(list = ls())
set.seed(42)

## Generating iid true error with mean = 0 and positive sd_u
sd_u <- runif(1, min = 0, max = 5)
print(sd_u)
true_u <- rnorm(31, mean = 0, sd = sd_u)
ggplot(data = data.frame(true_u), aes(x = true_u)) +
  geom_histogram(bins = 15, fill = "gray", color = "black")

## x components: noise and trend
sd_x <- runif(1, min = 0.3, max = 0.6) # sd for x-noise
print(sd_x)
noise_x <- rnorm(31, sd = sd_x)
ggplot(data = data.frame(noise_x), aes(x = true_u)) +
  geom_histogram(bins = 15, fill = "gray", color = "black")


beta_trend_x <- runif(1, min = 0.1, max = 0.3) # slope of trend
print(beta_trend_x)
trend_x <- seq(0, 10, length.out = 31) # time index

alpha <- 2 # intercept
true_x <- alpha + beta_trend_x * trend_x + noise_x
ggplot(data = data.frame(true_x), aes(x = true_x)) +
  geom_histogram(bins = 15, fill = "gray", color = "black")

## Replacing true x with observed x
observed_x <- c(2, 3, 4.5, 5)
observed_x_t <- c(1, 11, 21, 31)
j <- 1
for (i in observed_x_t) {
  true_x[i] <- observed_x[j]
  j <- j + 1
}
print(true_x)

## Building interpolated x between observed points
interpol_x <- c()
for (i in seq_len(length(observed_x) - 1)) {
  if (i == 1) {
    chunk <- seq(from = observed_x[i], to = observed_x[i + 1], by = 0.1)
  } else if (i == 2) {
    chunk <- seq(from = observed_x[i], to = observed_x[i + 1], by = 0.15)
  } else {
    chunk <- seq(from = observed_x[i], to = observed_x[i + 1], by = 0.05)
  }
  if (i > 1) chunk <- chunk[-1] # drop overlap
  interpol_x <- c(interpol_x, chunk)
}
print(interpol_x)

helper_list_x <- list(observed_x[1:2], observed_x[2:3], observed_x[3:4])
aggregated_x <- c()
j <- 1
for (part in helper_list_x) {
  aggregated_x[j] <- mean(part)
  j <- j + 1
}

## Generating y
true_beta <- runif(1, min = 0, max = 5)
print(true_beta)
true_intercept <- runif(1, min = 0, max = 5)
print(true_intercept)

observed_y <- true_intercept + true_beta * true_x + true_u
observed_y

part_y <- c(observed_y[1], observed_y[11], observed_y[21], observed_y[31])

helper_list_y <- list(observed_y[1:11], observed_y[11:21], observed_y[21:31])
aggregated_y <- c()
j <- 1
for (part in helper_list_y) {
  aggregated_y[j] <- mean(part)
  j <- j + 1
}
ggplot(data = data.frame(observed_y), aes(x = observed_y)) +
  geom_histogram(bins = 15, fill = "gray", color = "black")

vis <- data.frame(true_x, observed_y)
vis$observed <- ifelse(vis$true_x %in% observed_x, TRUE, FALSE)
ggplot(data = vis, aes(x = true_x, y = observed_y, color = observed)) +
  geom_point(size = 2) +
  scale_color_manual(values = c("TRUE" = "red", "FALSE" = "black")) +
  scale_fill_manual(values = c("TRUE" = "red", "FALSE" = "grey50")) +
  geom_abline(slope = true_beta, intercept = true_intercept, color = "blue")

## Creating models
whole_data <- data.frame(true_x, observed_y, interpol_x)
part_data <- data.frame(observed_x, part_y)
aggregated_data <- data.frame(aggregated_x, aggregated_y)

model1 <- lm("observed_y ~ interpol_x", data = whole_data)
model2 <- lm("part_y ~ observed_x", data = part_data)
model3 <- lm("aggregated_y ~ aggregated_x", data = aggregated_data)

whole_data$fit_model1 <- model1$fitted.values
whole_data$res_model1 <- model1$residuals

part_data$fit_model2 <- model2$fitted.values
part_data$res_model2 <- model2$residuals

aggregated_data$fit_model3 <- model3$fitted.values
aggregated_data$res_model3 <- model3$residuals

## a) Calculating variance of OLS as a function of res variance
var_model1 <- sum(whole_data$res_model1**2) / ((nrow(whole_data) - length(model1$coefficients)) * sum((whole_data$interpol_x - mean(whole_data$interpol_x))**2))
var_model2 <- sum(part_data$res_model2**2) / ((nrow(part_data) - length(model2$coefficients)) * sum((part_data$observed_x - mean(part_data$observed_x))**2))
var_model3 <- sum(aggregated_data$res_model3**2) / ((nrow(aggregated_data) - length(model3$coefficients)) * sum((aggregated_data$aggregated_x - mean(aggregated_data$aggregated_x))**2))
