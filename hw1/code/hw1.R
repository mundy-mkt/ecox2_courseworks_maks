### PROBLEM 1 ---------------------------------------------------------------

rm(list = ls())
# library(tidyverse)
# library(stargazer)
# library(ggplot2)

## ------- Dependent variables -------

observed_u = 
observed_y = 

observed_x <- c(2, 3, 4.5, 5)
interpol_x <- c()

# Generating interpolated x
for (i in seq_len(length(observed_x) - 1)) {
  if (i == 1) {
    chunk <- seq(from = observed_x[i], to = observed_x[i + 1], by = 0.1)
  } else if (i == 2) {              
    chunk <- seq(from = observed_x[i], to = observed_x[i + 1], by = 0.15)
  } else {
    chunk <- seq(from = observed_x[i], to = observed_x[i + 1], by = 0.05)
  }
  if (i > 1) chunk <- chunk[-1]     
  interpol_x <- c(interpol_x, chunk)
}
print(interpol_x)
length(interpol_x)



### PROBLEM 2 ---------------------------------------------------------------

library(tidyverse)
library(stargazer)
library(ggplot2)