### PROBLEM 2 ---------------------------------------------------------------

## PART 2: Exploratory Data Analysis ----------------

# Load analysis libraries.
library(tidyverse)
library(stargazer)
library(ggplot2)
library(zoo)
library(readxl)
library(stringr)

# Load the main dataset
data_houses <- read_excel("hw1/data/Ecox2 HW1 data New.xlsx")

# Capture original column names.
true_names <- colnames(data_houses)
# Drop the time variable.
true_names <- true_names[-1]

# Rename columns for convinience
colnames(data_houses) <- c("year_quarter", "const_overall", "constr_fam", "constr_apart", "renov_fam", "renov_apart", "constr_nondwell", "house_price_idx_2010", "pop", "cpi_yr_per", "nom_gdp")

# 2.2 Adding new variables from CNB and Eurostat data ----

# Import CNB and Eurostat sources.
cnb_loans_interest <- read_excel("hw1/data/cnb_loans_interest.xlsx", col_names = FALSE, skip = 3)
cnb_loans_volume <- read_excel("hw1/data/cnb_loans_volume.xlsx", col_names = FALSE, skip = 3)
eurostat_prod_prices <- read_excel("hw1/data/eurostat_prod_prices.xlsx", col_names = FALSE, sheet = 2, skip = 10)
eurostat_employed <- read_excel("hw1/data/eurostat_employed.xlsx", col_names = FALSE, sheet = 2, skip = 9)

# Normalize quarter formatting
data_houses$year_quarter <- as.yearqtr(gsub(" - ", " ", data_houses$year_quarter))

# Extract relevant columns from CNB data
cnb_loans_interest <- cnb_loans_interest[2:41, c(1,2)] %>%
  rename_with(~c("yr_qr", "house_loan_interest")) %>% 
  arrange(yr_qr)
cnb_loans_volume <- cnb_loans_volume[2:41, c(1,2)] %>%
  rename_with(~c("yr_qr", "house_loan_volume")) %>% 
  arrange(yr_qr)

# Attach CNB series to houses
data_houses$house_loan_interest <- parse_double(cnb_loans_interest$house_loan_interest)
data_houses$house_loan_volume <- parse_double(cnb_loans_volume$house_loan_volume)

# Define Eurostat import helper
eurostat_import <- function(eurostat_df, col_name = "value") {
  # Pull time and value rows
  time <- as.vector(t(eurostat_df[1, 2:ncol(eurostat_df)]))
  vals <- as.vector(t(eurostat_df[3, 2:ncol(eurostat_df)]))
  
  # Build output df
  out <- data.frame(yr_qr = time, value = vals) %>%
    dplyr::mutate(
      yr_qr = gsub("-", " ", yr_qr),
      yr_qr = as.yearqtr(yr_qr, format = "%Y Q%q"),
      value = na_if(value, ":"),
      value = parse_double(value)
    ) %>%
    tidyr::drop_na() %>%
    dplyr::filter(
      yr_qr >= as.yearqtr("2015 Q1", format = "%Y Q%q"),
      yr_qr <= as.yearqtr("2024 Q4", format = "%Y Q%q")
    ) %>% 
    arrange(yr_qr)
  
  # Rename value to target name
  colnames(out)[2] <- col_name
  return(out)
}

# Import Eurostat employed and producer prices series
eurostat_employed <- eurostat_import(eurostat_employed, col_name = "employed_1000")
eurostat_prod_prices <- eurostat_import(eurostat_prod_prices, col_name = "prod_price_idx_2021")

# Append Eurostat series to houses.
data_houses$employed_1000 <- eurostat_employed$employed_1000
data_houses$prod_price_idx_2021 <- eurostat_prod_prices$prod_price_idx_2021

# Adding invasion of Ukraine and quarter dummies
data_houses <- data_houses %>%
  mutate(
    ones = 1L,
    quart = str_sub(as.character(year_quarter), -2),
  ) %>%
  pivot_wider(
    names_from = quart,
    values_from = ones,
    values_fill = 0,
    values_fn = sum
  )

data_houses <- data_houses %>%
  mutate(ukr_invasion = as.integer(year_quarter >= as.yearqtr("2022 Q1")))

# Cleaning intermediary objects
keep <- c("data_houses", "true_names")
rm(list = setdiff(ls(envir = .GlobalEnv), keep), envir = .GlobalEnv)

## 2.2.3 Summary table of the data  ----
summary_stats <- function(df_col) {
  stats <- c(
    min(df_col, na.rm = TRUE),
    quantile(df_col, 0.25, na.rm = TRUE),
    median(df_col, na.rm = TRUE),
    mean(df_col, na.rm = TRUE),
    quantile(df_col, 0.75, na.rm = TRUE),
    max(df_col, na.rm = TRUE),
    sum(is.na(df_col))
  )
  return(stats)
}

# Initialize summary table header.
summary_df <- data.frame(
  "Summary Statistics" = c(
    "Min", "First Quartile", "Median", "Mean", "Third Quartile", "Max",
    "Number of NAs"
  )
)

# Extend display names list.
true_names <- c(true_names, "Loans on house purchase (millions of CZK, current prices)", "Interest on house purchase loans", "Employed persons (thousands)", "Production price index (2021=100)")

# Pair display names with sources.
paired_names <- Map(list, true_names, colnames(data_houses)[-1])

# Fill summary table columns.
for (cols in paired_names) {
  new_name <- cols[[1]]
  old_name <- cols[[2]]
  
  df_col <- data_houses[[old_name]]
  
  if (is.numeric(df_col)) {
    summary_df[[old_name]] <- summary_stats(df_col)
  }
}

## 2.2.4 Visualizations of the most important variables  ----

# Histogram
data_houses %>%
  select(where(is.numeric)) %>%
  pivot_longer(
    cols = c(house_price_idx_2010, pop, house_loan_interest, const_overall, house_loan_volume, prod_price_idx_2021),
    names_to = "variable",
    values_to = "value"
  ) %>%
  ggplot(aes(x = value)) +
  geom_histogram(color = "black", fill = "blue", bins = 15) +
  facet_wrap(~variable, scales = "free_x") +
  theme_bw() +
  labs(
    title = 'Histogram of the most important variables'
  )

# Time series plot with non-linear trend
data_houses %>%
  pivot_longer(
    cols = c(house_price_idx_2010, pop, house_loan_interest, const_overall, house_loan_volume, prod_price_idx_2021),
    names_to = "variable",
    values_to = "value"
  ) %>%
  ggplot(aes(x = year_quarter, y = value, group = variable)) +
  geom_line(aes(color = "Observed")) +
  geom_smooth(aes(color = "Linear trend"), method = "lm", se = FALSE) +
  facet_wrap(~variable, scales = "free_y") +
  scale_color_manual(values = c("Observed" = "black", "Linear trend" = "blue")) +
  guides(color = guide_legend(title = NULL)) +
  theme_bw() +
  labs(
    title = 'Time series of the most important variables'
  )


## 2.2.5 Stationarity and trending variables ----

data_houses %>%
  pivot_longer(
    cols = -c(year_quarter, Q1, Q2, Q3, Q4, ukr_invasion),
    names_to = "variable", values_to = "value"
  ) %>%
  ggplot(aes(x = year_quarter, y = value, group = variable)) +
  geom_vline(
    aes(
      xintercept = as.yearqtr('2022 Q1'), 
      color = "Ukraine invasion"
      ),
      linewidth = 1.1
    ) +
  geom_line(aes(color = "Observed"), alpha = 0.9) +
  geom_smooth(aes(color = "Non-linear trend"), method = "loess", se = FALSE) +
  geom_point(aes(color = "Observed"), size = 0.8) +
  facet_wrap(~variable, scales = "free_y") +
  theme_classic() +
  scale_color_manual(
    values = c(
      "Observed" = "black", 
      "Non-linear trend" = "blue", 
      "Ukraine invasion" = "red"
      )
  ) +
  labs(
    title = "Stationarity and trending/seasonality analysis"
  )


