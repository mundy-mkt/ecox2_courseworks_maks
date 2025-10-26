### PROBLEM 2 ---------------------------------------------------------------

library(tidyverse)
library(stargazer)
library(ggplot2)
library(zoo)
library(readxl)
#
data_houses <- read_excel("hw1/data/Ecox2 HW1 data New.xlsx")
colnames(data_houses) <- c("year_quarter", "const_overall", "constr_fam", "constr_apart", "renov_fam", "renov_apart", "constr_nondwell", "house_price_idx_2015", "pop", "cpi_yr_per", "nom_gdp")

cnb_loans_interest <- read_excel("hw1/data/cnb_loans_interest.xlsx", col_names = FALSE, skip = 3)
cnb_loans_volume <- read_excel("hw1/data/cnb_loans_volume.xlsx", col_names = FALSE, skip = 3)
eurostat_prod_prices <- read_excel("hw1/data/eurostat_prod_prices.xlsx", col_names = FALSE, sheet = 2, skip = 10)
eurostat_employed <- read_excel("hw1/data/eurostat_employed.xlsx", col_names = FALSE, sheet = 2, skip = 9)

#
data_houses$year_quarter <- as.yearqtr(gsub(" - ", " ", data_houses$year_quarter))
cnb_loans_interest <- cnb_loans_interest[2:41, 2] %>%
  rename_with(~"house_loan_interest")
cnb_loans_volume <- cnb_loans_volume[2:41, 2] %>%
  rename_with(~"house_loan_volume")
data_houses$house_loan_interest <- cnb_loans_interest
data_houses$house_loan_volume <- cnb_loans_volume

# FUNCTION FOR IMPORTING EUROSTAT DATA
eurostat_import <- function(eurostat_df, col_name = "value") {
  
  time <- as.vector(t(eurostat_df[1, 2:ncol(eurostat_df)]))
  vals <- as.vector(t(eurostat_df[3, 2:ncol(eurostat_df)]))
  
  out <- data.frame(yr_qr = time, value = vals) %>%
    dplyr::mutate(
      yr_qr = gsub("-", " ", yr_qr),
      yr_qr = zoo::as.yearqtr(yr_qr, format = "%Y Q%q"),
      value = dplyr::na_if(value, ":"),
      value = readr::parse_number(value)
    ) %>%
    tidyr::drop_na() %>%
    dplyr::filter(
      yr_qr >= zoo::as.yearqtr("2015 Q1", format = "%Y Q%q"),
      yr_qr <= zoo::as.yearqtr("2024 Q4", format = "%Y Q%q")
    )
  colnames(out)[2] <- col_name
  return(out)
}

eurostat_employed <- eurostat_import(eurostat_employed, col_name = 'employed_1000')
eurostat_prod_prices <- eurostat_import(eurostat_prod_prices, col_name = 'prod_price_idx_2021')

### DATA FINISHES HERE -----------------

## (b) Summary table of the data
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

summary_df <- data.frame(
  "Summary Statistics" = c(
    "Min", "First Quartile", "Median", "Mean", "Third Quartile", "Max",
    "Number of NAs"
  )
)

paired_names <- Map(list, true_names, colnames(data_houses))
for (cols in paired_names) {
  new_name <- cols[[1]]
  old_name <- cols[[2]]

  df_col <- data_houses[[old_name]]

  if (is.numeric(df_col)) {
    summary_df$old_name <- summary_stats(df_col)
  }
}


data_houses %>%
  select(where(is.numeric)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "value"
  ) %>%
  ggplot(aes(x = value)) +
  geom_histogram(color = "black", fill = "blue", bins = 15) +
  facet_wrap(~variable, scales = "free_x")


data_houses %>%
  pivot_longer(
    cols = -year_quarter,
    names_to = "variable",
    values_to = "value"
  ) %>%
  ggplot(aes(x = year_quarter, y = value, group = variable)) +
  geom_line() +
  geom_smooth(method = "loess") +
  facet_wrap(~variable, scales = "free_y")
