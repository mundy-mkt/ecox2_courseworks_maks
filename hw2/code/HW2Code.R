### ||| PROBLEM 2 ---------------------------------------------------------------

## Imports
# Core data manipulation, plotting, import/writing packages
library(tidyverse)
library(stargazer)
library(ggplot2)
library(zoo)
library(readxl)
library(stringr)
library(purrr)

# Statistical tests and robust errors
library(lmtest)
library(sandwich)
library(tseries)
library(car)

# No scientific numbers
options(scipen = 999)

## || Task 2: Downloaing the data ----------------

# Read Excel and rename first column
parental_leave <- read_excel("hw2/data/parental_leave_data.xlsx")
parental_leave <- parental_leave %>%
  rename(country = ...1)

# Find columns for leave types
protected_idx <- as.vector(parental_leave[1, ]) == "job-protected leave (weeks)"
paid_idx <- as.vector(parental_leave[1, ]) == "paid leave (weeks)"

# Keep country column in selection
protected_idx[1] <- TRUE
paid_idx[1] <- TRUE

# Pivot protected leave from wide
parental_leave_protected <- parental_leave[-1, protected_idx] %>%
  pivot_longer(
    cols = -country,
    names_to = "year",
    values_to = "jobprotected_leave"
  ) %>%
  mutate(
    year = as.integer(str_sub(as.character(year), 1, 4)),
    jobprotected_leave = as.numeric(jobprotected_leave)
  )

# Pivot paid leave from wide
parental_leave_paid <- parental_leave[-1, paid_idx] %>%
  pivot_longer(
    cols = -country,
    names_to = "year",
    values_to = "paid_leave"
  ) %>%
  mutate(
    year = as.integer(str_sub(as.character(year), 1, 4)),
    paid_leave = as.numeric(paid_leave)
  )

# Merge datasets on country and year
## |Main dataset is created ----
parental_leave <- merge(
  parental_leave_protected,
  parental_leave_paid,
  by = c("country", "year")
)

# Cleaning intermediary objects
keep <- c("parental_leave")
rm(list = setdiff(ls(envir = .GlobalEnv), keep), envir = .GlobalEnv)

## |Importing OTHER datasets ----

col_names <- c("country", "2013", "2018", "2023")
country_names <- unique(parental_leave$country)

data_prep <- function(df, rows_skip, cols_skip, var_name) {
  df <- df[-rows_skip, -cols_skip] %>%                 
    rename_with(~ col_names) %>%                       
    filter(country %in% country_names) %>%             
    pivot_longer(
      cols = -country,                                 
      names_to = "year",
      values_to = var_name
    ) %>%
    mutate(
      year = as.integer(stringr::str_sub(as.character(year), 1, 4)),
      !!var_name := suppressWarnings(as.numeric(.data[[var_name]]))
    )
}


# Gross domestic product (GDP) and main components per capita ----
# [nama_10_pc__custom_18724757
gdp_per_capita <- read_excel("hw2/data/gdp_per_capita.xlsx", col_names = FALSE, sheet = 2, skip = 8)
gdp_per_capita <- data_prep(
  df = gdp_per_capita, 
  rows_skip = c(1, 2, seq(42, 47, 1)), 
  cols_skip = c(3, 5, 7), 
  var_name = 'gdp_per_capita'
  )

## Gender employment gap ----
# [sdg_05_30__custom_18726860]
gender_empl_gap <- read_excel("hw2/data/gen_empl_gap.xlsx", col_names = FALSE, sheet = 2, skip = 8)
gender_empl_gap <- data_prep(
  df = gender_empl_gap, 
  rows_skip = c(1, 2, seq(38, 43, 1)), 
  cols_skip = c(3, 5, 7), 
  var_name = 'gender_empl_gap'
)

# Gender pay gap in unadjusted form ----
# [sdg_05_20__custom_18724287]
gender_pay_gap <- read_excel("hw2/data/gender_pay_gap.xlsx", col_names = FALSE, sheet = 2, skip = 8)
gender_pay_gap <- data_prep(
  df = gender_pay_gap, 
  rows_skip = c(1, 2, seq(39, 46, 1)), 
  cols_skip = c(3, 5, 7), 
  var_name = 'gender_pay_gap'
)
# Employed information and communications technology (ICT) specialists by sex ----
# [isoc_sks_itsps__custom_18726980]
ict_spec_sex <- read_excel("hw2/data/ict_spec_sex.xlsx", col_names = FALSE, sheet = 2, skip = 7)
ict_spec_sex <- data_prep(
  df = ict_spec_sex, 
  rows_skip = c(1, 2, seq(39, 46, 1)), 
  cols_skip = c(3, 5, 7), 
  var_name = 'ict_spec_sex'
)
# Children in formal childcare or education ---- 
# [ilc_caindformal__custom_18741434]
child_forml_care <- read_excel("hw2/data/child_formal_care.xlsx", col_names = FALSE, sheet = 2, skip = 9)
child_forml_care <- data_prep(
  df = child_forml_care, 
  rows_skip = c(1, 2, seq(39, 43, 1)), 
  cols_skip = c(3, 5, 7), 
  var_name = 'child_forml_care'
)

## | MERGING all variables into main dataset ----
dfs_list <- list(
  parental_leave, 
  child_forml_care, 
  gdp_per_capita, 
  gender_pay_gap, 
  ict_spec_sex,
  gender_empl_gap
  )
pay_gap <- reduce(dfs_list, merge, by = c("country", "year"), all = T)

# Cleaning intermediary objects
keep <- c("pay_gap")
rm(list = setdiff(ls(envir = .GlobalEnv), keep), envir = .GlobalEnv)


## || 3: Preparing variables + Visualizations ----------------

# Removing United Kingdom
glimpse(pay_gap %>% filter(country == 'United Kingdom'))
pay_gap <- pay_gap %>%
  filter(country != 'United Kingdom')


# | Visualizing distributions and relationships ----

# Preparing long df for visualizing all histograms
hist_long <- pay_gap %>%
  mutate(year = factor(year)) %>%                       
  pivot_longer(                                         
    cols = -c(country, year),
    names_to = "variable", values_to = "value"
  )

# means for each variable and year
means <- hist_long %>%
  group_by(variable, year) %>%
  summarise(average = mean(value, na.rm = TRUE), .groups = "drop")

# Histograms ----
ggplot(hist_long, aes(x = value, colour = year, fill = year)) +
  geom_density(linewidth = 0.7, alpha = 0.1) +
  geom_vline(data = means, aes(xintercept = average, colour = year),
             linetype = "dashed", linewidth = 0.6, show.legend = FALSE) +
  facet_wrap(~ variable, scales = "free") +
  labs(x = NULL, y = "Density") +
  theme_minimal() +
  labs(
    title = 'Density of all variables'
  )

# Making gdp_per_capita in log form
pay_gap <- pay_gap %>% 
  mutate(
    log_gdp_per_capita = log(gdp_per_capita)
  ) %>% 
  select(-gdp_per_capita)

box_long <- pay_gap %>%
  mutate(year = factor(year)) %>%                       
  pivot_longer(                                         
    cols = -c(country, year),
    names_to = "variable", values_to = "value"
  )

# Boxplots ----
ggplot(box_long, aes(x = value, y = year)) + 
  geom_boxplot() +
  geom_jitter() +
  facet_wrap(~ variable, scales = 'free')

# Preparing long df for visualizing scaterplotts with gender_pay_gap
sc_long <- pay_gap %>%
  mutate(year = factor(year)) %>%
  pivot_longer(
    cols = -c(country, year, gender_pay_gap),
    names_to = "xvar", values_to = "x"
  )

# Scatterplots ----
ggplot(sc_long, aes(x = x, y = gender_pay_gap, colour = year)) +
  geom_point(alpha = 0.7, size = 1.9) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.7) +
  facet_wrap(~ xvar, scales = "free_x") +
  labs(x = NULL, y = "Gender pay gap (pp)", colour = "Year") +
  theme_minimal()

## | Creating columns for FDE and FE ----

# Cleaning intermediary objects
keep <- c("pay_gap")
rm(list = setdiff(ls(envir = .GlobalEnv), keep), envir = .GlobalEnv)

# Creating centered variables and differences (FE, FDE) ----
centering <- function(x) {
  x - mean(x, na.rm = TRUE)
}
pay_gap <- pay_gap %>% 
  arrange(country, year) %>%
  mutate(
    across(where(is.numeric) & !any_of('year'),
           ~ .x - mean(.x, na.rm = TRUE),
           .names = "{.col}_centered"),
    across(where(is.numeric) & !any_of('year') & !ends_with("_centered"),
           ~ .x - dplyr::lag(.x),
           .names = "{.col}_fd"),
    .by = country
  )

## || 4 + 5: First model + Summary statistics ----------------

# | Summary table of the data  ----

# Creating a function to give all necessary summary stats
summary_stats <- function(x) {
  c(
    Min = min(x, na.rm = TRUE),
    QRT1 = quantile(x, 0.25, na.rm = TRUE),
    Median = median(x, na.rm = TRUE),
    Mean = mean(x, na.rm = TRUE),
    QRT3 = quantile(x, 0.75, na.rm = TRUE),
    Max = max(x, na.rm = TRUE),
    Number_of_NAs = sum(is.na(x)),
    SD = sd(x, na.rm = TRUE)
  )
}
# Initializing summary df
summary_df <- data.frame(
  Var_name = character(),
  Min = numeric(),
  QRT1 = numeric(),
  Median = numeric(),
  Mean = numeric(),
  QRT3 = numeric(),
  Max = numeric(),
  Number_of_NAs = integer(),
  SD = numeric()
)

# Filling summary df row-wise
for (name in colnames(pay_gap)[-c(1, 2)]) {
  x <- pay_gap[[name]]
  
  stats <- summary_stats(x)
  row   <- data.frame(
    Var_name = name,
    t(stats)
  )
  summary_df <- rbind(summary_df, row)
}


# | 1st model using 2023 data ----
model1_2023 <- lm(
  'gender_pay_gap ~ jobprotected_leave + paid_leave', 
  data = pay_gap[pay_gap$year == 2023,]
  )

# Model results
summary(model1_2023)

# Visualizing normality and func specification
check_model(model1_2023, check = c('normality', 'linearity'))

# Homoscedasticity test
bptest(model1_2023)

## || 6: First model all years ----------------

model1_all <- lm(
  gender_pay_gap ~ jobprotected_leave + paid_leave + factor(year), 
  data = pay_gap
)

# Model results
summary(model1_all)

# Visualizing normality, func specification, VIF, leverage, homoscedasticity
check_model(model1_all)

# Breusch-Pagan test (Homoscedasticity)
bptest(model1_all)

# Durbin–Watson test (only 1 order autocorrelation)
dwtest(model1_all)
# Visualization for higher order autocorrelation
acf(residuals(model1_all))

# Heteroscedasticity and serial correlation-robust SEs
coeftest(model1_all, vcov. = vcovCL(model1_all, cluster = ~ country))

## || 7. gender_pay_gap ~ paid_leave + controls (FE) ----------------

# Fixed effects
FE_ind <- plm(
  gender_pay_gap ~ child_forml_care + log_gdp_per_capita + paid_leave + jobprotected_leave + gender_empl_gap + ict_spec_sex,
  model = 'within',
  effect = 'individual',
  index = c('country', 'year'),
  data = pay_gap
)

FE <- plm(
  gender_pay_gap ~ paid_leave + ict_spec_sex + log_gdp_per_capita + I(paid_leave)^2,
  model = 'within',
  effect = 'twoways',
  index = c('country', 'year'),
  data = pay_gap
)

FE_all_vars <- plm(
  gender_pay_gap ~ child_forml_care + log_gdp_per_capita + paid_leave + jobprotected_leave + gender_empl_gap + ict_spec_sex,
  model = 'within',
  effect = 'twoways',
  index = c('country', 'year'),
  data = pay_gap
)

FE_initial <- plm(
  gender_pay_gap ~ paid_leave + jobprotected_leave,
  model = 'within',
  index = c('country', 'year'),
  effect = 'twoways',
  data = pay_gap
)
summary(FE_all_vars)


performance::check_model(FE_all_vars)
bptest(FE_all_vars)
pwartest(FE_all_vars)
acf(residuals(FE_all_vars))
coeftest(FE_all_vars, vcov. = vcovHC(FE_all_vars, type = "HC1", cluster = "group"))


## || 8. Assumptions disscussion ----------------

## || 9. FE, FDE, RE, Pooled OLS ----------------

FE_main <- plm(
  gender_pay_gap ~ log_gdp_per_capita + paid_leave + jobprotected_leave + ict_spec_sex,
  model = 'within',
  effect = 'twoways',
  index = c('country', 'year'),
  data = pay_gap
)

# Random Effects
RE_main <- plm(
  gender_pay_gap ~ log_gdp_per_capita + paid_leave + jobprotected_leave + ict_spec_sex,
  model = 'random',
  effect = 'individual',
  index = c('country', 'year'),
  data = pay_gap
)

check_model(RE)
bptest(RE)
dwtest(RE)
acf(residuals(RE))
coeftest(RE, vcov. = vcovCL(RE, cluster = ~ country))


# Hausman test for serial correlation due to a_i
phtest(FE_main, RE_main)

# First Difference Estimator
FD <- plm(
  'gender_pay_gap ~ child_forml_care + log_gdp_per_capita + paid_leave',
  model = 'fd',
  index = c('country', 'year'),
  data = pay_gap
)
summary(FD)

check_model(FD)
bptest(FD)
dwtest(FD)
acf(residuals(FD))
coeftest(FD, vcov. = vcovCL(FD, cluster = ~ country))


# Pooled OLS
pooledOLS <- plm(
  gender_pay_gap ~ child_forml_care + log_gdp_per_capita + paid_leave + factor(year),
  index = c('country', 'year'),
  model = 'pooling',
  data = pay_gap
)
summary(pooledOLS)

summary(pooledOLS)
check_model(pooledOLS)
bptest(pooledOLS)
dwtest(pooledOLS)
acf(residuals(pooledOLS))
coeftest(pooledOLS, vcov. = vcovCL(pooledOLS, cluster = ~ country))
