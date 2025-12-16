# PROBLEM 2 ---------------------------------------------------------------

# NOTE:
# Interesting functions found
#
# skimr::skim                   - quick, compact summary of all variables (types, missingness, basic stats)
# janitor::tabyl()              - fast frequency table for a variable (optionally with percentages)
# DataExplorer::create_report() - generates an automatic HTML EDA report for the whole dataset


## PART 2: Data Cleaning ----------------

### Importing libraries ----
# load packages for plotting, paths and data wrangling
library(ggplot2)
library(here)
library(tidyverse)
library(DataExplorer)
library(janitor)
library(knitr)
library(scales)

### Importing data ----
# read semicolon-separated CSV file
data <- read.csv2("hw4/data/Ecox2_HW4_data.csv")

### Missing values ----
# quick check for any missing values in the dataset
any(is.na(data))

### Inconsistent categories ----
# inspect structure after import and standardise names
glimpse(data)
data <- data %>%
  clean_names() %>%
  rename(
    default = default_payment_next_month,
    pay_1 = pay_0
    )

# bar chart of default proportions
p_default_bar <- data %>%
  count(default) %>%
  mutate(
    prop = n / sum(n),
    default = as.factor(default)
  ) %>%
  ggplot(aes(x = default, y = prop, fill = default)) +
  geom_col() +
  geom_text(
    aes(label = scales::percent(prop, accuracy = 0.1)),
    vjust = -0.3
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    x = "Default",
    y = "Proportion of observations",
    fill = "Default",
    title = "Class balance of the Default Next Month variable"
  ) +
  theme_bw()

# save plot to file
ggsave(
  filename = "hw4/code./default_proportions_bar.png",
  plot = p_default_bar,
  width = 6,
  height = 4
)

# define categorical-like variables and PAY variables for inspection
cat_cols <- c("sex", "education", "marriage", "default")
pay_cols <- colnames(data)[str_starts(colnames(data), "^pay_[0-9]+")]

# count distinct values in selected variables
n_unique <- data %>%
  select(all_of(c(cat_cols, pay_cols))) %>%
  summarise(
    across(
      everything(),
      ~ n_distinct(.x),
      .names = "{.col}"
    )
  )

# list unique categories for each selected variable
cat_unique <- data %>%
  select(all_of(c(cat_cols, pay_cols))) %>%
  summarise(
    across(
      everything(),
      ~ list(unique(.x)),
      .names = "{.col}"
    )
  )

# recode education and marriage, drop invalid codes and implausible ages
dim(data)

data <- data %>%
  mutate(
    education = recode(
      education,
      "1" = "graduate",
      "2" = "university",
      "3" = "high_school",
      "4" = "others"
    ),
    marriage = as.character(marriage),
    marriage = recode(
      marriage,
      "1" = "married",
      "2" = "single",
      "3" = "others",
    )
  ) %>%
  filter(!education %in% c("0", "5", "6")) %>%
  filter(marriage != "0") %>%
  filter(age <= 130) 

dim(data)

# number of rows where at least one of the repayment status eq -2
data %>% 
  filter(
    if_any(
      all_of(pay_cols),
      ~ .x == -2
    )
  ) %>%
  nrow()

# check updated distinct counts after cleaning
n_unique_clean <- data %>%
  select(all_of(c(cat_cols, pay_cols))) %>%
  summarise(
    across(
      everything(),
      ~ list(n_distinct(.x)),
      .names = "{.col}"
    )
  )

# Latex table with counts of distinct values
kable(
  n_unique_clean,
  format = "latex",
  booktabs = TRUE,
  caption = "Number of unique values in each qualitative variable after cleaning."
)

# encode education as ordered factor and set factor types for other variables
data$education <- ordered(data$education, levels = c("high_school", "university", "graduate", "others"))
data$sex <- as.factor(ifelse(data$sex == 1, "male", "female"))
data$marriage <- as.factor(data$marriage)

# clean and convert bill_amt1 to numeric
data <- data %>%
  mutate(
    bill_amt1 = str_replace_all(bill_amt1, ",", ""),
    bill_amt1 = as.numeric(bill_amt1)
  )

# verify structure after all recoding steps
glimpse(data)

# Cleaning intermediary objects
keep <- c("data", "pay_cols")
rm(list = setdiff(ls(envir = .GlobalEnv), keep), envir = .GlobalEnv)

### Duplicates ----

# summarise how often each id appears
id_counts <- data %>%
  count(id) %>%
  arrange(desc(n))
id_counts[1:3, ]

# inspect duplicate rows and one problematic id
data[duplicated(data), ]
data %>% filter(id == 29964)

# remove exact duplicate rows, keeping the first occurrence
data <- data[!duplicated(data), ]

### Outliers ----
# identify columns with bill amounts and repayment amounts
bill_cols <- colnames(data)[str_starts(colnames(data), "^bill_amt[0-9]+")]
pay_amt_cols <- colnames(data)[str_starts(colnames(data), "^pay_amt[0-9]+")]
quant_vars <- c(bill_cols, pay_amt_cols, "age", "limit_bal")

# boxplots for age, bill and repayment amounts to visualise extreme values
p_box <- data %>%
  select(all_of(quant_vars)) %>% 
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "value"
  ) %>%
  ggplot(aes(x = "", y = value)) +
  geom_boxplot(outlier.alpha = 0.4) +
  facet_wrap(~ variable, scales = "free_y") +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

# save boxplots for reporting
ggsave(
  filename = "hw4/code/boxplots_age_bill_pay.png",
  plot = p_box,
  width = 10,
  height = 8
)

# cutting the values
data <- data %>%
  mutate(
    across(
      all_of(quant_vars),
      ~ pmin(.x, quantile(.x, 0.998, na.rm = TRUE))
    )
  )

# Cleaning intermediary objects
keep <- c("data", "pay_cols")
rm(list = setdiff(ls(envir = .GlobalEnv), keep), envir = .GlobalEnv)

## PART 3: Summary Statistics ----------------

### Summary dataframe ----
summary_stats <- function(x) {
  c(
    Min = round(min(x, na.rm = TRUE), 2),
    QRT1 = round(quantile(x, 0.25, na.rm = TRUE), 2),
    Median = round(median(x, na.rm = TRUE), 2),
    Mean = round(mean(x, na.rm = TRUE), 2),
    QRT3 = round(quantile(x, 0.75, na.rm = TRUE), 2),
    Max = round(max(x, na.rm = TRUE), 2),
    NAs = sum(is.na(x)),
    SD = round(sd(x, na.rm = TRUE), 2)
  )
}

# Initializing summary df
summary_df <- data.frame(
  Name = character(),
  Min = numeric(),
  QRT1 = numeric(),
  Median = numeric(),
  Mean = numeric(),
  QRT3 = numeric(),
  Max = numeric(),
  NAs = integer(),
  SD = numeric()
)

# finding numeric columns
num_cols <- c(names(data)[sapply(data, function(x) is.numeric(x) && length(unique(x)) > 40)], pay_cols)

# Filling summary df row-wise
for (name in num_cols[-1]) {
  x <- data[[name]]
  
  stats <- summary_stats(x)
  row   <- data.frame(
    Name = name,
    t(stats)
  )
  summary_df <- rbind(summary_df, row)
}

kable(
  summary_df,
  format = "latex",
  booktabs = TRUE,
  caption = "Summary statistics of continuous variables."
)

### Correlations ----

# compute matrix for all variables
corr_all <- cor(
  data[ , c('default', num_cols[-1])],
)

# plot it 
png(
  filename = "hw4/code/corr_all_num.png",
  width  = 10,
  height = 10,
  units  = "in",
  res = 400
)

corrplot::corrplot(
  corr_all,
  method = "color",      
  type = "upper",
  order = "hclust",     
  addCoef.col = "black",
  tl.col = "black",    
  tl.srt = 45,         
  diag = FALSE         
)

dev.off()

### Categorical variables ----

# choose categorical predictors (without the outcome)
cat_cols <- c("education", "marriage", "sex") 
pay_cols <- colnames(data)[str_starts(colnames(data), "^pay_[0-9]+")] 
cat_vars <- c(cat_cols, pay_cols)

#### Share of defaults for each category of cat_vars ----

# summarise defaults by category and variable
share_defaults_category_df <- data %>%
  mutate(
    across(
      all_of(cat_vars),
      ~ factor(.x, ordered = FALSE)
    )
  ) %>%
  pivot_longer(
    cols = all_of(cat_vars),
    names_to  = "variable",
    values_to = "category"
  ) %>%
  group_by(variable, category) %>%
  summarise(
    n = n(),                       
    n_default = sum(default == 1), 
    default_rate = n_default / n,  
    .groups = "drop"
  )

# plot with counts and proportions above bars
share_defaults_category_plot <- ggplot(
  share_defaults_category_df,
  aes(x = category, y = default_rate)
) +
  geom_col() +
  geom_text(
    aes(
      label = paste0(
        n_default, " (",
        scales::percent(default_rate, accuracy = 0.1), ")"
      )
    ),
    vjust = -0.3,
    size  = 3
  ) +
  facet_wrap(~ variable, scales = "free_x") +
  labs(
    x = NULL,
    y = "Proportion of defaults",
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

share_defaults_category_plot

# save plot to file
ggsave(
  filename = "hw4/code/share_defaults_category.png",
  plot = share_defaults_category_plot,
  width = 14,
  height = 8
)


#### Distribution of default observations for each categorical variable ---- 

# overall default stats for subtitle
overall_default <- data %>%
  summarise(
    n_default = sum(default == 1),
    n_total = n()
  ) %>%
  mutate(
    prop_default = n_default / n_total
  )

n_default <- overall_default$n_default
prop_default <- overall_default$prop_default

# build data for plot: P(category | default = 1)
plot_defaults_by_cat <- data %>%
  filter(default == 1) %>%
  mutate(
    across(
      everything(),
      ~ factor(.x, ordered = FALSE)
    )
  ) %>% 
  pivot_longer(
    cols = all_of(cat_vars),
    names_to  = "variable",
    values_to = "category"
  ) %>%
  group_by(variable, category) %>%
  summarise(
    n_default_cat = n(),                               
    .groups = "drop_last"
  ) %>%
  mutate(
    prop_within_default = n_default_cat / sum(n_default_cat)  
  ) %>%
  ungroup()

# plot: distribution of defaults across categories, facetted by variable
p_defaults_dist <- ggplot(
  plot_defaults_by_cat,
  aes(x = category, y = prop_within_default)
) +
  geom_col() +
  geom_text(
    aes(
      label = paste0(
        n_default_cat, " (",
        scales::percent(prop_within_default, accuracy = 0.1), ")"
      )
    ),
    vjust = -0.3,
    size  = 3
  ) +
  facet_wrap(~ variable, scales = "free_x") +
  labs(
    x = NULL,
    y = "Share of all defaults",
    subtitle = paste0(
      "Overall defaults: ", n_default, " (",
      scales::percent(prop_default, accuracy = 0.1), " of all observations)"
    )
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

p_defaults_dist

# save plot to file
ggsave(
  filename = "hw4/code./p_defaults_dist.png",
  plot = p_defaults_dist,
  width = 14,
  height = 8
)

# Cleaning intermediary objects
keep <- c("data", "cat_cols", "pay_cols", "cat_vars")
rm(list = setdiff(ls(envir = .GlobalEnv), keep), envir = .GlobalEnv)

## Part 4: Feature engineering and model selection ----------------

### 4.1 Feature engineering ----

#### Regrouping of repayment status variables ---- 

recode_pay_group <- function(x) {
  case_when(
    x == -2 ~ "no_use",
    x == -1 ~ "paid_duly",
    x == 0 ~ "min_pay",
    x %in% c(1, 2) ~ "delay_1_2",
    x >= 3 ~ "delay_3plus"
  )
}

features <- data %>% 
  mutate(
    across(
      all_of(pay_cols),
      recode_pay_group
      )
  )


#### Aggregating 6-months repayment status variables per client  ---- 

features <- features %>%
  rowwise() %>%
  mutate(
    n_no_use = sum(c_across(all_of(pay_cols)) == "no_use", na.rm = TRUE),
    n_paid_duly = sum(c_across(all_of(pay_cols)) == "paid_duly", na.rm = TRUE),
    n_min_pay = sum(c_across(all_of(pay_cols)) == "min_pay", na.rm = TRUE),
    n_late_1_2 = sum(c_across(all_of(pay_cols)) == "delay_1_2", na.rm = TRUE),
    n_late_3plus = sum(c_across(all_of(pay_cols)) == "delay_3plus", na.rm = TRUE)
  ) %>%
  ungroup() 

#### Aggregating bill and repayment amounts per client ---- 
bill_cols <- colnames(features)[str_starts(colnames(features), "^bill_amt[0-9]+")]
pay_amount_cols <- colnames(features)[str_starts(colnames(features), "^pay_amt[0-9]+")]
num_cols <- c(bill_cols, pay_amount_cols, pay_cols, 'id', 'limit_bal', 'age')

features <- features %>%
  rowwise() %>%
  mutate(
    # bill amounts summary across all bill_amt months
    bill_median = median(c_across(all_of(bill_cols)), na.rm = TRUE),
    bill_sd = sd(c_across(all_of(bill_cols)), na.rm = TRUE),
    bill_max = max(c_across(all_of(bill_cols)), na.rm = TRUE),
    # payment amounts summary across all pay_amt months
    pay_median = median(c_across(all_of(pay_amount_cols)), na.rm = TRUE),
    pay_sd = sd(c_across(all_of(pay_amount_cols)), na.rm = TRUE),
    pay_max = max(c_across(all_of(pay_amount_cols)), na.rm = TRUE)
  ) %>%
  ungroup() 

#### Designing credit utilization ratio ---- 

features <- features %>%
  mutate(
    cred_util_rat_median = bill_median / limit_bal
  )

### 4.2 New feature correlations ---- 
new_num_features <- features %>%
  select(-all_of(num_cols)) %>%
  select(where(is.numeric))

corr_features <- cor(new_num_features)

png(
  filename = "hw4/code/corr_features.png",
  width = 9,
  height = 9,
  units = "in",
  res = 400
)

corr_features_map <- corrplot::corrplot(
  corr_features,
  method = "color",      
  type = "upper",
  order = "hclust",     
  addCoef.col = "black",
  tl.col = "black",    
  tl.srt = 45,         
  diag = FALSE         
)

dev.off()

# Cleaning intermediary objects
keep <- c("data", "features")
rm(list = setdiff(ls(envir = .GlobalEnv), keep), envir = .GlobalEnv)

## Part 5: Estimating models ----------------

### 5.1 Estimating main models ----

#### Logit model ---- 
model_logit <- glm(
  default ~ sex + pay_median + cred_util_rat_median +
    pay_1 + n_late_1_2 + n_late_3plus,
  family = binomial(link = "logit"),
  data = features
)

#### Probit model ---- 
model_probit <- glm(
  default ~ sex + pay_median + cred_util_rat_median +
    pay_1 + n_late_1_2 + n_late_3plus,
  family = binomial(link = "probit"),
  data = features
)

#### Calculating metrics ---- 

# Null models (intercept only)
intercept_logit  <- glm(default ~ 1, family = binomial(link = "logit"),  data = features)
intercept_probit <- glm(default ~ 1, family = binomial(link = "probit"), data = features)

# Pseudo-R^2
pR2_logit  <- 1 - as.numeric(logLik(model_logit)  / logLik(intercept_logit))
pR2_probit <- 1 - as.numeric(logLik(model_probit) / logLik(intercept_probit))

# AIC
aic_logit  <- AIC(model_logit)
aic_probit <- AIC(model_probit)

# BIC
bic_logit  <- BIC(model_logit)
bic_probit <- BIC(model_probit)

# Predicted probabilities
p_logit  <- predict(model_logit,  type = "response")
p_probit <- predict(model_probit, type = "response")

# class predictions at threshold 0.5
yhat_logit  <- ifelse(p_logit  >= 0.5, 1, 0)
yhat_probit <- ifelse(p_probit >= 0.5, 1, 0)

# Accuracy
acc_logit  <- mean(yhat_logit  == features$default)
acc_probit <- mean(yhat_probit == features$default)

### 5.2 Trying different set of predictors ----

#### Logit model 2---- 
model_logit2 <- glm(
  default ~ limit_bal + sex + pay_median + pay_sd + pay_1 + bill_max,
  family = binomial(link = "logit"),
  data = features
)

#### Probit model 2---- 
model_probit2 <- glm(
  default ~ limit_bal + sex + pay_median + pay_sd + pay_1 + bill_max,
  family = binomial(link = "probit"),
  data = features
)

#### Calculating metrics 2----

# Pseudo-R^2
pR2_logit2  <- 1 - as.numeric(logLik(model_logit2)  / logLik(intercept_logit))
pR2_probit2 <- 1 - as.numeric(logLik(model_probit2) / logLik(intercept_probit))

# AIC
aic_logit2  <- AIC(model_logit2)
aic_probit2 <- AIC(model_probit2)

# BIC
bic_logit2  <- BIC(model_logit2)
bic_probit2 <- BIC(model_probit2)

# predicted probabilities
p_logit2  <- predict(model_logit2,  type = "response")
p_probit2 <- predict(model_probit2, type = "response")

# Class predictions at threshold 0.5
yhat_logit2  <- ifelse(p_logit2  >= 0.5, 1, 0)
yhat_probit2 <- ifelse(p_probit2 >= 0.5, 1, 0)

# Accuracy
acc_logit2  <- mean(yhat_logit2  == features$default)
acc_probit2 <- mean(yhat_probit2 == features$default)

library(stargazer)

stargazer(
  model_logit,    
  model_probit,   
  model_logit2,   
  model_probit2,  
  title = "Logit and probit estimates for default",
  dep.var.labels = "Default (1 = default)",
  
  column.labels= c("Logit main", "Probit main", "Logit 2", "Probit 2"),
  column.separate = c(1, 1, 1, 1),
  model.numbers = FALSE,   
  
  header = FALSE,
  type = "latex",
  style = "all",
  report = "vcsp*",
  omit.stat = c("LL", "ser", "f", "aic", "bic", "rsq"),
  
  column.sep.width = "1pt",  
  font.size = "scriptsize",
  no.space = FALSE,
  
  add.lines = list(
    c("Pseudo $R^2$",
      sprintf("%.3f", pR2_logit),
      sprintf("%.3f", pR2_probit),
      sprintf("%.3f", pR2_logit2),
      sprintf("%.3f", pR2_probit2)),
    c("Accuracy",
      sprintf("%.3f", acc_logit),
      sprintf("%.3f", acc_probit),
      sprintf("%.3f", acc_logit2),
      sprintf("%.3f", acc_probit2)),
    c("AIC",
      sprintf("%.1f", aic_logit),
      sprintf("%.1f", aic_probit),
      sprintf("%.1f", aic_logit2),
      sprintf("%.1f", aic_probit2)),
    c("BIC",
      sprintf("%.1f", bic_logit),
      sprintf("%.1f", bic_probit),
      sprintf("%.1f", bic_logit2),
      sprintf("%.1f", bic_probit2))
  )
)

# Cleaning intermediary objects
keep <- c("data", "features", "model_probit")
rm(list = setdiff(ls(envir = .GlobalEnv), keep), envir = .GlobalEnv)


## Part 6 Reporting effects ----------------
library(marginaleffects)

### 6.1 Average Partial Effects ----

ape_cont <- avg_slopes(
  model_probit,
  variables = c("pay_median", "cred_util_rat_median", "n_late_1_2", "n_late_3plus"),
  type = "response"   # probability scale
)
ape_cont

ape_disc <- avg_comparisons(
  model_probit,
  variables = c("sex", "pay_1"),
  type = "response"
  )

ape_disc

ape_cont_df <- as.data.frame(ape_cont) %>%
  transmute(
    Variable = term,
    APE = estimate,
    SE = std.error,
    z = statistic,
    p = p.value,
    CI_low  = conf.low,
    CI_high = conf.high
  )

stargazer(
  ape_cont_df,
  type = 'latex',
  summary = FALSE,
  rownames = FALSE,
  digits = 4,
  title = "Average Partial Effects for continious variables - Main probit",
  label = "tab:ape_cont"
)


ape_disc_df <- as.data.frame(ape_disc) %>%
  transmute(
    Var_name = term,
    Comparison = contrast,
    APE = estimate,
    SE = std.error,
    z = statistic,
    p = p.value,
    CI_low = conf.low,
    CI_high = conf.high
  )

stargazer(
  ape_disc_df,
  type = 'latex',
  summary = FALSE,
  rownames = FALSE,
  digits = 4,
  title = "Average Partial Effects for discrete variables - Main probit",
  label = "tab:ape_disc"
)

# Cleaning intermediary objects
keep <- c("data", "features", "model_probit")
rm(list = setdiff(ls(envir = .GlobalEnv), keep), envir = .GlobalEnv)


## 7. Out-of-sample performance  ----------------

# Setting seed for replicability
set.seed(42)

### 7.1 Creating train/test split (stratified for defaulted observations) ----
idx1 <- which(features$default == 1)
idx0 <- which(features$default == 0)

# Defining indices
train_idx <- c(
  sample(idx1, floor(0.7 * length(idx1))),
  sample(idx0, floor(0.7 * length(idx0)))
)

# Subsetting the main dataset
train <- features[train_idx, ]
test  <- features[-train_idx, ]

### 7.2 Fitting probit on train ----
model_probit_train <- glm(
  default ~ sex + pay_median + cred_util_rat_median + pay_1 + n_late_1_2 + n_late_3plus,
  family = binomial(link = "probit"),
  data = train
)

### 7.3 Computing metrics and confusion matrices for train and test ----

#### Train data ----

# Predictions on train
p_hat_tr <- predict(model_probit, newdata = train, type = "response")
y_hat_tr <- ifelse(p_hat_tr >= 0.5, 1, 0)

# Confusion matrix
conf_mat_tr <- table(Predicted = y_hat_tr, Actual = train$default)

# Extracting counts
TP <- conf_mat_tr["1","1"]
TN <- conf_mat_tr["0","0"]
FP <- conf_mat_tr["1","0"]
FN <- conf_mat_tr["0","1"]

# Metrics table
metrics_train_df <- data.frame(
  Metric = c(
    "Accuracy",
    "Sensitivity (True Positive Rate)",
    "Specificity (True Negative Rate)",
    "Precision (Positive Predictive Value)"
  ),
  Value = c(
    (TP + TN) / sum(conf_mat_tr),
    TP / (TP + FN),
    TN / (TN + FP),
    TP / (TP + FP)
  )
)

stargazer(
  metrics_train_df,
  type = 'latex',
  summary = FALSE,
  rownames = FALSE,
  digits = 4,
  title = "Prediction metrics for train data",
  label = "tab:metrics_train"
)

#### Test data ----

# Predictions on test data
p_hat <- predict(model_probit, newdata = test, type = "response")
y_hat <- ifelse(p_hat >= 0.5, 1, 0)

# Confusion matrix
conf_mat <- table(Predicted = y_hat, Actual = test$default)
conf_mat_df <- as.data.frame.matrix(conf_mat)
rownames(conf_mat_df) <- c("Pred 0", "Pred 1")
colnames(conf_mat_df) <- c("Actual 0", "Actual 1")

stargazer(
  conf_mat_df,
  type = 'latex',
  summary = FALSE,
  rownames = TRUE,
  digits = 0,
  title = "Confusion matrix for test set",
  label = "tab:conf_mat"
)

# Metrics table 
TP <- conf_mat["1","1"]
TN <- conf_mat["0","0"]
FP <- conf_mat["1","0"]
FN <- conf_mat["0","1"]

metrics_df <- data.frame(
  Metric = c(
    "Accuracy",
    "Sensitivity (True Positive Rate)",
    "Specificity (True Negative Rate)",
    "Precision (Positive Predictive Value)"
  ),
  Value  = c(
    (TP + TN) / sum(conf_mat),
    TP / (TP + FN),
    TN / (TN + FP),
    TP / (TP + FP)
  )
)

stargazer(
  metrics_df,
  type = 'latex',
  summary = FALSE,
  rownames = FALSE,
  digits = 4,
  title = "Prediction metrics on test set",
  label = "tab:metrics"
)
