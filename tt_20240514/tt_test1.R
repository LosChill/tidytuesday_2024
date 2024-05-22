library(tidytuesdayR)
library(tidyverse)
library(caret)
library(stringr)
library(vcd)
source("one_hot_tt1.R")

# Import data
coffee_survey <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-05-14/coffee_survey.csv')

# Trim out free-text data, fix "spend" typo ("spend" is not a noun, stop trying to make it one)
trimdata <- coffee_survey %>%
  select(-matches("other|specify|coffee_a|coffee_b|coffee_c|coffee_d|prefer")) %>%
  rename(monthly_expenditure = total_spend) %>%
  mutate(across(everything(), ~ if(is.character(.)) tolower(.) else .)) %>%
  mutate(across(c(age, cups, favorite, style, strength, roast_level, caffeine,
                  expertise, wfh, monthly_expenditure, taste, know_source,
                  most_paid, most_willing, value_cafe, spent_equipment,
                  value_equipment, gender, education_level, ethnicity_race,
                  employment_status, number_children, political_affiliation
                  ), as.factor)
         )

# List of columns with multi-value cells
hot_columns <- c("where_drink",
                 "brew",
                 "purchase",
                 "additions",
                 "dairy",
                 "sweetener",
                 "why_drink"
)

trimdata_factors_clean <- trimdata %>%
  select(-all_of(hot_columns)) %>%
  drop_na()

# Function to calculate Cramér's V
cramers_v <- function(x, y) {
  chi2 <- chisq.test(x, y)
  n <- sum(chi2$observed)
  phi2 <- chi2$statistic / n
  r <- length(unique(x))
  k <- length(unique(y))
  return(sqrt(phi2 / min(r - 1, k - 1)))
}

# Assuming trimdata_factors_clean is your cleaned data
factor_columns <- select(trimdata_factors_clean, -submission_id)

# Generate all combinations of pairs of factor columns
combinations <- combn(names(factor_columns), 2, simplify = FALSE)

# Calculate Cramér's V for each pair
results <- map_dfr(combinations, function(cols) {
  cramer_v <- cramers_v(factor_columns[[cols[1]]], factor_columns[[cols[2]]])
  tibble(
    var1 = cols[1],
    var2 = cols[2],
    cramers_v = cramer_v
  )
})

# Rank the pairs by Cramér's V
results <- results %>% arrange(desc(cramers_v))

exp_source <- trimdata_factors_clean %>%
  select(expertise, know_source)

ggplot(exp_source, aes(x = know_source, y = as.numeric(expertise), color = know_source)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  geom_jitter(width = 0.25, height = 0.5, alpha = 0.2) +
  scale_y_continuous(breaks = seq(0, 10, by = 1)) +
  labs(
    title = "Distribution of Knowledge of Source by Self-Reported Expertise",
    x = "Do You Know Where Your Coffee Comes From?",
    y = "Claimed Level of Coffee Expertise"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
  
exp_source_outliers <- trimdata_factors_clean %>%
  mutate(expertise = as.integer(expertise)) %>%
  filter(expertise > 7) %>%
  filter(str_detect(know_source, "no"))

