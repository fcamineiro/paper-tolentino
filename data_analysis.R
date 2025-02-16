library(readxl)
library(dplyr)
library(tidyr)
library(tibble)
library(stringr)

# Data engineering --------------------------------------------------------
# Loading excel files
pre_test_data <- read_excel("data/pre.xlsx")
post_test_data <- read_excel("data/post.xlsx")

# Excluding unnecessary data
pre_test_data <- pre_test_data %>%
  select(-c(1:7, 28:29))

post_test_data <- post_test_data %>%
  select(-c(1:7, 28))

# Renaming column names
pre_test_data <- pre_test_data %>%
  rename_with(~ sprintf("%02d", as.integer(str_extract(., "\\d+"))))

post_test_data <- post_test_data %>%
  rename_with(~ sprintf("%02d", as.integer(str_extract(., "\\d+"))))

# Convert Likert Scale
likert_map <- c("Discordo totalmente" = 1,
                "Discordo" = 2,
                "Não concordo nem discordo" = 3,
                "Concordo" = 4,
                "Concordo totalmente" = 5)

pre_test_num <- pre_test_data %>%
  mutate(across(where(is.character), ~ as.integer(likert_map[trimws(.)])))

post_test_num <- post_test_data %>%
  mutate(across(where(is.character), ~ as.integer(likert_map[trimws(.)])))

# Adding ids
pre_test_num <- pre_test_num %>%
  mutate(part_id = row_number()) %>%
  relocate(part_id, .before = everything())

post_test_num <- post_test_num %>%
  mutate(part_id = row_number()) %>%
  relocate(part_id, .before = everything())

# Clean temporary values
rm(likert_map)

# Reshape data to tidy format
pre_long <- pre_test_num %>%
  pivot_longer(cols = -part_id, names_to = "question", values_to = "response") %>%
  mutate(time = "pre")

post_long <- post_test_num %>%
  pivot_longer(cols = -part_id, names_to = "question", values_to = "response") %>%
  mutate(time = "post")

tidy_data <- bind_rows(pre_long, post_long) %>%
  mutate(time = factor(time, levels = c("pre", "post"))) %>%
  group_by(part_id, question) %>%
  filter(n() == 2, all(!is.na(response))) %>%
  ungroup() %>%
  arrange(part_id, question, time)

wide_data <- tidy_data %>%
  pivot_wider(names_from = time, values_from = response)

# Statistical Analysis ----------------------------------------------------
# Descriptive stats
descriptive_summary <- tidy_data %>%
  group_by(question, time) %>%
  summarise(
    n = n(),
    mean = mean(response, na.rm = TRUE),
    sd = sd(response, na.rm = TRUE),
    median = median(response, na.rm = TRUE),
    min = min(response, na.rm = TRUE),
    max = max(response, na.rm = TRUE),
    variance = var(response, na.rm = TRUE),
    .groups = "drop"
  )

# Paired t-tests
library(purrr)
t_test_results <- wide_data %>%
  group_by(question) %>%
  summarise(
    t_test = list(t.test(pre, post, paired = TRUE))
  ) %>%
  mutate(
    t_statistic = map_dbl(t_test, ~ .x$statistic),
    degrees_freedom = map_dbl(t_test, ~ .x$parameter),
    p_value = map_dbl(t_test, ~ .x$p.value)
  )

significant_results <- t_test_results %>%
  filter(p_value < 0.05)