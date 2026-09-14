
library(tidyverse)
library(ggplot2)
library(skimr)
library(knitr)

data <- read.csv("extracted_dataset.csv", sep = ',') %>%
  mutate(state = case_when(
    PR_state == -1 ~ "closed",
    PR_state == 0 ~ "open",
    PR_state == 1 ~ "merged",
  )) %>% rename(
    median = Polarity_median,
    mean = Polarity_mean,
    mode = Polarity_mode,
    LOC = class_loc,
    CBO = class_cboModified,
    WMC = class_wmc,
    LCOM = `class_lcom.`,
    methods = total_methods_qtt,
    mLOC = method_loc,
    mCBO = method_cboModified,
    mWMC = method_wmc,
    parameters = parameters_qtt,
    added = lines_added,
    removed = lines_removed,
    modified = modified_classes
  )

# Correlation analysis 

library(tidyverse)

sentiment_vars <- c(
  "mean",
  "median",
  "mode",
  "added",
  "removed",
  "modified",
  "LOC",
  "CBO",
  "WMC",
  "LCOM",
  "methods",
  "mLOC",
  "mCBO",
  "mWMC",
  "parameters"
)

corr_matrix <- data %>%
  select(all_of(sentiment_vars)) %>%
  cor(
    use = "pairwise.complete.obs",
    method = "spearman"
  )

sentiment_corr <- corr_matrix[c("mean", "median", "mode"),
                              setdiff(colnames(corr_matrix),
                                      c("mean", "median", "mode"))]

corr_long <- sentiment_corr %>%
  as.data.frame() %>%
  rownames_to_column("Sentiment") %>%
  pivot_longer(
    -Sentiment,
    names_to = "Metric",
    values_to = "Correlation"
  ) %>%
  mutate(
    Metric = factor(Metric, levels = sentiment_vars),
    Sentiment = factor(
      Sentiment,
      levels = c("mean", "median", "mode")
    )
  )

ggplot(
  corr_long,
  aes(
    x = Metric,
    y = Sentiment,
    fill = Correlation
  )
) +
  geom_tile(color = "white") +
  geom_text(
    aes(label = sprintf("%.2f", Correlation)),
    size = 3
  ) +
  scale_fill_gradient2(
    low = "#B2182B",
    mid = "white",
    high = "#2166AC",
    midpoint = 0,
    limits = c(-1, 1)
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  ) +
  labs(
    title = "",
    x = NULL,
    y = NULL,
    fill = "Spearman ρ"
  )
