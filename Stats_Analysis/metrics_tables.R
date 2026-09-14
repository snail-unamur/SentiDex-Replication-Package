library(tidyverse)
library(knitr)
library(kableExtra)
library(ggplot2)

data <- read.csv("extracted_dataset.csv", sep = ',') %>%
  mutate(state = case_when(
    PR_state == -1 ~ "closed",
    PR_state == 0  ~ "open",
    PR_state == 1  ~ "merged"
  )) %>%
  rename(
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

vars <- c(
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

dir.create("sparklines", showWarnings = FALSE)

create_sparkline <- function(x, filename) {
  
  x <- na.omit(x)
  
  p <- ggplot(
    data.frame(value = x),
    aes(x = value)
  ) +
    geom_histogram(
      bins = 15,
      fill = "#4F81BD",
      color = NA
    ) +
    theme_void()
  
  ggsave(
    filename,
    p,
    width = 1.2,
    height = 0.35,
    dpi = 300,
    bg = "white"
  )
}

build_table <- function(df, state_name) {
  
  results <- map_dfr(vars, function(v) {
    
    values <- df[[v]]
    
    img <- sprintf(
      "sparklines/%s_%s.png",
      state_name,
      v
    )
    
    create_sparkline(values, img)
    
    tibble(
      Variable = v,
      N = sum(!is.na(values)),
      Mean = mean(values, na.rm = TRUE),
      SD = sd(values, na.rm = TRUE),
      Q1 = quantile(values, .25, na.rm = TRUE),
      Median = median(values, na.rm = TRUE),
      Q3 = quantile(values, .75, na.rm = TRUE),
      Distribution = sprintf("\\includegraphics[width=2cm]{%s}", img)
    )
  })
  
  results %>%
    mutate(
      Variable = recode(
        Variable,
        added      = "Lines Added",
        removed    = "Lines Removed",
        modified   = "Modified Classes",
        LOC        = "Class LOC",
        CBO        = "Class CBO",
        WMC        = "Class WMC",
        LCOM       = "Class LCOM",
        methods    = "Methods",
        mLOC       = "Method LOC",
        mCBO       = "Method CBO",
        mWMC       = "Method WMC",
        parameters = "Parameters"
      )
    )
}

closed_tbl <- build_table(
  filter(data, state == "closed"),
  "closed"
)

open_tbl <- build_table(
  filter(data, state == "open"),
  "open"
)

merged_tbl <- build_table(
  filter(data, state == "merged"),
  "merged"
)

kable(
  closed_tbl,
  format = "latex",
  escape = FALSE,
  booktabs = TRUE,
  digits = 2,
  caption = "Descriptive statistics for the closed PRs."
) %>%
  kable_styling(latex_options = "hold_position")

kable(
  open_tbl,
  format = "latex",
  escape = FALSE,
  booktabs = TRUE,
  digits = 2,
  caption = "Descriptive statistics for the open PRs."
) %>%
  kable_styling(latex_options = "hold_position")

kable(
  merged_tbl,
  format = "latex",
  escape = FALSE,
  booktabs = TRUE,
  digits = 2,
  caption = "Descriptive statistics for the merged PRs."
) %>%
  kable_styling(latex_options = "hold_position")
