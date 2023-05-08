


# Place tar.gz file in project at "/renv/cellar/KAscore_1.0.0.tar.gz"

# Install {KAscore} from the "cellar"
renv::install("KAscore@1.0.0")

dplyr::glimpse(KAscore::loans)

df <- KAscore::loans

KAscore::woe(
  data = df,
  outcome = default_status,
  predictors = -c(loan_id, default_status)
)

df$default_status <- factor(
  df$default_status, 
  levels = c("good", "bad")
)

# Bin the 'loan_amount' variable
KAscore::bin_quantile(df$loan_amount) |> levels()

breaks <- list(
  `c(-Inf, 13000, 23000, 39000, Inf)` = c(-Inf, 13000, 23000, 39000, Inf),
  `c(-Inf, 13500, 23500, 39500, Inf)` = c(-Inf, 13500, 23500, 39500, Inf),
  `c(-Inf, 14000, 24000, 40000, Inf)` = c(-Inf, 14000, 24000, 40000, Inf),
  bin_quantile = c(-Inf ,13655, 23195, 39722, Inf)
)


map_iv <- function(data, breaks, outcome, predictor) {
  
  df <- data |> 
    dplyr::mutate(
      {{predictor}} := cut(df$loan_amount, breaks = breaks)
    )
  
  KAscore::iv(
    data = df, 
    outcome = {{outcome}}, 
    predictors = {{predictor}}, 
    verbose = FALSE, 
    labels = TRUE
  )
  
}

# install.packages("ggplot2")

purrr::map(
  .x = breaks, 
  .f = function(x) map_iv(
    data = df, 
    breaks = x, 
    outcome = default_status, 
    predictor = loan_amount
  )
) |> 
  purrr::list_rbind(names_to = "breaks") |> 
  dplyr::mutate(label = paste0(
    round(iv, 4), " (", label, ")"
  )) |> 
  ggplot2::ggplot(
    ggplot2::aes(
      x = reorder(breaks, iv),
      y = iv 
      # fill = label
    )
  ) + 
  ggplot2::geom_col(width = 0.6, fill = "gray70") +
  ggplot2::labs(
    x = "",
    y = "Information Value",
    title = "Variable: Loan Amount", 
    subtitle = "Information Value by Bin Breaks"
  ) +
  ggplot2::coord_flip() + 
  ggplot2::geom_text(
    ggplot2::aes(label = label),
    hjust = -0.05,
    nudge_x = 0
  ) +
  ggplot2::scale_y_continuous(limits = c(NA, 0.20)) +
  ggplot2::theme_minimal()
