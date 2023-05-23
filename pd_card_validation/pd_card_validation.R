


# Place tar.gz file in project at "/renv/cellar/KAscore_1.0.0.tar.gz"

# Install {KAscore} from the "cellar"
# renv::install(
#   packages = "KAscore@1.0.0",
#   dependencies = c("Depends", "Imports", "LinkingTo", "Suggests")
# )

# There is a current bug with {KAscore} that requires you to install 
# {rstudioapi} and {crayon}
# install.packages(c("rstudioapi", "crayon"))

dplyr::glimpse(KAscore::loans)

df <- KAscore::loans

# Make sure to reverse the factor levels of the dependent variable
# See ?
df$default_status <- factor(
  df$default_status, 
  levels = c("good", "bad")
)

# Calculate Weight-of-Evidence (for categorical variables)
KAscore::woe(
  data = df,
  outcome = default_status,
  predictors = dplyr::where(is.factor)
)

# Bin the 'loan_amount' variable by quantile & view breaks
KAscore::bin_quantile(
  df$loan_amount, 
  n_bins = 4
) |> levels()

# Create a list of 4 different "breaks" scenarios
breaks <- list(
  
  `c(-Inf, 13000, 23000, 39000, Inf)` = c(-Inf, 13000, 23000, 39000, Inf),
  
  `c(-Inf, 13500, 23500, 39500, Inf)` = c(-Inf, 13500, 23500, 39500, Inf),
  
  `c(-Inf, 14000, 24000, 40000, Inf)` = c(-Inf, 14000, 24000, 40000, Inf),
  
  bin_quantile = c(-Inf ,13655, 23195, 39722, Inf)
)

# Define a function to calculate the information value statistic that we can map
# over the list of breaks
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

# Plot the Information Value statistic for "LoanAmount" across the different 
# binning scenarios
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
  ggplot2::theme_minimal() + 
  ggplot2::theme(
    panel.grid.major.y = ggplot2::element_blank()
  )


# More EDA -- plot distribution of continuous independent variable by dependent 
# variable (this can help guide binning)
df |> 
  ggplot2::ggplot(
    ggplot2::aes(
      x = loan_amount,
      color = default_status
    )
  ) + 
  ggplot2::geom_density(linewidth = 1) + 
  ggplot2::scale_x_continuous(labels = scales::label_dollar()) + 
  ggplot2::labs(
    x = "Loan Amount",
    y = "Density",
    title = "Distribution of Loan Amount by Default Status"
  ) + 
  ggplot2::theme_minimal() + 
  # Add bin breaks
  ggplot2::geom_vline(
    xintercept = breaks[[1]], 
    linetype = "dashed"
  )



map_woe <- function(data, breaks, outcome, predictor) {
  
  df <- data |> 
    dplyr::mutate(
      {{predictor}} := cut(df$loan_amount, breaks = breaks)
    )
  
  KAscore::woe(
    data = df, 
    outcome = {{outcome}}, 
    predictors = {{predictor}}, 
    verbose = FALSE
  )
  
}

map_woe(
  df, 
  breaks = breaks[[1]], 
  outcome = default_status, 
  predictor = loan_amount
)

KAscore::woe(
  df, 
  outcome = default_status, 
  predictors = collateral_type,
  verbose = FALSE
) |> 
  dplyr::select(class, dplyr::starts_with("n_"), woe) |> 
  dplyr::arrange(woe)

