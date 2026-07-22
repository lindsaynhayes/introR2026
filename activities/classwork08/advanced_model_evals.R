extract_fit_engine(lr_fit) |> 
  coef(lr_engine) |>
  as.data.frame() |>
  rownames_to_column("y.level") |>
  pivot_longer(
    cols = -y.level,
    names_to = "term",
    values_to = "estimate"
  ) |>
  filter(term != "(Intercept)") |>
  ggplot(aes(x = term, y = estimate, fill = term)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ y.level) +   # separate panel for each species
  coord_flip() +
  labs(
    x = "Predictor",
    y = "Coefficient estimate",
    title = "Logistic Regression: Predictor Coefficients by Species"
  ) +
  theme_minimal()




rf_engine <- extract_fit_engine(rf_fit)

# importance scores 
rf_importance <- rf_engine$variable.importance

rf_importance





