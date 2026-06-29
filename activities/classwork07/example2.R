---
title: "Untitled"
editor_options: 
  chunk_output_type: console
---


```{r}
library(tidymodels)
```



```{r}
head(diamonds)


pca_rec <- recipe(price~., data = diamonds) |>
  #step_dummy(all_nominal_predictors()) |>
  step_ordinalscore(all_nominal_predictors()) |>
  step_normalize(all_numeric()) |>
  step_pca(all_predictors())

pca_rec

pca_prep <- prep(pca_rec)

pca_prep

pca_bake <- bake(pca_prep, new_data = NULL)

pca_bake %>%
  ggplot(aes(x = PC1, y = PC2, color = price)) + geom_point()
  

```



```{r}

library(recipes)
penguin_recipe <-
  recipe(~., data = penguins) %>% 
  update_role(species, island, sex, new_role = "id") %>% 
  step_naomit(all_predictors()) %>% 
  step_normalize(all_predictors()) %>%
  step_pca(all_predictors(), id = "pca") %>% 
  prep()

penguin_pca <- 
  penguin_recipe %>% 
  tidy(id = "pca") 

penguin_pca


penguin_recipe %>% 
  tidy(id = "pca", type = "variance") %>% 
  dplyr::filter(terms == "percent variance") %>% 
  ggplot(aes(x = component, y = value)) + 
  geom_col(fill = "#b6dfe2") + 
  xlim(c(0, 5)) + 
  ylab("% of total variance")



penguin_pca %>%
  ggplot(aes(abs(value), terms, fill = value > 0)) +
  geom_col() +
  facet_wrap(~component, scales = "free_y") +
  scale_fill_manual(values = c("#b6dfe2", "#0A537D")) +
  labs(
    x = "Absolute value of contribution",
    y = NULL, fill = "Positive?"
  ) 


penguin_pca_bake <- bake(penguin_recipe, new_data = NULL)

pca_wider <- penguin_pca %>% 
  tidyr::pivot_wider(names_from = component, id_cols = terms)

pca_plot <- penguin_pca_bake |> 
  ggplot(aes(PC1, PC2)) +
  geom_point(aes(color = species, shape = species), 
             alpha = 0.8, 
             size = 2) +
  scale_colour_manual(values = c("darkorange","purple","cyan4")) 



arrow_style <- arrow(length = unit(.05, "inches"),
                     type = "closed")

pca_plot +
  geom_segment(data = pca_wider,
               aes(xend = PC1, yend = PC2), 
               x = 0, 
               y = 0, 
               arrow = arrow_style) + 
  geom_text(data = pca_wider,
            aes(x = PC1, y = PC2, label = terms), 
            hjust = 0, 
            vjust = 1,
            size = 5, 
            color = '#0A537D') 
```


```{r}
pca_plot <- penguin_pca_bake |> 
  ggplot(aes(PC1, PC3)) +
  geom_point(aes(color = species, shape = species), 
             alpha = 0.8, 
             size = 2) +
  scale_colour_manual(values = c("darkorange","purple","cyan4")) 



arrow_style <- arrow(length = unit(.05, "inches"),
                     type = "closed")

pca_plot +
  geom_segment(data = pca_wider,
               aes(xend = PC1, yend = PC3), 
               x = 0, 
               y = 0, 
               arrow = arrow_style) + 
  geom_text(data = pca_wider,
            aes(x = PC1, y = PC3, label = terms), 
            hjust = 0, 
            vjust = 1,
            size = 5, 
            color = '#0A537D') 
```



