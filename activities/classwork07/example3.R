




# create training and testing sets
index <- initial_split(iris)
training_data <- training(index)
testing_data <- testing(index)

# preprocess

data_recipe <- recipe(Species ~ Sepal.Length + Sepal.Width + Petal.Width + Petal.Length, data = training_data) |>
  step_normalize(all_numeric_predictors())


example <- data_recipe |> 
              prep() |> 
              bake(new_data = NULL)



# create a model specification (long way)

knn_model <- nearest_neighbor(mode = "classification", engine = "kknn")


Iris_workflow <- workflow(preprocessor = data_recipe, spec = knn_model)

# train the model and evaluate

final_model <- last_fit(Iris_workflow, index)
final_model$.metrics
final_model$.predictions












