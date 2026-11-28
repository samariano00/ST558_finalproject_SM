#
# This is a Plumber API. You can run the API by clicking
# the 'Run API' button above.
#
# Find out more about building APIs with Plumber here:
#
#    https://www.rplumber.io/
#
# finalproject_SM.R

library(plumber)
library(tidyverse)

# I'm going to start by reading in the data and processing it as was done in the .qmd file.  Since this process has already been completed, I'm going to exclude my reasonsings behind what was done and copy the exact same processes.
store_data <- read_excel("../US Superstore data.xls")

store_parsed <- store_data |>
  mutate(
    `Order Date` = as.Date(`Order Date`),
    `Ship Date` = as.Date(`Ship Date`),
    `Postal Code` = as.character(`Postal Code`),
    `Ship Mode` = as.factor(`Ship Mode`),
    Segment = as.factor(Segment),
    State = as.factor(State),
    Region = as.factor(Region),
    Category = as.factor(Category),
    `Sub-Category` = as.factor(`Sub-Category`)
  )

# As the regression tree model was found to be the best fit, I'm going to include the process of creating that model, but not in the comparison to the random forest model.  No notes have been included in this process as they are detailed in the rendered github site.
model_rec <- recipe(Profit ~ Discount + `Sub-Category` + Segment, data = store_training) |>
  step_dummy(`Sub-Category`, Segment) |>
  step_normalize(Discount)

tree_mod <- decision_tree(tree_depth = tune(),
                          min_n = 20,
                          cost_complexity = tune()) |>
  set_engine("rpart") |>
  set_mode("regression")

tree_wkf <- workflow() |>
  add_recipe(model_rec) |>
  add_model(tree_mod)

tree_grid <- grid_regular(cost_complexity(),
                          tree_depth(),
                          levels = c(10,5))

tree_fit <- tree_wkf |>
  tune_grid(resamples = store_cv_folds,
            grid = tree_grid,
            metrics = metric_set(rmse))

tree_best_param <- select_best(tree_fit, metric = "rmse")

tree_final_wkf <- tree_wkf |>
  finalize_workflow(tree_best_param)

final_model <- tree_final_wkf |>
  fit(store_parsed)

#* @apiTitle Plumber Example API
#* @apiDescription Plumber example description.

#* Echo back the input
#* @param msg The message to echo
#* @get /echo
function(msg = "") {
    list(msg = paste0("The message is: '", msg, "'"))
}

#* Plot a histogram
#* @serializer png
#* @get /plot
function() {
    rand <- rnorm(100)
    hist(rand)
}

#* Return the sum of two numbers
#* @param a The first number to add
#* @param b The second number to add
#* @post /sum
function(a, b) {
    as.numeric(a) + as.numeric(b)
}

# Programmatically alter your API
#* @plumber
function(pr) {
    pr %>%
        # Overwrite the default serializer to return unboxed JSON
        pr_set_serializer(serializer_unboxed_json())
}
