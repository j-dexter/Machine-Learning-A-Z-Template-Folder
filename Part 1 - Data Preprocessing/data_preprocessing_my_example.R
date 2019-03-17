# Data Preprocessing Template

# 1.0 Setup ----

# Load libraries
library(tidyverse)
library(rsample)

# Importing the dataset
dataset_raw_tbl = read_csv('Data.csv')


# 2.0 Preprocess Data ----


dataset_tbl <- dataset_raw_tbl %>% 
    
    # Replace rows w/missing age with avg. age
    mutate(Age = if_else(is.na(Age),
                    mean(Age, na.rm = T),
                    Age)) %>% 
    
    # Replace rows w/missing salary with avg. salary
    mutate(Salary = if_else(is.na(Salary),
                        mean(Salary, na.rm = T),
                        Salary)) %>% 
    
    # Encode categorical by converting to factor (forcats library way)
    mutate(Country_Fct   = factor(Country,
                                  levels = c("France", "Spain", "Germany"),
                                  labels = c(1, 2, 3))) %>% 
    
    # Encode Using factor() to set labels 0 & 1 (base R way)
    mutate(Purchased_Fct = factor(Purchased,
                                 levels = c("No", "Yes"),
                                 labels = c(0, 1))) %>% 
    
    # Reorder columns
    select(Age, Salary, contains("Cou"), contains("Pur"))

dataset_tbl


# 3.0 Split Data ----

# Splitting the dataset into the Training set and Test set
set.seed(123)
split_obj <- initial_split(data = dataset_tbl,
                           prop = 0.7,
                           strata = "Purchased")

train_tbl <- split_obj %>% training()
test_tbl  <- split_obj %>% testing()


# 4.0 Feature Scaling ----

# Puts numerical features on the same scale
# This is done so that higher values in one feature, do not dominate small 
    # values in another feature (being squared in pythagorean for euclidian distance)
# Transforms variables so that they are in the same range, same scale so that
    # no varible is dominated by the other.

# Could do it this way with base R: Leaves weird output for column name
train_tbl %>% 
    
    mutate(Age_Scaled    = scale(Age),
           Salary_Scaled = scale(Salary))

# A better way with Recipes (can do entire preprocessing step pipeline)

# Link to Recipes: https://tidymodels.github.io/recipes/
# Simple example:  https://tidymodels.github.io/recipes/articles/Simple_Example.html

library(recipes)

# Define recipe object w/outcome(s) and predictors
recipe_obj <- recipe(Purchased ~ ., data = dataset_raw_tbl) %>% 
    
    step_meanimpute(all_numeric()) %>% 
    step_center(all_numeric()) %>%  # Centers on zero
    step_scale(all_numeric()) %>%   # Scales range to -1, +1
    step_dummy(all_nominal()) %>% 
    
    # Prepare recipe: This does the intermediate calculations
    prep()

# Apply recipe to train/test sets
train_preprocessed_tbl <- bake(recipe_obj, new_data = train_tbl)
test_preprocessed_tbl <- bake(recipe_obj, new_data = test_tbl)

hist(train_preprocessed_tbl$Age)


# training_set = scale(training_set)
# test_set = scale(test_set)





