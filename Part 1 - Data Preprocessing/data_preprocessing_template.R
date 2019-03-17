# Data Preprocessing Template

# 1.0 Setup ----

# Libraries
library(tidyverse)
library(rsample)
library(tidyquant)
library(recipes)

# Load Data
path_data <- "Data.csv"
dataset_raw_tbl = read_csv(path_data)


# Splitting the dataset into the Training set and Test set
set.seed(123)
split_obj <- initial_split(data = dataset_raw_tbl,
                           prop = 0.7,
                           strata = "Purchased")

train_tbl <- split_obj %>% training()
test_tbl  <- split_obj %>% testing()


# 2.0 EDA for Preprocessing ----

# RUN FUNCTION AT BOTTOM OF SCRIPT

# Plot distributions to identify what needs scaling
plot_hist_facet(train_tbl)

# Identify skewed features that need transforming
skewed_feature_names <- train_tbl %>% 
    select_if(is.numeric) %>% 
    map_df(skewness) %>% 
    gather(factor_key = T) %>% 
    arrange(desc(value)) %>% 
    filter(value >= 1.0 | value <= -1.0) %>% 
    pull(key) %>% 
    as.character()


# 3.0 Data Preprocessing w/Recipes ----

# Recipes creates a data pre-processing pipeline (repeatable)

# Link to Recipes: https://tidymodels.github.io/recipes/
# Simple example:  https://tidymodels.github.io/recipes/articles/Simple_Example.html

# 3.1 Final Recipe ----

recipe_obj <- recipe(formula = Purchased ~ ., data = train_tbl) %>% 
    #step_zv(all_predictors()) %>%   # zero variance feature removal
    step_meanimpute(all_numeric()) %>% 
    #step_YeoJohnson(skewed_feature_names) %>% # applies transformations
    step_center(all_numeric()) %>%  # Centers on zero
    step_scale(all_numeric()) %>%   # Scales range to -1, +1
    step_dummy(all_nominal()) %>%   # Important to do after scaling
    prep()

# Apply + Bake Recipe to get Preprocessed test/train datasets
train_preprocessed_tbl <- bake(recipe_obj, new_data = train_tbl)
test_preprocessed_tbl <- bake(recipe_obj, new_data = test_tbl)

# View data + Plot data
glimpse(train_preprocessed_tbl)
plot_hist_facet(train_preprocessed_tbl)


# 4.1 Correlation Plot

get_cor(train_preprocessed_tbl, target = Purchased_Yes)
plot_cor(train_preprocessed_tbl, target = Purchased_Yes)




# 4.0 EDA Functions ----


# 4.1 Plot Distributions ----

## Stepping Down to Understand the Function

#1 To Inspect Functions: Save parameters w/same name as the function inputs
#data <- smpl_data_raw_tbl %>% 
#    select(-subcustomer_sc_id, -last_trxn)

# custom historgram plot:
# purpose: To inspect feature distributions + 
# check feature skewdness + identify transformations needed.

# These Historams allow us to really inspect for different things that are going
# on in the data that we might not have otherwise seen.

# A great way to visualize lots of features at once!


plot_hist_facet <- function(data, fct_reorder = FALSE, fct_rev = FALSE, 
                            bins = 10, fill = palette_light()[[3]], color = "white", ncol = 5, scale = "free") {
    
    data_factored <- data %>%
        mutate_if(is.character, as.factor) %>%  # so plot doesn't fail by getting character data
        mutate_if(is.factor, as.numeric) %>%    # so plot doesn't fail by getting factor data
        gather(key = key, value = value, factor_key = TRUE) # gathers wide data AND converts to long data
    
    if (fct_reorder) {
        data_factored <- data_factored %>%
            mutate(key = as.character(key) %>% as.factor())
    }
    
    if (fct_rev) {
        data_factored <- data_factored %>%
            mutate(key = fct_rev(key))
    }
    
    g <- data_factored %>%
        ggplot(aes(x = value, group = key)) +
        geom_histogram(bins = bins, fill = fill, color = color) +
        facet_wrap(~ key, ncol = ncol, scale = scale) + 
        theme_tq()
    
    return(g)
    
}


# 4.2 Get Correlation Function ----

get_cor <- function(data, target, use = "pairwise.complete.obs",
                    fct_reorder = FALSE, fct_rev = FALSE) {
    
    feature_expr <- enquo(target)
    feature_name <- quo_name(feature_expr)
    
    data_cor <- data %>%
        mutate_if(is.character, as.factor) %>%
        mutate_if(is.factor, as.numeric) %>%
        cor(use = use) %>%
        as.tibble() %>%
        mutate(feature = names(.)) %>%
        select(feature, !! feature_expr) %>%
        filter(!(feature == feature_name)) %>%
        mutate_if(is.character, as_factor)
    
    if (fct_reorder) {
        data_cor <- data_cor %>% 
            mutate(feature = fct_reorder(feature, !! feature_expr)) %>%
            arrange(feature)
    }
    
    if (fct_rev) {
        data_cor <- data_cor %>% 
            mutate(feature = fct_rev(feature)) %>%
            arrange(feature)
    }
    
    return(data_cor)
    
}


# 4.3 Plot Correlations ----

plot_cor <- function(data, target, fct_reorder = FALSE, fct_rev = FALSE, 
                     include_lbl = TRUE, lbl_precision = 2, lbl_position = "outward",
                     size = 2, line_size = 1, vert_size = 1, 
                     color_pos = palette_light()[[1]], 
                     color_neg = palette_light()[[2]]) {
    
    feature_expr <- enquo(target)
    feature_name <- quo_name(feature_expr)
    
    data_cor <- data %>%
        get_cor(!! feature_expr, fct_reorder = fct_reorder, fct_rev = fct_rev) %>%
        mutate(feature_name_text = round(!! feature_expr, lbl_precision)) %>%
        mutate(Correlation = case_when(
            (!! feature_expr) >= 0 ~ "Positive",
            TRUE                   ~ "Negative") %>% as.factor())
    
    g <- data_cor %>%
        ggplot(aes_string(x = feature_name, y = "feature", group = "feature")) +
        geom_point(aes(color = Correlation), size = size) +
        geom_segment(aes(xend = 0, yend = feature, color = Correlation), size = line_size) +
        geom_vline(xintercept = 0, color = palette_light()[[1]], size = vert_size) +
        expand_limits(x = c(-1, 1)) +
        theme_tq() +
        scale_color_manual(values = c(color_neg, color_pos)) 
    
    if (include_lbl) g <- g + geom_label(aes(label = feature_name_text), hjust = lbl_position)
    
    return(g)
    
}



