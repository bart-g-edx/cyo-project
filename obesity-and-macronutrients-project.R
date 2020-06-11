#
# title: "CYO project: Obesity and Macronutrients"
# author: "bart-g"
# date: "6/10/2020"
# output: results tibble
#

# Prerequisites - packages ----------------------------------------------------

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(gam)) install.packages("gam", repos = "http://cran.us.r-project.org")
if(!require(gbm)) install.packages("gbm", repos = "http://cran.us.r-project.org")
if(!require(timeDate)) install.packages("timeDate", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
knitr::opts_chunk$set(echo = TRUE) 

# Data preparation & cleaning -------------------------------------------------

# data is loaded from two CSV formatted files.

data_adults <- read.csv("share-of-adults-defined-as-obese.csv")
names(data_adults)<- c("country", "country_code", "year", "y_obese")

data_macronutrients <- read.csv("daily-caloric-supply-derived-from-carbohydrates-protein-and-fat.csv")
names(data_macronutrients) <- c("country", "country_code", "year", "animal_proteins_supply", "plant_proteins_supply", "fat_supply", "carbonhydrates_supply")
data_adults <- data_adults %>% select(-country_code)
data_macronutrients <- data_macronutrients %>% select(-country_code)

# share of obese adults, snapshot:
head(data_adults) %>% knitr::kable()

# daily caloric supply [kcal], snapshot:
head(data_macronutrients) %>% knitr::kable()

# share of obese adults as uploaded from file: 
t(c(dim(data_adults)[1], 
    min(data_adults$year), 
    max(data_adults$year))) %>%
    knitr::kable(col.names = c("Obesity Share (total records)", "First year", "Last year"))

# daily caloric supply as uploaded from file:
t(c(dim(data_macronutrients)[1], 
    min(data_macronutrients$year), 
    max(data_macronutrients$year))) %>%
    knitr::kable(col.names = c("Macronutrients Supply (total records)", "First year", "Last year"))

# annually merged obesity data across all countries

obesity <- inner_join(data_adults, data_macronutrients, by = c("country", "year"))
obesity_countries_count <- as.numeric(
    obesity %>% distinct(country) %>% summarize(n())
)
names(obesity)[3] <- "y"

t(c(dim(obesity)[1], 
    obesity_countries_count,
    min(obesity$year), 
    max(obesity$year)
)) %>%
knitr::kable(col.names = c("Obesity data across countries (total records)", 
                           "Countries count", 
                           "First year", 
                           "Last year"))
rm(obesity_countries_count)
rm(data_adults, data_macronutrients)

#
# add lagged obesity variable for future research
# as well as changes in macronutrients supply
obesity <- obesity %>% 
    mutate(y_last = NA, 
           d_animal_proteins_supply = NA, 
           d_plant_proteins_supply = NA, 
           d_fat_supply = NA, 
           d_carbonhydrates_supply = NA)

#
# split all by country and process in calendar order,
# assign differentials
country_grouping_var <- obesity$country
groups_by_country <- split(obesity, country_grouping_var)
res_groups <- lapply(groups_by_country, function(x) {
    res <- arrange(x, year)
    res$y_last <- lag(res$y,1)
    res$d_animal_proteins_supply <- res$animal_proteins_supply - lag(res$animal_proteins_supply,1)
    res$d_plant_proteins_supply <- res$plant_proteins_supply - lag(res$plant_proteins_supply,1)
    res$d_fat_supply <- res$fat_supply - lag(res$fat_supply,1)
    res$d_carbonhydrates_supply <- res$carbonhydrates_supply - lag(res$carbonhydrates_supply,1)
    res
})
rm(groups_by_country)

# un-split into data frame again
obesity_new <- do.call(rbind, res_groups)
rm(res_groups)
obesity <- obesity_new %>% filter(!is.na(y_last))
rm(obesity_new)

# Data partitioning -----------------------------------------------------------

#
# partition obesity data 80/20
set.seed(11)
test_index <- createDataPartition(obesity$y, times = 1, p = 0.2, list=FALSE)
train_set <- obesity[-test_index,]
test_set <- obesity[test_index,]
max_year <- max(obesity$year)

# removing original data set,
# preventing look-ahead bias
rm(obesity)

# Data exploration ------------------------------------------------------------

# Is there a commonality among countries observing obesity?

top_countries <- train_set %>% filter(year == max_year) %>% select(y, country) %>% arrange(desc(y)) %>% head(5)

bottom_countries <- train_set %>% filter(year == max_year) %>% select(y, country) %>% arrange(desc(y)) %>% tail(5)

obese_extreme_countries <- c(
    top_countries$country, 
    bottom_countries$country)

col_scale <- scale_colour_discrete(limits = unique(obese_extreme_countries))

train_set %>% filter(country %in% obese_extreme_countries) %>%
    ggplot(aes(year, y, color = country)) + 
    geom_line() +
    col_scale +
    ggtitle("Highest and least obese countries - top & bottom 5")
rm(col_scale, obese_extreme_countries)
rm(top_countries, bottom_countries)

ggplot(train_set, aes(x=year, y=y, group=year)) +
    geom_boxplot(fill = "red", alpha=0.5) +
    ylab("") +
    ggtitle("Training set obesity worldwide [share of obese people]")

# plotting shows strong autocorrelation of obesity with its lagged variable

train_set %>% ggplot(aes(y_last, y, color = "red")) + 
  geom_point() + 
  geom_line() + 
  xlab("") + 
  ylab("") +
  ggtitle("Autocorrelation: Obesity vs 1-year lagged obesity") +
  theme(legend.position = "none")

# How supply of macronutrients progressed over the years?

# boxplot animal proteins across years 
ggplot(train_set, aes(x=year, y=animal_proteins_supply, group=year)) +
    geom_boxplot(fill = "green", alpha=0.4) +
    ylab("") +
    ggtitle("Animal proteins supply over the years [kcal/year/person]")

# boxplot plant proteins across years 
ggplot(train_set, aes(x=year, y=plant_proteins_supply, group=year)) +
    geom_boxplot(fill = "green", alpha=0.4) +
    ylab("") +
    ggtitle("Plant proteins supply over the years [kcal/year/person]")

# boxplot fat supply across years 
ggplot(train_set, aes(x=year, y=fat_supply, group=year)) +
    geom_boxplot(fill = "green", alpha=0.4) +
    ylab("") +
    ggtitle("Fat supply over the years [kcal/year/person]")

# boxplot carbonhydrates supply across years 
ggplot(train_set, aes(x=year, y=carbonhydrates_supply, group=year)) +
    geom_boxplot(fill = "green", alpha=0.4) +
    ylab("") +
    ggtitle("Carbonhydrates supply over the years [kcal/year/person]")

# How representative is macronutrients data?

# select raw macronutrients data for correlation estimation

col_names <- c("animal proteins", "plant proteins", "fat", "carbonhydrates")
obesity_slim <- train_set %>% select(-country, -year, -y, -y_last, 
                                     -d_animal_proteins_supply, 
                                     -d_plant_proteins_supply, 
                                     -d_fat_supply, 
                                     -d_carbonhydrates_supply)

# run correlation on raw macronutrients only

cor_macronutrients <- cor(obesity_slim) 
rownames(cor_macronutrients) <- col_names
cor_macronutrients %>% knitr::kable(
  col.names = col_names,
  align = 'c',
  digits = 2)

# run PCA on macronutrients

mn_pca <- prcomp(obesity_slim, scale = TRUE)

# recalc proportion of variance for each component

varExplained <- tibble(
  PC = c("PC1", "PC2", "PC3", "PC4"), 
  Proportion_of_Variance = 100*mn_pca$sdev / sum(mn_pca$sdev)
  )

variance_labels <- as.character(str_glue("{format(varExplained$Proportion_of_Variance, digits = 4, nsmall = 2)} %"))

# barplot components

varExplained %>% ggplot(aes(PC,Proportion_of_Variance, label = variance_labels)) +
    geom_bar(stat = "identity", fill="blue", width = 0.7) +
    ggtitle("Macronutrients PCA") +
    ylab("") +
    geom_text(nudge_y = 1, size = 3) +
    theme_minimal()
rm(obesity_slim, mn_pca) 

# Regression methods

# prepare RMSE function

calculate_error = function(actual, predicted) {
    sqrt(mean((actual-predicted)^2))
}

# k-Nearest Neighbors method (kNN) --------------------------------------------

# train features model (a): macronutrients

set.seed(41)
train_knn <- train(y ~ animal_proteins_supply + plant_proteins_supply + fat_supply + carbonhydrates_supply,
                   data = train_set,
                   method = "knn",
                   trControl = trainControl(method = "cv", number = 5),
                   tuneGrid = expand.grid(k = seq(1, 11, by = 1)))
plot(train_knn)
#
# best fit collection
bestfit_knn <- train_knn$results[which.min(train_knn$results$RMSE),]
rownames(bestfit_knn) <- "Best fit"
bestfit_knn %>% knitr::kable()
train_1_knn <- train_knn
rm(train_knn)

# train features model (b): auto-regressive obesity & macronutrients

set.seed(41)
train_knn <- train(y ~ y_last + animal_proteins_supply + plant_proteins_supply + fat_supply + carbonhydrates_supply,
                  data = train_set,
                  method = "knn",
                  trControl = trainControl(method = "cv", number = 5),
                  tuneGrid = expand.grid(k = seq(1, 11, by = 1)))

plot(train_knn)
#
# best fit collection
bestfit_knn_ar1 <- train_knn$results[which.min(train_knn$results$RMSE),]
rownames(bestfit_knn_ar1) <- "Best fit"
bestfit_knn_ar1 %>% knitr::kable()
train_2_knn <- train_knn
rm(train_knn)

# Generalized Additive Model using Splines ------------------------------------

# train features model (a): macronutrients

set.seed(41)
train_splines <- train(y ~ animal_proteins_supply + plant_proteins_supply + fat_supply + carbonhydrates_supply,
                       data = train_set,
                       method = "gamSpline",
                       trControl = trainControl(method = "cv", number = 5),
                       tuneGrid = expand.grid(
                           df = seq(1,25,by = 1)
                       ),
                       verbose = FALSE)
plot(train_splines)
#
# best fit collection
bestfit_splines_ar1 <- train_splines$results[which.min(train_splines$results$RMSE),]
rownames(bestfit_splines_ar1) <- "Best fit"
bestfit_splines_ar1 %>% knitr::kable()
train_1_splines <- train_splines
rm(train_splines)

# train features model (b): auto-regressive obesity & macronutrients

set.seed(41)
train_splines <- train(y ~ y_last + animal_proteins_supply + plant_proteins_supply + fat_supply + carbonhydrates_supply,
                       data = train_set,
                       method = "gamSpline",
                       trControl = trainControl(method = "cv", number = 5),
                       tuneGrid = expand.grid(
                           df = seq(1,25,by = 1)
                       ),
                       verbose = FALSE)

plot(train_splines)
#
# best fit collection
bestfit_splines <- train_splines$results[which.min(train_splines$results$RMSE),]
rownames(bestfit_splines) <- "Best fit"
bestfit_splines %>% knitr::kable()
train_2_splines <- train_splines
rm(train_splines)

# Generalized Additive Model using Loess --------------------------------------

# train features model (a): macronutrients 

set.seed(41)
train_loess <- train(
    y ~ animal_proteins_supply + plant_proteins_supply + fat_supply + carbonhydrates_supply,
    data = train_set,
    method = "gamLoess",
    trControl = trainControl(method = "cv", number = 5),
    tuneGrid = expand.grid(
        span = seq(0.10, 0.80, len = 7), degree = 1
    ),
    verbose = FALSE)
plot(train_loess)
#
# best fit collection
bestfit_loess <- train_loess$results[which.min(train_loess$results$RMSE),]
rownames(bestfit_loess) <- "Best fit"
bestfit_loess %>% knitr::kable()
train_1_loess <- train_loess
rm(bestfit_loess)

# train features model (b): auto-regressive obesity & macronutrients

set.seed(41)
train_loess <- train(
    y ~ y_last + animal_proteins_supply + plant_proteins_supply + fat_supply + carbonhydrates_supply,
    data = train_set,
    method = "gamLoess",
    trControl = trainControl(method = "cv", number = 5),
    tuneGrid = expand.grid(
        span = seq(0.10, 0.80, len = 7), degree = 1
    ),
    verbose = FALSE)
plot(train_loess)
#
# best fit collection
bestfit_loess <- train_loess$results[which.min(train_loess$results$RMSE),]
rownames(bestfit_loess) <- "Best fit"
bestfit_loess %>% knitr::kable()
train_2_loess <- train_loess
rm(bestfit_loess)

# Gradient Boosting Machines --------------------------------------------------

# train features model (a): macronutrients 

set.seed(41)
#
# once the "gbm" package supports parallel execution:
# - consider multi-core processing
# - afford half of the CPU power available:
#     cpuCoresWanted <- parallel::detectCores() / 2
gbm_grid = expand.grid(interaction.depth = c(1, 2, 3, 4),
                       n.trees = seq(100, 2500, 100),
                       shrinkage = c(0.1, 0.5),
                       n.minobsinnode = 10
                       )
train_gbm <- train(
    y ~ animal_proteins_supply + plant_proteins_supply + fat_supply + carbonhydrates_supply,
    data = train_set,
    method = "gbm",
    trControl = trainControl(method = "cv", number = 5),
    tuneGrid = gbm_grid, 
    verbose = FALSE)
ggplot(train_gbm)
#
# best fit collection
besttune_gbm <- train_gbm$bestTune 
rownames(besttune_gbm) <- "Best fit"
besttune_gbm %>% knitr::kable()
bestfit_gbm <- train_gbm$results[which.min(train_gbm$results$RMSE),] %>% 
 select(RMSE, Rsquared, MAE, RMSESD, RsquaredSD)
rownames(bestfit_gbm) <- "Best residuals"
bestfit_gbm %>% knitr::kable()
train_1_gbm <- train_gbm
rm(bestfit_gbm, besttune_gbm)
#
# prepare relative influence of variables based on final model trained
res <- tibble(variable = train_gbm$finalModel$var.names, influence = relative.influence(train_gbm$finalModel))
res$influence <- res$influence * 100/ sum(res$influence) 
res$variable <- str_replace_all(res$variable, "[_]", " ")
influence_labels <- as.character(str_glue("{format(res$influence, digits = 4, nsmall = 2)} %"))
#
# plot relative influence of variables
res %>% ggplot(aes(reorder(variable, -influence), influence, label = influence_labels)) +
    geom_bar(stat = "identity", fill="green", width = 0.7) +
    ggtitle("Relative Influence in Gradient Boosted model") +
    ylab("") +
    xlab("") +
    geom_text(nudge_y = 1, size = 3) +
    theme_minimal()
rm(res,influence_labels) 

# train features model (b): auto-regressive obesity & macronutrients

set.seed(41)
train_gbm <- train(
    y ~ y_last + animal_proteins_supply + plant_proteins_supply + fat_supply + carbonhydrates_supply,
    data = train_set,
    method = "gbm",
    trControl = trainControl(method = "cv", number = 5),
    tuneGrid = gbm_grid,
    verbose = FALSE)
ggplot(train_gbm)
#
# best fit collection
besttune_gbm <- train_gbm$bestTune 
rownames(besttune_gbm) <- "Best fit"
besttune_gbm %>% knitr::kable()
bestfit_gbm <- train_gbm$results[which.min(train_gbm$results$RMSE),] %>% 
    select(RMSE, Rsquared, MAE, RMSESD, RsquaredSD)
rownames(bestfit_gbm) <- "Best residuals"
bestfit_gbm %>% knitr::kable()
train_2_gbm <- train_gbm
rm(bestfit_gbm, besttune_gbm)
#
# prepare relative influence of variables based on final model trained
res <- tibble(variable = train_gbm$finalModel$var.names, influence = relative.influence(train_gbm$finalModel))
res$influence <- res$influence * 100/ sum(res$influence) 
res$variable <- str_replace_all(res$variable, "[_]", " ")
influence_labels <- as.character(str_glue("{format(res$influence, digits = 4, nsmall = 1)} %"))
#
# plot relative influence of variables
res %>% ggplot(aes(reorder(variable, -influence), influence, label = influence_labels)) +
    geom_bar(stat = "identity", fill="green", width = 0.7) +
    ggtitle("Relative Influence in Gradient Boosted model") +
    ylab("") +
    xlab("") +
    geom_text(nudge_y = 1, size = 3) +
    theme_minimal()
rm(res,influence_labels) 

# Extended modelling variants: using macronutrients supply's 1st difference

# train features model (c): change in macronutrients 

set.seed(41)
gbm_grid = expand.grid(interaction.depth = c(1, 2, 3, 4),
                       n.trees = seq(100, 2500, 100),
                       shrinkage = c(0.1, 0.5),
                       n.minobsinnode = 10
                       )
train_gbm <- train(
    y ~ d_animal_proteins_supply + d_plant_proteins_supply + d_fat_supply + d_carbonhydrates_supply,
    data = train_set,
    method = "gbm",
    trControl = trainControl(method = "cv", number = 5),
    tuneGrid = gbm_grid, 
    verbose = FALSE)
ggplot(train_gbm)
#
# best fit collection
besttune_gbm <- train_gbm$bestTune 
rownames(besttune_gbm) <- "Best fit"
besttune_gbm %>% knitr::kable()
bestfit_gbm <- train_gbm$results[which.min(train_gbm$results$RMSE),] %>% 
    select(RMSE, Rsquared, MAE, RMSESD, RsquaredSD)
rownames(bestfit_gbm) <- "Best residuals"
bestfit_gbm %>% knitr::kable()
train_3_gbm <- train_gbm
rm(bestfit_gbm, besttune_gbm)
#
# prepare relative influence of variables based on final model trained
res <- tibble(variable = train_gbm$finalModel$var.names, influence = relative.influence(train_gbm$finalModel))
res$influence <- res$influence * 100/ sum(res$influence) 
res$variable <- str_replace_all(res$variable, "d_", "delta ")
res$variable <- str_replace_all(res$variable, "[_]", " ")
res$variable <- str_replace_all(res$variable, "supply", "")
influence_labels <- as.character(str_glue("{format(res$influence, digits = 4, nsmall = 2)} %"))
#
# plot relative influence of variables
res %>% ggplot(aes(reorder(variable, -influence), influence, label = influence_labels)) +
    geom_bar(stat = "identity", fill="green", width = 0.7) +
    ggtitle("Relative Influence in Gradient Boosted model") +
    ylab("") +
    xlab("") +
    geom_text(nudge_y = 1, size = 3) +
    theme_minimal()
rm(res,influence_labels) 

# train features model (d): auto-regressive obesity & change in macronutrients

set.seed(41)
train_gbm <- train(
    y ~ y_last + d_animal_proteins_supply + d_plant_proteins_supply + d_fat_supply + d_carbonhydrates_supply,
    data = train_set,
    method = "gbm",
    trControl = trainControl(method = "cv", number = 5),
    tuneGrid = gbm_grid,
    verbose = FALSE)
ggplot(train_gbm)
#
# best fit collection
besttune_gbm <- train_gbm$bestTune 
rownames(besttune_gbm) <- "Best fit"
besttune_gbm %>% knitr::kable()
bestfit_gbm <- train_gbm$results[which.min(train_gbm$results$RMSE),] %>% 
    select(RMSE, Rsquared, MAE, RMSESD, RsquaredSD)
rownames(bestfit_gbm) <- "Best residuals"
bestfit_gbm %>% knitr::kable()
train_4_gbm <- train_gbm
rm(bestfit_gbm, besttune_gbm)
#
# prepare relative influence of variables based on final model trained
res <- tibble(variable = train_gbm$finalModel$var.names, influence = relative.influence(train_gbm$finalModel))
res$influence <- res$influence * 100/ sum(res$influence) 
res$variable <- str_replace_all(res$variable, "d_", "delta ")
res$variable <- str_replace_all(res$variable, "[_]", " ")
res$variable <- str_replace_all(res$variable, "supply", "")
influence_labels <- as.character(str_glue("{format(res$influence, digits = 4, nsmall = 1)} %"))
#
# plot relative influence of variables
res %>% ggplot(aes(reorder(variable, -influence), influence, label = influence_labels)) +
    geom_bar(stat = "identity", fill="green", width = 0.7) +
    ggtitle("Relative Influence in Gradient Boosted model") +
    ylab("") +
    xlab("") +
    geom_text(nudge_y = 1, size = 3) +
    theme_minimal()
rm(res,influence_labels) 
#
# cleanup temp data
rm(bestfit_knn, bestfit_knn_ar1, bestfit_splines, bestfit_splines_ar1)
rm(cor_macronutrients, gbm_grid, test_index, varExplained, variance_labels)
rm(max_year, col_names, country_grouping_var)

# Results compilation ---------------------------------------------------------
             
#
# function combining training and test set RMSE
#
method_test_run <- function(method_label, model_label, trained_method, 
                            train_set_selection, test_set_selection) {
    #
    # test outcome
    test_predictions <- predict(trained_method, test_set_selection)
    test_error <- calculate_error(test_set_selection$y, test_predictions)
    #
    # training outcome
    training_predictions <- predict(trained_method, train_set_selection) 
    training_error <- calculate_error(train_set_selection$y, training_predictions)
    #
    # error change between training set and test set
    # always expecting under-performance out-of-sample, 
    # as a consequence of the over-fitting 
    error_change <- ((test_error / training_error) - 1) * 100
    tibble(method = method_label, 
        model = model_label, 
        test_error = test_error, 
        training_error = training_error,
        test_error_change = error_change)
}
             
results <- tibble()
results <- rbind(results, 
                method_test_run("kNN", "macronutrients", train_1_knn, train_set, test_set))
results <- rbind(results, 
                method_test_run("kNN", "AR(1) & macronutrients", train_2_knn, train_set, test_set))
results <- rbind(results, 
                method_test_run("GAM with Splines", "macronutrients", train_1_splines, train_set, test_set))
results <- rbind(results, 
                method_test_run("GAM with Splines", "AR(1) & macronutrients", train_2_splines, train_set, test_set))
results <- rbind(results, 
                method_test_run("GAM with Loess", "macronutrients", train_1_loess, train_set, test_set))
results <- rbind(results, 
                method_test_run("GAM with Loess", "AR(1) & macronutrients", train_2_loess, train_set, test_set))
results <- rbind(results, 
                method_test_run("GBM", "macronutrients", train_1_gbm, train_set, test_set))
results <- rbind(results, 
                method_test_run("GBM", "AR(1) & macronutrients", train_2_gbm, train_set, test_set))
results <- rbind(results, 
                method_test_run("GBM", "macronutrients change", train_3_gbm, train_set, test_set))
results <- rbind(results, 
                method_test_run("GBM", "AR(1) & macronutrients change", train_4_gbm, train_set, test_set))

results %>% knitr::kable(col.names = c("Method", "Features", "RMSE(test)", "RMSE(training)", "% Change"))

