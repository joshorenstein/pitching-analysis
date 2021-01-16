


library(rsample)
set.seed(2453)
cv_splits <- vfold_cv(train, v = 10)
cv_splits

# Slide 29

# The `split` objects contain the information about the sample sizes
cv_splits$splits[[1]]

# Use the `analysis` and `assessment` functions to get the data
analysis(cv_splits$splits[[1]]) %>% dim()
assessment(cv_splits$splits[[1]]) %>% dim()

# Slide 30

library(yardstick)
library(dplyr)

lm_fit <- function(data_split, ...) 
  lm(..., data = analysis(data_split))

# A formula is also needed for each model:
form <- as.formula(
  FT~TwoPA+ThreePA+ORB+TOV
)

model_perf <- function(data_split, mod_obj) {
  vars <- rsample::form_pred(mod_obj$terms)
  assess_dat <- assessment(data_split) %>%
    select(!!!vars, FT) %>%
    mutate(
      pred = predict(
        mod_obj, 
        newdata = assessment(data_split)
      ),
      FT = FT
    )
  
  rmse <- assess_dat %>% 
    rmse(truth = FT, estimate = pred)
  rsq <- assess_dat %>% 
    rsq(truth = FT, estimate = pred)
  data.frame(rmse = rmse, rsq = rsq)
}

# Slide 31

library(purrr)
cv_splits <- cv_splits %>%
  mutate(lm_mod = map(splits, lm_fit, formula = form))
cv_splits

# Slide 32

# map2 can be used to move over two objects of equal length
lm_res <- map2_df(cv_splits$splits, cv_splits$lm_mod, model_perf) %>% 
  dplyr::rename(rmse_simple = rmse, rsq_simple = rsq)
head(lm_res, 3)

## Merge in results:
cv_splits <- cv_splits %>% bind_cols(lm_res)

## Rename the columns and compute the resampling estimates:
cv_splits %>% select(rmse_simple, rsq_simple) %>% colMeans

# Slide 34

get_assessment <- function(splits, model) 
  augment(model, newdata = assessment(splits)) %>%
  mutate(.resid = FT - .fitted)

holdout_results <- map2_df(cv_splits$splits, cv_splits$lm_mod, get_assessment)


# Slide 39

library(caret)

knn_train_mod <- knnreg(FT ~ TwoPA+ThreePA+ORB+TOV,
                        data = train,
                        k = 2)
repredict <- data.frame(FT = train$FT) %>%
  mutate(pred = 
           predict(knn_train_mod, 
                   newdata = train %>% select(TwoPA,ThreePA,ORB,TOV)
           )
  )

repredict %>% rsq(truth = "FT", estimate = "pred") # <- the ruckus is here

# Slide 40

knn_fit <- function(data_split, ...) 
  knnreg(..., data = analysis(data_split))

cv_splits <- cv_splits %>%
  mutate(knn_mod = map(splits, knn_fit, formula = form, k = 2))

knn_res <- map2_df(cv_splits$splits, cv_splits$knn_mod, model_perf) %>% 
  rename(rmse_knn = rmse, rsq_knn = rsq)

## Merge in results:
cv_splits <- cv_splits %>% bind_cols(knn_res)

colMeans(knn_res)

# Slide 42

rs_comp <- data.frame(
  rmse = c(cv_splits$rmse_simple, cv_splits$rmse_knn),
  Model = rep(c("Linear\nRegression", "2-NN"), each = nrow(cv_splits)),
  Resample = cv_splits$id
)

ggplot(rs_comp, aes(x = Model, y = rmse, group = Resample, col = Resample)) + 
  geom_point() + 
  geom_line() + 
  theme(legend.position = "none")

# Slide 43

t.test(cv_splits$rmse_simple, cv_splits$rmse_knn, paired = TRUE)

# Slide 49

knn_rmse <- function(k, split) {
  mod <- knnreg(FT ~ TwoPA+ThreePA+ORB+TOV,
                data = analysis(split),  
                k = k)
  # Extract the names of the predictors
  preds <- form_pred(mod$terms)
  data.frame(FT = (assessment(split)$FT)) %>%
    mutate(pred = predict(mod, assessment(split) %>% select(!!!preds))) %>%
    rmse(FT, pred)
}

# Slide 50
library(dplyr)

knn_grid <- function(split) {
  # Create grid
  tibble(k = 1:20) %>%
    # Execute grid for this resample
    mutate(
      rmse = map_dbl(k, knn_rmse, split = split),
      # Attach the resample indicators using `lables`
      id = labels(split)[[1]]
    )
}

# Slide 51

iter_over_resamples <- 
  function(resamp) 
    map_df(resamp$splits, knn_grid)

# Slide 52

knn_tune_res <- iter_over_resamples(cv_splits)
knn_tune_res %>% head(15)

# Slide 53

rmse_by_k <- knn_tune_res %>%
  group_by(k) %>%
  summarize(rmse = mean(rmse))

ggplot(rmse_by_k, aes(x = k, y = rmse)) + 
  geom_point() + geom_line()

# Slide 54

best_k <- knn_tune_res %>%
  group_by(id) %>%
  summarize(k = k[which.min(rmse)],
            rmse = rmse[which.min(rmse)])

ggplot(rmse_by_k, aes(x = k, y = rmse)) + 
  geom_point() + geom_line() + 
  geom_line(data = knn_tune_res, 
            aes(group = id, 
                col = id),
            alpha = .2, lwd = 1) + 
  geom_point(data = best_k, aes(col = id),
             alpha = .5, cex = 2)+
  theme(legend.position = "none")

# Slide 55
TwoPA+ThreePA+ORB+TOV

final_knn <- knnreg(FT ~ TwoPA + ThreePA + ORB + TOV,
                    data = train,
                    k = 4)
