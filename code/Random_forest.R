library(tidyverse)
library(sf)
library(randomForest)
library(blockCV)
library(terra)
library(tidyterra)

source ("./code/Functions.R")
source ("./code/Load_predictors.R")

## Load calibration data

vme_id <- c("4a", "4b")  # Lophelia, Madrepora

vme_cnrm <- read_rds("./data/vme_terrain_cnrm_present.rds") %>%
  filter(vme %in% vme_id) %>%
  select(-vme) %>%
  drop_na()

vme_gfdl <- read_rds("./data/vme_terrain_gfdl_present.rds") %>%
  filter(vme %in% vme_id) %>%
  select(-vme) %>%
  drop_na()


## Load background points and subsample

set.seed(100)
back_cnrm <- read_rds("./data/backgr_terrain_cnrm_present.rds") %>%
  drop_na() %>%
  sample_n(nrow(vme_cnrm))

back_gfdl <- read_rds("./data/backgr_terrain_gfdl_present.rds") %>%
  drop_na() %>%
  sample_n(nrow(vme_gfdl))

training_cnrm <- bind_rows(vme_cnrm, back_cnrm) %>%
  mutate(occ = as.factor(occ))

training_gfdl <- bind_rows(vme_gfdl, back_gfdl) %>%
  mutate(occ = as.factor(occ))

## Spatial blocks

set.seed(100)
sac <- cv_spatial_autocor(r = env_cnrm,
                          x = st_as_sf(training_cnrm %>%
                                         select(occ, x, y),
                                       coords = c("x", "y"),
                                       crs = "+proj=laea +lat_0=69 +lon_0=-4 +datum=WGS84"),
                          column = "occ")

ecv <- cv_spatial(r = env_cnrm,
                  x = st_as_sf(training_cnrm %>%
                                 select(occ, x, y),
                               coords = c("x", "y"),
                               crs = "+proj=laea +lat_0=69 +lon_0=-4 +datum=WGS84"),
                  column = "occ",
                  k = 5,
                  size = round(sac$range),
                  hexagon = FALSE,
                  selection = "random")

training_cnrm <- training_cnrm %>%
  mutate(fold = ecv$folds_ids)

training_gfdl <- training_gfdl %>%
  mutate(fold = ecv$folds_ids)


model <- occ ~ depth + east21 + east3 + north21 + north3 + planc21 + planc3 +
  profc21 + slope21 + slope3 + tpi21 + tpi3 + vrm21 + vrm3 + cs + phos + POC_sed + oxygen + arag_sat + pH + sal + temp

###########################################
# Cross validation
###########################################

models.rf <- list()
predictions.rf <- list()
tmp <- Sys.time()

for (i in 1:5) {
  set.seed(100)
  rf <- randomForest(formula = as.formula(model),
                     data = training_cnrm %>% filter(fold == i),
                     ntree = 200,
                     mtry = 6,
                     maxnodes = 5)

  models.rf[[i]] <- rf
  base::print(Sys.time() - tmp)
  pred <- predict(rf, type = "prob")
  predictions.rf[[i]] <- pred[,2]
  base::print(Sys.time() - tmp)
}
Sys.time() - tmp

oob <- NULL
for (i in 1:5){
  conf <- models.rf[[i]]$confusion[,-ncol(models.rf[[i]]$confusion)]
  oob.val <- 1 - (sum(diag(conf))/sum(conf))
  oob.val <- oob.val * 100
  oob <- c(oob, oob.val)
}
(mean.oob <- mean(oob))



rf.cv <- CV(obs = training_cnrm$occ,
            fold = training_cnrm$fold,
            pred = predictions.rf)

###########################################
# Full model
###########################################
set.seed(100)
rf <- randomForest(formula = as.formula(model),
                       data = training_cnrm,
                       ntree = 200,
                       mtry = 6,
                       maxnodes = 5)

## RF Prediction
pr <- predict(rf, training_cnrm, type = "prob")


rpr <- predict(env_cnrm, rf, type = "prob", index = 2)
rpr126 <- predict(env_cnrm_126, rf, type = "prob", index = 2)
rpr370 <- predict(env_cnrm_370, rf, type = "prob", index = 2)

writeRaster(rpr, "./prediction/rf_4_present.tif", overwrite = TRUE)
writeRaster(rpr, "./prediction/rf_4_ssp126.tif", overwrite = TRUE)
writeRaster(rpr, "./prediction/rf_4_ssp370.tif", overwrite = TRUE)


## Need to do validation of the full model

## Diagnostics
plot(rf, main = "RF")

## Variable importance
gini <- tibble(var = row.names(rf$importance),
               score = rf$importance[, 1]) %>%
  arrange(desc(score))


ggplot(data = gini, aes(y = reorder(var, score))) +
  geom_col(aes(x = score), fill = "gray50") +
  theme_bw() +
  labs(x = "Mean decrease of Gini Index", y = "Variables") +
  theme(plot.margin = ggplot2::margin(r = 50),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14))

## Variable effects plot

par(mfrow = c(4, 2), cex = 0.77)
for (i in 1:8){
  partialPlot(x = rf,  pred.data = training_cnrm,
              x.var = gini$var[i],
              which.class = 1, main = gini$var[i], xlab = "")
}
