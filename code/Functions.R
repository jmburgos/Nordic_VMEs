
####################################################################
## Cross validation
####################################################################

CV <- function(obs, fold, pred) {

  AUC <- NULL
  PAUC <- NULL
  TSS <- NULL
  COR <- NULL

  for (i in 1:5){


    AUC[i] <- pROC::auc(response = obs[fold == i],
                        predictor = pred[[i]])
    PAUC[i] <- pROC::auc(response = obs[which(fold == i)],
                         predictor = pred[[i]],
                         partial.auc = c(0.5, 1),
                         partial.auc.correct = TRUE)

    # Calculate TSS
    actual <- obs[which(fold == i)]
    probabilities <- pred[[i]]
    thresholds <- seq(0, 1, by = 0.01)
    tss <- sapply(thresholds, function(threshold) {
      predicted <- ifelse(probabilities > threshold, 1, 0)
      df <- data.frame(predicted, actual)
      truepositive <- filter(df, predicted == 1 & actual == 1)
      truenegative <- filter(df, predicted == 0 & actual == 0)
      (nrow(truepositive) / sum(actual == 1)) + (nrow(truenegative) / sum(actual == 0)) - 1
    })
    TSS[i] <- max(tss)

    COR[i] <- cor(as.numeric(obs[foldvector == i]), pred[[i]])
  }

  res <- tibble(auc = AUC, pauc = PAUC, maxTSS = TSS,cor = COR)
  mres <- res %>%
    summarise(across(everything(), mean))
  sdres <- res %>%
    summarise(across(everything(), sd))

  list(res, mres, sdres)

}

####################################################################
## TSS
####################################################################

tss.fun <- function(data, pred){
  actual <- data
  probabilities <- pred
  thresholds <- seq(0, 1, by = 0.01)
  tss <- sapply(thresholds, function(threshold) {
    predicted <- ifelse(probabilities > threshold, 1, 0)
    df <- as.data.frame(cbind(predicted, actual))
    truepositive <- dplyr::filter(df, predicted == 1 & actual == 1)
    sensitivity <- nrow(truepositive)/length(actual[which(actual == 1)])
    truenegative <- dplyr::filter(df, predicted == 0 & actual == 0)
    specificity <- nrow(truenegative)/length(actual[which(actual == 0)])
    specificity + sensitivity - 1
  })
  optimal_threshold <- thresholds[which.max(tss)]
  plot(tss)
  max_tss <- cbind(tss.train = max(tss), optimal_threshold)
  max_tss
}

tss.ci <- function(data, pred, num_iterations = 2000, conf_level = 0.95){
  tss_values <- numeric(num_iterations)
  for (i in 1:num_iterations) {
    tog <- cbind(data, pred)
    zero <- tog[which(tog[,1] == 0), ]
    one <- tog[which(tog[,1] == 1), ]
    zero_sample <- zero[sample(1:nrow(zero), size = nrow(zero), replace = TRUE), ]
    one_sample <- one[sample(1:nrow(one), size = nrow(one), replace = TRUE), ]
    comb <- rbind(zero_sample, one_sample)
    actual <- comb[,1]
    probabilities <- comb[,2]
    thresholds <- seq(0, 1, by = 0.01)
    tss <- sapply(thresholds, function(threshold) {
      predicted <- ifelse(probabilities > threshold, 1, 0)
      df <- as.data.frame(cbind(predicted, actual))
      truepositive <- dplyr::filter(df, predicted == 1 & actual == 1)
      sensitivity <- nrow(truepositive)/length(actual[which(actual == 1)])
      truenegative <- dplyr::filter(df, predicted == 0 & actual == 0)
      specificity <- nrow(truenegative)/length(actual[which(actual == 0)])
      specificity + sensitivity - 1
    })
    max_tss <- max(tss)
    tss_values[i] <- max_tss
  }
  mean_tss <- mean(tss_values)
  ci_lower <- quantile(tss_values, (1 - conf_level) / 2)
  ci_upper <- quantile(tss_values, 1 - (1 - conf_level) / 2)
  return(list(mean_tss = mean_tss, ci_lower = ci_lower, ci_upper = ci_upper))
}

train_validation <- function(predraster, lat_lon, testdata){
  if (class(predraster) == "SpatRaster"){
    train.pred <- extract(predraster, lat_lon)
    train.pred <- train.pred[,2]
  }
  if (class(predraster) == "numeric"){
    train.pred <- predraster
  }
  # AUC and PAUC
  auc.train <- as.numeric(pROC::auc(testdata, train.pred))
  pauc.train <- as.numeric(pROC::auc(testdata, train.pred,
                                     partial.auc = c(0.5, 1), partial.auc.correct = TRUE))
  # TSS
  tss.train <- tss.fun(testdata, train.pred)
  # COR
  cor.train <- cor(testdata,
                   train.pred)
   ## CIs
  ci.tss <- tss.ci(testdata, train.pred)
  ci.tss <- rbind(ci.tss$ci_lower, ci.tss$ci_upper)
  roc <- pROC::roc(testdata, train.pred)
  ci.auc <- pROC::ci.auc(roc, conf.level = 0.95, method = "b")
  ci.auc <- rbind(ci.auc[1], ci.auc[3])
  proc <- pROC::roc(testdata, train.pred, partial.auc = c(0.5, 1), partial.auc.correct = TRUE)
  ci.pauc <- pROC::ci.auc(proc, conf.level = 0.95)
  ci.pauc <- rbind(ci.pauc[1], ci.pauc[3])
  cor.test <- cor.test(testdata, train.pred)
  ci.cor <- cor.test$conf.int
  ci.cor <- rbind(ci.cor[1], ci.cor[2])

  results <- cbind(auc.train, pauc.train, tss.train, cor.train)
  colnames(results) <- c("AUC", "PAUC", "TSS", "OptimalThreshold", "COR")
  conf_int <- cbind(ci.auc, ci.pauc, ci.tss, ci.cor)
  colnames(conf_int) <- c("AUC", "PAUC", "TSS", "COR")
  rownames(conf_int) <- c("LowerCI", "UpperCI")
  list(results = results, conf_int = conf_int)
}
