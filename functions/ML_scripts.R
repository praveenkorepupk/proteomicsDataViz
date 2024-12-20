### Preprocess data 

prep_data <- function(data, metadata, group){
  
  dw = pivot_wider(data = data, id_cols = "SampleID",names_from = "Assay",values_from = "NPX",values_fn = mean)
  dw = as.data.frame(dw)
  rownames(dw) = dw$SampleID
  dw = dw[,-1]  
  
  met = metadata
  rownames(met) = met$SampleID
  met = met[rownames(dw),]
  
  dw[[group]] = met[[group]]
  ## remove the samples for which the classification is not available or NA. 
  dw2 = dw[complete.cases(dw),] 
  if (is.numeric(dw2[[group]])){
    
    dw2[[group]] = paste0("X",dw2[[group]])
    
  }
  
  dw2[[group]] = factor(dw2[[group]])
  
  return(dw2)

}


prep_data2 <- function(data, metadata, group, random){
  
  # Convert long to wide format
  dw <- pivot_wider(data = data, id_cols = "SampleID",names_from = "Assay",values_from = "NPX",values_fn = mean)
  dw <- as.data.frame(dw)
  rownames(dw) <- dw$SampleID
  dw <- dw[,-1]  
  
  # Metadata
  met <- metadata
  rownames(met) <- met$SampleID
  met <- met[rownames(dw),]
  
  # Adding classification variable i.e group
  dw[[group]] <- met[[group]]
  
  # Addiing random variables
  dw[random] <- met[random]
  
  # Consider complete cases
  dw2 <- dw[complete.cases(dw),]   ## remove the samples for which the classification is not available or NA. 
  
  # Handle numeric class variable
  if (is.numeric(dw2[[group]])){
    
    dw2[[group]] <- paste0("X",dw2[[group]])
    
  }
  
  for (col_name in random) {
    # Check if the column is numeric
    if (is.numeric(dw2[[col_name]])) {
      # Convert numeric column to character and prefix with "X"
      dw2[[col_name]] <- paste0("X",(dw2[[col_name]]))
    }
  }
  
  # Convert group in factor
  dw2[[group]] <- factor(dw2[[group]])
 
  return(dw2)
}


split_data <-function(data, trainperc, group){

  ## Split the data 80:20
  set.seed(124)
  
  ##outer loop data
  trainIndex <- createDataPartition(data[[group]], p = as.numeric(trainperc),
                                    list = FALSE, times = 1)
  traindata1 <- data[ trainIndex,]
  testdata1  <- data[-trainIndex,]
  
  ## X-vector having only predictors
  traindata1X <- traindata1[, !colnames(traindata1) %in% group]
  traindata1X <- traindata1X %>% mutate_if(is.character, as.numeric)
  testdata1X <- testdata1[, !colnames(testdata1) %in% group]
  testdata1X <- testdata1X %>% mutate_if(is.character, as.numeric)
  
  ## Y-vector
  traindata1Y <- as.factor(traindata1[[group]])
  testdata1Y <- as.factor(testdata1[[group]])

  # create inner loop data
  train2Index <- createDataPartition(traindata1[[group]], p = as.numeric(trainperc),list = F,times = 1)
  traindata2 = traindata1[train2Index,]
  testdata2 = traindata1[-train2Index,]
  
  ## X-vector having only predictors
  traindata2X <- traindata2[, !colnames(traindata2) %in% group]
  traindata2X <- traindata2X %>% mutate_if(is.character, as.numeric)
  testdata2X <- testdata2[, !colnames(testdata2) %in% group]
  testdata2X <- testdata2X %>% mutate_if(is.character, as.numeric)
  
  ## Y-vector having class response
  traindata2Y <- as.factor(traindata2[[group]])
  testdata2Y <- as.factor(testdata2[[group]])
  
  return(list(traindata1, testdata1, traindata2, testdata2, traindata1X, traindata1Y, testdata1X, testdata1Y, traindata2X, traindata2Y, testdata2X, testdata2Y))

}


split_data2 <- function(data, trainperc, group, random){
  
  ## Split the data 80:20
  set.seed(124)
  
  ##outer loop data
  trainIndex <- createDataPartition(data[[group]], p = as.numeric(trainperc),
                                    list = FALSE, times = 1)
  traindata1 <- data[ trainIndex,]
  testdata1  <- data[-trainIndex,]
  
  ## X-vector having only predictors
  traindata1X <- traindata1[, !(colnames(traindata1) %in% c(group, random))]
  traindata1X <- traindata1X %>% mutate_if(is.character, as.numeric)
  testdata1X <- testdata1[, !(colnames(testdata1) %in% c(group, random))]
  testdata1X <- testdata1X %>% mutate_if(is.character, as.numeric)
  
  ## Y-vector
  traindata1Y <- as.factor(traindata1[[group]])
  testdata1Y <- as.factor(testdata1[[group]])

  return(list(traindata1, testdata1, traindata1X, traindata1Y, testdata1X, testdata1Y))
  
}

################## PLS #############################

pls_model_tuning <- function(traindx,traindy,testdx,group){
  
  # Set up the tuning grid for ncomp hyper parameters
  tuneGrid <- expand.grid(ncomp = c(1, 2, 3, 4, 5)) 
  
  # Define fitcontrol based on nlevel of traindy
  if(nlevels(traindy) == 2){
    
    # Perform 10 fold CV
    fitControl <- trainControl(## 10-fold CV
      method = "repeatedcv",
      classProbs = T,
      number = 10,
      repeats = 2,savePredictions = "all",summaryFunction = twoClassSummary)
    
  } else if(nlevels(traindy) > 2){
    
    fitControl <- trainControl(## 10-fold CV
      method = "repeatedcv",
      classProbs = T,
      number = 10,
      repeats = 2,savePredictions = "all",summaryFunction = multiClassSummary)
    
  }
 
  # Train the model inner loop
  pslm1 <- train(x = traindx,y = traindy, method = "pls", tuneGrid = tuneGrid, trControl = fitControl, importance = T, metric="ROC")
  
  # Use the best model for prediction
  predictions <- predict(pslm1, newdata = testdx)
  
  return(pslm1)
}

pls_feature_sel <- function(traindx,traindy,group){
  
  # Define tuneGrid
  tuneGrid <- expand.grid(ncomp = c(1, 2, 3, 4, 5)) 
  
  # Feature selection control
  featSel <- sbfControl(method = "repeatedcv", number = 10, p = .8, saveDetails = TRUE)
  
  # Fit Control
  if(nlevels(traindy) == 2){
    
    # Perform 10 fold CV
    fitControl <- trainControl(## 10-fold CV
      method = "repeatedcv",
      classProbs = T,
      number = 10,
      repeats = 2,savePredictions = "all",summaryFunction = twoClassSummary)
    
  } else if(nlevels(traindy) > 2){
    
    fitControl <- trainControl(## 10-fold CV
      method = "repeatedcv",
      classProbs = T,
      number = 10,
      repeats = 2,savePredictions = "all",summaryFunction = multiClassSummary)
  }
  
  # Run sbf
  sbf_fit <- sbf(x = traindx, y = traindy, method = "pls", tuneGrid=tuneGrid, trControl = fitControl, sbfControl = featSel)
  return(sbf_fit)
}

train_pls <- function(traindx,traindy,group,mdl1,mdl2){
  
  # Get best parameters from fine tuning model
  best_ncomp = mdl1$bestTune  ## model tuning
  
  # Get best features from feature selection model
  best_features = mdl2$optVariables  # feature selection
  
  # Subset data
  traindx = traindx[,best_features] # subset train data with best features
  
  # Define fit Contol
  if(nlevels(traindy) == 2){
    
    # Perform 10 fold CV
    fitControl <- trainControl(## 10-fold CV
      method = "repeatedcv",
      classProbs = T,
      number = 10,
      repeats = 2,savePredictions = "all",summaryFunction = twoClassSummary)
    
  } else if(nlevels(traindy) > 2){
    
    fitControl <- trainControl(## 10-fold CV
      method = "repeatedcv",
      classProbs = T,
      number = 10,
      repeats = 2,savePredictions = "all",summaryFunction = multiClassSummary)
    
  }
  
  # Use ncomp and best features on traindata from outer loop
  pslmf <- train(x = traindx,y = traindy,method = "pls",trControl = fitControl,tuneGrid = data.frame(ncomp = best_ncomp),importance = T,metric="ROC")
  return(pslmf)
  
}

################# KNN #############################
knn_model_tuning <- function(traindx,traindy,testdx,group){
  
  # Define the tuning grid for KNN
  tuneGrid <- expand.grid(
    k = c(5, 10, 15, 20)   # Number of nearest neighbors     
  )
  
  # Define fit control
  if(nlevels(traindy) == 2){
    
    # Perform 10 fold CV
    fitControl <- trainControl(## 10-fold CV
      method = "repeatedcv",
      classProbs = T,
      number = 10,
      repeats = 2,savePredictions = "all",summaryFunction = twoClassSummary)
    
  } else if(nlevels(traindy) > 2){
    
    fitControl <- trainControl(## 10-fold CV
      method = "repeatedcv",
      classProbs = T,
      number = 10,
      repeats = 2,savePredictions = "all",summaryFunction = multiClassSummary)
    
  }
  
  # Train the model inner loop
  knnm1 <- train(x = traindx,y = traindy, method = "knn", tuneGrid = tuneGrid, trControl = fitControl, metric="ROC")
  
  # Use the best model for prediction
  predictions <- predict(knnm1, newdata = testdx)
  
  return(knnm1)
}

knn_feature_sel <- function(traindx,traindy,group){
  
  # Define tune grid
  tuneGrid <- expand.grid(
    k = c(5, 10, 15, 20)   # Number of nearest neighbors  
  )
  
  # Define control for feature selection
  featSel <- sbfControl(method = "repeatedcv", number = 10, p = .8, saveDetails = TRUE)
  
  # Define fit control
  if(nlevels(traindy) == 2){
    
    # Perform 10 fold CV
    fitControl <- trainControl(## 10-fold CV
      method = "repeatedcv",
      classProbs = T,
      number = 10,
      repeats = 2,savePredictions = "all",summaryFunction = twoClassSummary)
    
  } else if(nlevels(traindy) > 2){
    
    fitControl <- trainControl(## 10-fold CV
      method = "repeatedcv",
      classProbs = T,
      number = 10,
      repeats = 2,savePredictions = "all",summaryFunction = multiClassSummary)
  }
  
  # Run sbf
  sbf_fit <- sbf(x = traindx, y = traindy, method = "knn", tuneGrid=tuneGrid, trControl = fitControl, sbfControl = featSel)
  return(sbf_fit)
}

train_knn <- function(traindx,traindy,group,mdl1,mdl2){
  
  # Get best parameters from fine tuning model
  best_k = mdl1$bestTune  ## model tuning
  
  # Get best features from feature selection model
  best_features = mdl2$optVariables  # feature selection
  
  # Subset data
  traindx = traindx[,best_features] # subset train data with best features
  
  # Define fit control
  if(nlevels(traindy) == 2){
    
    # Perform 10 fold CV
    fitControl <- trainControl(## 10-fold CV
      method = "repeatedcv",
      classProbs = T,
      number = 10,
      repeats = 2,savePredictions = "all",summaryFunction = twoClassSummary)
    
  } else if(nlevels(traindy) > 2){
    
    fitControl <- trainControl(## 10-fold CV
      method = "repeatedcv",
      classProbs = T,
      number = 10,
      repeats = 2,savePredictions = "all",summaryFunction = multiClassSummary)
    
  }
  
  # Use best param and best features on traindata from outer loop 
  knnmf <- train(x = traindx,y = traindy,method = "knn",trControl = fitControl,tuneGrid = data.frame(k = best_k),metric="ROC")
  return(knnmf)
  
}


################# SPLS ############################
spls_feature_sel <- function(traindx,traindy,group){
  
  # Define control for feature selection
  featSel <- sbfControl(method = "repeatedcv", number = 10, p = .8, saveDetails = TRUE)
  
  # Define fit control
  if(nlevels(traindy) == 2){
    
    # Perform 10 fold CV
    fitControl <- trainControl(## 10-fold CV
      method = "repeatedcv",
      classProbs = T,
      number = 10,
      repeats = 2,savePredictions = "all",summaryFunction = twoClassSummary)
    
  } else if(nlevels(traindy) > 2){
    
    fitControl <- trainControl(## 10-fold CV
      method = "repeatedcv",
      classProbs = T,
      number = 10,
      repeats = 2,savePredictions = "all",summaryFunction = multiClassSummary)
  }
  
  # Run sbf
  sbf_fit <- sbf(x = traindx, y = traindy, method = "pls", trControl = fitControl, sbfControl = featSel)
  return(sbf_fit)
}

spls_model_tuning <- function(traindx,traindy,testdx,group,mdl1){
  
  # Select top 50 best features from feature selection model
  best_features <- mdl1$optVariables[1:50] #selecting top 100 features
  best_features <- na.omit(best_features)
  
  # Subsetting the data based on selected features
  traindx <- traindx[,best_features]
  testdx <- traindx[,best_features]
 
  # Set up the tuning grid for hyper parameters
  tuneGrid <- expand.grid(
    K = c(5, 10, 15),      # Number of variables selected at each iteration
    eta = 0.5,   # Penalization parameter
    kappa = 1e-06  # Coefficient threshold
  )
  
  # Define fit control
  if(nlevels(traindy) == 2){
    
    # Perform 10 fold CV
    fitControl <- trainControl(## 10-fold CV
      method = "repeatedcv",
      classProbs = T,
      number = 10,
      repeats = 2,savePredictions = "all",summaryFunction = twoClassSummary)
    
  } else if(nlevels(traindy) > 2){
    
    fitControl <- trainControl(## 10-fold CV
      method = "repeatedcv",
      classProbs = T,
      number = 10,
      repeats = 2,savePredictions = "all",summaryFunction = multiClassSummary)
    
  }
  
  # Train the model inner loop
  splsm1 <- train(x = traindx,y = traindy, method = "spls", tuneGrid = tuneGrid, trControl = fitControl, metric="ROC")
  
  # Use the best model for prediction
  predictions <- predict(splsm1, newdata = testdx)
  
  return(splsm1)
}

train_spls <- function(traindx,traindy,group,mdl1,mdl2){
  
  # Get best param from fine tuning model
  best_param <- mdl2$bestTune
  
  # Get best features from feature selection model
  best_features <- mdl1$optVariables[1:50] #selecting top 50 features
  best_features <- na.omit(best_features)# feature selection
  
  # Subset data
  traindx <- traindx[,best_features] # subset train data with best features
  
  # Define fit control
  if(nlevels(traindy) == 2){
    
    # Perform 10 fold CV
    fitControl <- trainControl(## 10-fold CV
      method = "repeatedcv",
      classProbs = T,
      number = 10,
      repeats = 2,savePredictions = "all",summaryFunction = twoClassSummary)
    
  } else if(nlevels(traindy) > 2){
    
    fitControl <- trainControl(## 10-fold CV
      method = "repeatedcv",
      classProbs = T,
      number = 10,
      repeats = 2,savePredictions = "all",summaryFunction = multiClassSummary)
    
  }
  
  # Use ncomp and best features on traindata from outer loop
  splsmf <- train(x = traindx,y = traindy,method = "spls",trControl = fitControl,tuneGrid = best_param,metric="ROC")
  return(splsmf)
  
}


################# GLMM ############################

glmm_feature_sel <- function(traindx,traindy,group){
  
  # Define control for feature selection
  featSel <- sbfControl(method = "repeatedcv", number = 10, p = .8, saveDetails = TRUE)
  
  # Define fit control
  if(nlevels(traindy) == 2){
    
    # Perform 10 fold CV
    fitControl <- trainControl(## 10-fold CV
      method = "repeatedcv",
      classProbs = T,
      number = 10,
      repeats = 2,savePredictions = "all",summaryFunction = twoClassSummary)
    
  } else if(nlevels(traindy) > 2){
    
    fitControl <- trainControl(## 10-fold CV
      method = "repeatedcv",
      classProbs = T,
      number = 10,
      repeats = 2,savePredictions = "all",summaryFunction = multiClassSummary)
  }
  
  # Run sbf
  sbf_fit <- sbf(x = traindx, y = traindy, method = "glm", trControl = fitControl, sbfControl = featSel)
  return(sbf_fit)
}

train_glmm <- function(traind,group,random,mdl1){
  
  # Get best features from feature selection method
  best_features = mdl1$optVariables # feature selection
  
  # Subset data
  traind <- traind[, c(group, random, best_features)]
  
  # Define fit control
  if(length(levels(traind[[group]])) == 2){
    
    # Perform 10 fold CV
    fitControl <- trainControl(## 10-fold CV
      method = "repeatedcv",
      classProbs = T,
      number = 10,
      repeats = 2,savePredictions = "all",summaryFunction = twoClassSummary)
    
  } else if(length(levels(traind[[group]])) > 2){
    
    fitControl <- trainControl(## 10-fold CV
      method = "repeatedcv",
      classProbs = T,
      number = 10,
      repeats = 2,savePredictions = "all",summaryFunction = multiClassSummary)
    
  }  
  
  # Create formula
  linear_vars <- setdiff(names(traind), c(random, group))
  formula <- paste(group, " ~ ", paste(ifelse(grepl("[^[:alnum:]_]", linear_vars), paste0("`", linear_vars, "`"), linear_vars), collapse = " + "), " + (1|", paste(ifelse(grepl("[^[:alnum:]_]", random), paste0("`", random, "`"), random), collapse = ") + (1|"), ")", sep = "")    
  
  # Use best features on traindata
  glmmf <- glmer(formula, data = traind, family = binomial)
  return(glmmf)
  
}


################# SVM #############################
model_tuning <- function(traind,testd, group){

  #generate a tunegrid or a vector of parameters ( in this case Cost) to be used for training.
  tuneGrid <-  expand.grid(C = seq(0.1,0.2,0.1))
  nrow(tuneGrid)
  
  traind[[group]] = factor(traind[[group]])
  
  if(length(levels(traind[[group]])) == 2){
    
    # Perform 10 fold CV
    fitControl <- trainControl(## 10-fold CV
      method = "repeatedcv",
      classProbs = T,
      number = 10,
      repeats = 2,savePredictions = "all",summaryFunction = twoClassSummary)

  } else if(length(levels(traind[[group]])) > 2){
    
    fitControl <- trainControl(## 10-fold CV
      method = "repeatedcv",
      classProbs = T,
      number = 10,
      repeats = 2,savePredictions = "all",summaryFunction = multiClassSummary)
    
  }

  
  # Create the formula ( eg. developed_RRMS ~.)
  Formula  = reformulate(".",response=group)

  # train the model inner loop
  mdl3 <- train(
    form = Formula,
    data = traind,
    method = "svmLinear",
    trControl = fitControl, tuneGrid = tuneGrid,importance = T,metric="ROC")

  #predict test data
  pred_cf = predict(object = mdl3,newdata = testd)
  #pred_prob = predict(object = mdl3,newdata = testd,type="prob")

  return(mdl3)

}

feature_sel <- function(traind, testd, group){

  tuneGrid <-  expand.grid(C = seq(0.1,0.2,0.1))
  nrow(tuneGrid)
  
  traind[[group]] = factor(traind[[group]])
  

  # Create the formula ( eg. developed_RRMS ~.)
  Formula  = reformulate(".",response=group)

  ## run sbf
  sbf_fit <- sbf(
    form = Formula,
    data = traind,
    method = "svmLinear",
    tuneGrid=tuneGrid,
    trControl = trainControl(),
    sbfControl = sbfControl(method = "repeatedcv",
                            number = 10,
                            p = .8,
                            saveDetails = TRUE))

  return(sbf_fit)

}

# 
train_svm <- function(traind, testd, group, mdl1, mdl2){

  best_cost = mdl1$bestTune  ## model tuning
  best_features = mdl2$optVariables  # feature selection
  traindata = traind[,best_features] # subset train data with best features
  traindata[[group]] = traind[[group]]
  traindata[[group]] = as.factor(traindata[[group]])
  testd[[group]] = as.factor(testd[[group]])


  if(length(levels(traindata[[group]])) == 2){
    
    # Perform 10 fold CV
    fitControl <- trainControl(## 10-fold CV
      method = "repeatedcv",
      classProbs = T,
      number = 10,
      repeats = 2,savePredictions = "all",summaryFunction = twoClassSummary)
    
  } else if(length(levels(traindata[[group]])) > 2){
    
    fitControl <- trainControl(## 10-fold CV
      method = "repeatedcv",
      classProbs = T,
      number = 10,
      repeats = 2,savePredictions = "all",summaryFunction = multiClassSummary)
    
  }
  
  
  Formula  = reformulate(".",response=group)
  

  # use best cost and best features on traindata from outer loop
  mdlF = train(
    form = Formula,
    data = traindata,
    method = "svmLinear",
    trControl = fitControl,tuneGrid = data.frame(C = best_cost),importance = T,metric="ROC")


  return(mdlF)



}


plot_roc <- function(model, testd, group, mdl1){
  
  # Best features
  best_features <- mdl1$optVariables
  
  # Subset data
  testd <- testd[, c(group,best_features)]
  
  # Predict
  pred <- predict(object = model, testd, type = "prob")
  
  # ROC plot
  rocF <- roc(testd[[group]],pred[,1])
  p <- plot(rocF, col="royalblue",main = "ROC curve")
  return(list(p,rocF))

}

plot_roc2 <- function(model,testd,group,random,mdl1){
  
  # Best features
  best_features <- mdl1$optVariables # feature selection
  
  # Data subset
  testd <- testd[, c(group, random, best_features)]
  pred = predict(object=model, testd, type = "response")
  rocF = roc(testd[[group]],pred)
  p = plot(rocF, col="royalblue",main = "ROC curve")
  return(list(p,rocF))
  
}

conf_matrix_spls <- function(model, group, testd, mdl1){
  
  # Best features
  best_features <- mdl1$optVariables[1:50] #selecting top 50 features
  best_features <- na.omit(best_features)# feature selection
  
  # Subset data
  testd <- testd[, c(group,best_features)]
  
  # Predict
  pred <- predict(object=model, testd) 
  
  # Confusion matrix plot
  conf_mat <- confusionMatrix(data = pred, testd[[group]])
  conf_mat <- as.data.frame(conf_mat$table)
  
  p <- ggplot(data=conf_mat, aes(x=Prediction, y=Reference, fill=Freq)) +
    geom_tile() + # Use geom_tile for heatmap
    geom_text(aes(label=sprintf("%d", Freq)), vjust=1) + # Add text labels
    scale_fill_gradient(low="white",high = "mediumorchid3") + # Color gradient
    theme_minimal() + # Minimal theme
    labs(x='Predicted', y='Actual', fill='Frequency') +
    labs(title = "Confusion Matrix")

  return(p)

}


conf_matrix <- function(model, group, testd, mdl1){
  
  # Best features
  best_features = mdl1$optVariables # feature selection
  
  # Data subset
  testd <- testd[, c(group,best_features)]
  
  # Predict
  pred = predict(object=model, testd) 
  
  # Confusion matrix plot
  conf_mat = confusionMatrix(data = pred, testd[[group]])
  conf_mat = as.data.frame(conf_mat$table)
  
  p = ggplot(data=conf_mat, aes(x=Prediction, y=Reference, fill=Freq)) +
    geom_tile() + # Use geom_tile for heatmap
    geom_text(aes(label=sprintf("%d", Freq)), vjust=1) + # Add text labels
    scale_fill_gradient(low="white",high = "mediumorchid3") + # Color gradient
    theme_minimal() + # Minimal theme
    labs(x='Predicted', y='Actual', fill='Frequency') +
    labs(title = "Confusion Matrix")

  return(p)
  
}

conf_matrix2 <- function(model, group, random, traind, testd, mdl1){
  tryCatch(
    {
      levels <- levels(traind[[group]])
      
      # Including only selected features
      best_features = mdl1$optVariables # feature selection
      
      # Data subset
      testd <- testd[, c(group, random, best_features)]
      
      # Predict
      pred = predict(object=model, testd, type = "response")
      predicted_class <- ifelse(pred > 0.5, levels[2], levels[1])
      predicted_class <- as.factor(predicted_class)
      
      # Confusion matrix plot
      conf_mat = confusionMatrix(data =  predicted_class, testd[[group]])
      conf_mat = as.data.frame(conf_mat$table)
      
      p = ggplot(data=conf_mat, aes(x=Prediction, y=Reference, fill=Freq)) +
        geom_tile() + # Use geom_tile for heatmap
        geom_text(aes(label=sprintf("%d", Freq)), vjust=1) + # Add text labels
        scale_fill_gradient(low="white",high = "mediumorchid3") + # Color gradient
        theme_minimal() + # Minimal theme
        labs(x='Predicted', y='Actual', fill='Frequency') +
        labs(title = "Confusion Matrix")
      
      return(p)
    },
    error = function(e) {
      shinyalert(
        title = "Class imbalance found between train and test dataset.",
        text = paste("An error occurred:", e$message,
                     "NOTE: The random variable should have sufficient amount of samples in each category."),
        type = "error")
      
    }
  )
}


plot_imp <- function(model,n){

  n = as.numeric(n)
  
  # Variable importance
  vi <- varImp(object = model)
  vi <- vi$importance
  vi2 = vi[order(vi[,1],decreasing = T),]
  vi2 = vi2[1:n,] 
  
  # Plot variable importance
  viF = vi2 %>% 
    tibble::rownames_to_column(var = "Protein") %>%
    pivot_longer(cols = names(vi2),names_to = "Group",values_to = "Importance")

  ggdotchart(viF, x = "Protein", y = "Importance",
             palette = "Set1",group = "Group",combine = T,color= "Group",facet.by = "Group",
             rotate = TRUE,
             ggtheme = theme_bw(),
             size = 3) + ggtitle("Features by Importance")
  
}

plot_imp2 <- function(model,n){
  
  n = as.numeric(n)
  
  # Variable importance
  vi <- varImp(object = model)
  vi <- vi$importance
  vi2 <- data.frame(Importance = vi)
  vi2 <- vi2[order(vi2$Overall, decreasing = TRUE), , drop = FALSE]
  row_names <- rownames(vi2)
  selected_row_names <- row_names[1:n]
  vi2 <- vi2[selected_row_names, , drop = FALSE]
  
  viF = vi2 %>% 
    tibble::rownames_to_column(var = "Protein") %>%
    pivot_longer(cols = names(vi2),names_to = "Group",values_to = "Importance")
  
  
  ggdotchart(viF, x = "Protein", y = "Importance",
             palette = "Set1",group = "Group",combine = T,color= "Group",facet.by = "Group",
             rotate = TRUE,
             ggtheme = theme_bw(),
             size = 3) + ggtitle("Features by Importance")
  
}

plot_imp3 <- function(model,n){
  
  n = as.numeric(n)
  
  # Extract coefficients
  coefficients <- fixef(model)
  
  # Calculate importance (absolute values of coefficients)
  importance <- abs(coefficients)
  
  # Create a data frame for plotting
  coeff_df <- data.frame(Coefficient = names(coefficients), Importance = importance)
  
  # Sort the data frame by importance
  coeff_df <- coeff_df[order(coeff_df$Importance, decreasing = TRUE), ]
  
  ggplot(head(coeff_df,n), aes(x = reorder(Coefficient, Importance), y = Importance)) +
    geom_bar(stat = "identity", fill = "skyblue", color = "black") +
    labs(title = "Coefficient Importance", x = "Coefficient", y = "Importance") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    coord_flip()
  
}


imp_lmm <- function(model){
  
  # Extract coefficients
  coefficients <- fixef(model)
  
  # Calculate importance (absolute values of coefficients)
  importance <- abs(coefficients)
  
  # Create a data frame for plotting
  coeff_df <- data.frame(Coefficient = names(coefficients), Importance = importance)
  
  # Sort the data frame by importance
  coeff_df <- coeff_df[order(coeff_df$Importance, decreasing = TRUE), ]

  return(coeff_df)
  
}

############################################ XGBoost ##############################################################


xgb_model_tuning <- function(traind,testd, group){
  
  #generate a tunegrid or a vector of parameters ( in this case Cost) to be used for training.
  tuneGrid <- expand.grid(nrounds = c(100, 200),
                          max_depth = c(2, 4, 6),
                          eta = c(0.01, 0.1),
                          gamma = 0,
                          colsample_bytree = 0.6,
                          min_child_weight = 1,
                          subsample = 0.5)
  
   nrow(tuneGrid)
  
  traind[[group]] = factor(traind[[group]])
  
  if(length(levels(traind[[group]])) == 2){
    
    # Perform 10 fold CV
    fitControl <- trainControl(## 10-fold CV
      method = "repeatedcv",
      classProbs = T,
      number = 10,
      repeats = 1,savePredictions = "all",summaryFunction = twoClassSummary)
    
  } else if(length(levels(traind[[group]])) > 2){
    
    fitControl <- trainControl(## 10-fold CV
      method = "repeatedcv",
      classProbs = T,
      number = 10,
      repeats = 2,savePredictions = "all",summaryFunction = multiClassSummary)
    
  }
  # Create the formula ( eg. developed_RRMS ~.)
  Formula  = reformulate(".",response=group)
  
  num_cores = detectCores() - 1
  registerDoParallel(num_cores)
  
  # train the model inner loop
  mdl3 <- train(
    form = Formula,
    data = traind,
    method = "xgbTree",
    trControl = fitControl, tuneGrid = tuneGrid)
  
  stopImplicitCluster()
  
  #predict test data
  pred_cf = predict(object = mdl3,newdata = testd)
  #pred_prob = predict(object = mdl3,newdata = testd,type="prob")
  
  return(mdl3)
  
}

## Feature selection for Xgb 

xgb_feature_sel <- function(traind, testd, group){
  
  tuneGrid <- expand.grid(nrounds = c(100, 200),
                          max_depth = c(2, 4, 6),
                          eta = c(0.01, 0.1),
                          gamma = 0,
                          colsample_bytree = 0.6,
                          min_child_weight = 1,
                          subsample = 0.5)
  
  nrow(tuneGrid)
  
  traind[[group]] = factor(traind[[group]])
  
  
  # Create the formula ( eg. developed_RRMS ~.)
  Formula  = reformulate(".",response=group)
  
  num_cores = detectCores() - 1
  registerDoParallel(num_cores)
  
  ## run sbf
  sbf_fit <- sbf(
    form = Formula,
    data = traind,
    method = "xgbTree",
    tuneGrid=tuneGrid,
    trControl = trainControl(),
    sbfControl = sbfControl(method = "repeatedcv",
                            number = 10,
                            p = .8,
                            saveDetails = TRUE))
  
  
  
  return(sbf_fit)
  
}


## train xgb 

train_xgb <- function(traind, testd, group, mdl1, mdl2){
  
  best_params = mdl1$bestTune  ## model tuning
  best_features = mdl2$optVariables  # feature selection
  traindata = traind[,best_features] # subset train data with best features
  traindata[[group]] = traind[[group]]
  traind[[group]] = as.factor(traind[[group]])
  testd[[group]] = as.factor(testd[[group]])
  
  
  if(length(levels(traind[[group]])) == 2){

    # Perform 10 fold CV
    fitControl = trainControl(
      method = "repeatedcv",
      classProbs = T,
      number = 10,
      repeats = 2,savePredictions = "all",summaryFunction = twoClassSummary)

  } else if(length(levels(traind[[group]])) > 2) {
    
    
    fitControl = trainControl(
    method = "repeatedcv",
    classProbs = T,
    number = 10,
    repeats = 2,savePredictions = "all",summaryFunction = multiClassSummary)
    
  }
  
  
  Formula  = reformulate(".",response=group)
  
  
  # use best cost and best features on traindata from outer loop
  mdlF = train(
    form = Formula,
    data = traindata,
    method = "xgbTree",
    trControl = fitControl,tuneGrid = best_params)
  
  
  return(mdlF)
  
  
  
}

## 
xgb_plot_imp <- function(model,n){
  
  num = as.numeric(n)
  vi = varImp(object = model)
  vi = as.data.frame(vi$importance)
  vi2 = head(vi,n = num)
  
  viF = vi2 %>% 
    tibble::rownames_to_column(var = "Protein") %>%
    pivot_longer(cols = names(vi2),names_to = "Group",values_to = "Importance")
  
  
  ggdotchart(viF, x = "Protein", y = "Importance",
             palette = "Set1",group = "Group",combine = T,color= "Group",facet.by = "Group",
             rotate = TRUE,
             ggtheme = theme_bw(),
             size = 3) + ggtitle("Features by Importance")
  
}

############################################### Random Forest ################################################################################


rf_model_tuning <- function(traind,testd, group){
  
  #generate a tunegrid or a vector of parameters ( in this case Cost) to be used for training.
  tuneGrid <-  expand.grid(mtry = seq(10,20,10))
  nrow(tuneGrid)
  
  traind[[group]] = factor(traind[[group]])
  
  if(length(levels(traind[[group]])) == 2){
    
    # Perform 10 fold CV
    fitControl <- trainControl(## 10-fold CV
      method = "repeatedcv",
      classProbs = T,
      number = 10,
      repeats = 2,savePredictions = "all",summaryFunction = twoClassSummary)
    
  } else if(length(levels(traind[[group]])) > 2){
    
    fitControl <- trainControl(## 10-fold CV
      method = "repeatedcv",
      classProbs = T,
      number = 10,
      repeats = 2,savePredictions = "all",summaryFunction = multiClassSummary)
    
  }
  
  
  # Create the formula ( eg. developed_RRMS ~.)
  Formula  = reformulate(".",response=group)
  
  # train the model inner loop
  mdl3 <- train(
    form = Formula,
    data = traind,
    method = "rf",
    trControl = fitControl, tuneGrid = tuneGrid,importance = T,metric="ROC")
  
  #predict test data
  pred_cf = predict(object = mdl3,newdata = testd)
  #pred_prob = predict(object = mdl3,newdata = testd,type="prob")
  
  return(mdl3)
  
}

rf_feature_sel <- function(traind, testd, group){
  
  tuneGrid <-  expand.grid(mtry = seq(100,500,100))
  nrow(tuneGrid)
  
  traind[[group]] = factor(traind[[group]])
  
  
  # Create the formula ( eg. developed_RRMS ~.)
  Formula  = reformulate(".",response=group)
  
  # num = detectCores()-1
  # registerDoParallel(cores = num)
  
  ## run sbf
  sbf_fit <- sbf(
    form = Formula,
    data = traind,
    method = "rf",
    tuneGrid=tuneGrid,
    trControl = trainControl(),
    sbfControl = sbfControl(method = "repeatedcv",
                            number = 10,
                            p = .8,
                            saveDetails = TRUE))
  
#  stopImplicitCluster()
  return(sbf_fit)
  
}

train_rf <- function(traind, testd, group, mdl1, mdl2){
  
  best_param = mdl1$bestTune  ## model tuning
  best_features = mdl2$optVariables  # feature selection
  traindata = traind[,best_features] # subset train data with best features
  traindata[[group]] = traind[[group]]
  traindata[[group]] = as.factor(traindata[[group]])
  testdata = testd[,best_features]
  testdata[[group]] = testd[[group]]
  testd[[group]] = as.factor(testd[[group]])
  
  
  if(length(levels(traindata[[group]])) == 2){
    
    # Perform 10 fold CV
    fitControl <- trainControl(## 10-fold CV
      method = "repeatedcv",
      classProbs = T,
      number = 10,
      repeats = 2,savePredictions = "all",summaryFunction = twoClassSummary)
    
  } else if(length(levels(traindata[[group]])) > 2){
    
    fitControl <- trainControl(## 10-fold CV
      method = "repeatedcv",
      classProbs = T,
      number = 10,
      repeats = 2,savePredictions = "all",summaryFunction = multiClassSummary)
    
  }
  
  
  Formula  = reformulate(".",response=group)
  
  
  # use best cost and best features on traindata from outer loop
  mdlF = train(
    form = Formula,
    data = traindata,
    method = "rf",
    trControl = fitControl,tuneGrid = best_param,importance = T,metric="ROC")
  
  
  return(mdlF)
  
  
  
}

##### ROC analysis ###############################################


comp_models <- function(L){
  
  L2 = Filter(Negate(is.null), L)
  models = lapply(L2, function(inner_list) inner_list[[1]])
  models = as.list(models)
  resamps = resamples(x = models)
  df = resamps$values
  
  avail_metrics = unique(str_split_fixed(string = colnames(df),pattern = "~",n = 2)[,2])
  
  return(list(resamps, avail_metrics))
  
  
}



plot_all_roc2 <- function(L){
  L2 = Filter(Negate(is.null), L)
  rocs = lapply(L2, function(inner_list) inner_list[[2]])
  rocs = as.list(rocs)
  
  plot(NULL, xlim=c(0, 1), ylim=c(0, 1), xlab="False Positive Rate", ylab="True Positive Rate")
  for (i in 1:length(rocs)) {
    plot.roc(rocs[[i]], add=TRUE, col=rainbow(length(rocs))[i], print.auc=TRUE)
  }
  legend("bottomright", legend= names(rocs), col=rainbow(length(rocs)), lty=1, cex=0.8)
  
  
}






  

  
  
  
  

  
  
  
  
  