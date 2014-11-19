
#' @title calculate_roc_binary_classifier
#' @description
#' Given a fitted binary classification from caret training and tuning, and a test set, 
#' calculate the ROC curve and AUC, returning a list of both.  Uses the ROCR package 
#' since it can handle the original factors and doesn't need the levels to be turned
#' numeric first.  
#' 
#' @param fit Tuned and fitting model from caret's train() function
#' @param test Data frame representing test data
#' @param label Column name representing the true label in the test set
#' @return list List with a ROC object and AUC
#' @export

calculate_roc_binary_classifier <- function(fit = NULL, test = NULL, label_class = NULL, curve_title = NULL ) {
  require(ROCR)
  rf.pr = predict(fit,type="prob",newdata=test)[,2]
  rf.pred = prediction(rf.pr, test[,label_class])
  rf.perf = performance(rf.pred, "tpr", "fpr")
  rf.auc = performance(rf.pred, "auc")
  retval <- list("roc" = rf.perf, "auc" = rf.auc, "title" = curve_title)
  retval
}


#'@title plot_roc
#'@description
#'Plots a single ROC curve with diagonal abline, given output objects from calculate_roc_binary_classifier()
#'
#'@param list Output object from calculate_roc_binary_classifier()
#'@export

plot_roc <- function(roc_obj) {
  require(ROCR)
  require(ggplot2)
  require(ggthemes)
  
  rocData <- roc_obj[["roc"]]
  auc_obj <- roc_obj[["auc"]]
  #aval <- round(auc_obj@y.values[[1]], 4)
  #label_text <- paste(sep = "", "AUC = ", aval)
  plot_title <- paste(sep = "", "ROC Analysis: ", roc_obj[["title"]])
  
  rocDataFrame <- data.frame(x=rocData@x.values[[1]],y=rocData@y.values[[1]])
  rocr.plot <- ggplot(data=rocDataFrame, aes(x=x, y=y), ) + geom_path(size=1, color="red") #+ geom_text(aes(x=1, y= 0, hjust=1, vjust=0, label=label_text,colour="black",size=8))
  rocr.plot <- rocr.plot + coord_fixed()
  rocr.plot <- rocr.plot + xlab("False Positive Rate") + ylab("True Positive Rate") + ggtitle(plot_title)
  rocr.plot <- rocr.plot + geom_abline(intercept = 0, slope = 1, color="grey")
  rocr.plot <- rocr.plot + theme_pander()
  rocr.plot
}


#' @title plot_multiple_roc
#' @description
#' Plots multiple ROC curves with diagonal abline, given multiple output objects from calculate_roc_binary_classifier()
#' 
#' @param multiple Output objects from calculate_roc_binary_classifier()
#' @return ggplot2 plot object
#' @export
 
plot_multiple_roc <- function(...) {
  require(ROCR)
  require(ggplot2)
  require(ggthemes)
  
  input_list <- list(...)
  plot_multiple_roc_impl(input_list)
}

#' @title plot_multiple_roc_from_list
#' @description
#' Plots multiple ROC curves with diagonal abline, given a list of output objects from calculate_roc_binary_classifier().  
#' Alternate wrapper for the function given that sometimes we'll already have a list of objects, say from a long-running
#' analysis, and not be passing in individual objects.  
#' 
#' @param multiple Output objects from calculate_roc_binary_classifier()
#' @return ggplot2 plot object
#' @export

plot_multiple_roc_from_list <- function(input_list) {
  plot_multiple_roc_impl(input_list)
}

# not exported, internal function
plot_multiple_roc_impl <- function (input_list) {
  plot_df <- data.frame(t(rep(NA,3)))
  names(plot_df) <- c("x","y","curve_label")
  
  for (roc_obj in input_list) {
    rocData <- roc_obj[["roc"]]
    curve <- roc_obj[["title"]]
    
    rocDataFrame <- data.frame(x=rocData@x.values[[1]],y=rocData@y.values[[1]],curve_label=curve)
    plot_df <- rbind(plot_df, rocDataFrame)
  }
  
  # remove the initializer of NA's from the data frame
  plot_df <- na.omit(plot_df)
  
  
  rocr.plot <- ggplot(data=plot_df, aes(x=x, y=y, color=curve_label), ) + geom_path(size=1) 
  rocr.plot <- rocr.plot + coord_fixed() + labs(color="ROC Comparison")
  rocr.plot <- rocr.plot + xlab("False Positive Rate") + ylab("True Positive Rate") 
  rocr.plot <- rocr.plot + geom_abline(intercept = 0, slope = 1, color="grey")
  rocr.plot <- rocr.plot + theme_pander()
  rocr.plot
}

#' @title train_randomforest
#' @description
#' Using the training and tuning function from the caret library, tune a random forest 
#' model on the training fraction of a data frame, after excluding a set of columns.  
#' Tuning occurs given the control and tuning grid objects passed, and the final fitted
#' object is returned along with the elapsed time, training and test data sets in a list. 
#' 
#'  @param data.frame the full data set, without splitting into train/test
#'  @param num the fraction of the data to use for training
#'  @param object A tuning grid object of the type used by the caret library (in this case a vector of mtry values)
#'  @param object A training control object of the type used by the caret library
#'  @param character A vector of the names of columns to exclude from the analysis
#'  @return list A list with the fitted result, the training and test data sets, and the elapsed time
#'  @export 

train_randomforest <- function(df, training_fraction, class_field, tuning_grid, training_control, exclude) {
  retval <- list()
  df_excluded_fields <- df[,!(names(df) %in% exclude)]
  
  nonTestIndex <- createDataPartition(df_excluded_fields$two_class_label, p = training_fraction,
                                      list = FALSE,
                                      times = 1)
  
  df_train <- df_excluded_fields[ nonTestIndex,]
  df_test  <- df_excluded_fields[-nonTestIndex,]
  
  start_time <- proc.time()[["elapsed"]]
  
  fit <- train(two_class_label ~ ., data = df_train,
               method="rf",
               verbose=FALSE,
               trControl = training_control,
               tuneGrid = tuning_grid)
  
  end_time <- proc.time()[["elapsed"]]
  sampled_training_minutes <- (end_time - start_time) / 60
  
  retval <- list("test_data" = df_test, "train_data" = df_train, "tunedmodel" = fit, "elapsed" = sampled_training_minutes)
  
  retval
}


#' @title train_gbm_classifier
#' @description
#' Using the training and tuning function from the caret library, tune a random forest 
#' model on the training fraction of a data frame, after excluding a set of columns.  
#' Tuning occurs given the control and tuning grid objects passed, and the final fitted
#' object is returned along with the elapsed time, training and test data sets in a list. 
#' 
#'  @param data.frame the full data set, without splitting into train/test
#'  @param num the fraction of the data to use for training
#'  @param character the name of the column which contains true class labels
#'  @param object A tuning grid object of the type used by the caret library (in this case a vector of mtry values)
#'  @param object A training control object of the type used by the caret library
#'  @param character A vector of the names of columns to exclude from the analysis
#'  @return list A list with the fitted result, the training and test data sets, and the elapsed time
#'  @export 

train_gbm_classifier <- function(df, training_fraction, class_field, tuning_grid, training_control, exclude, verbose = FALSE) {
  retval <- list()
  df_excluded_fields <- df[,!(names(df) %in% exclude)]
  
  nonTestIndex <- createDataPartition(df_excluded_fields[[class_field]], p = training_fraction,
                                      list = FALSE,
                                      times = 1)
  
  df_train <- df_excluded_fields[ nonTestIndex,]
  df_test  <- df_excluded_fields[-nonTestIndex,]
  
  form <- as.formula(paste(class_field, "~", ".", sep = " "))
  
  start_time <- proc.time()[["elapsed"]]
  
  fit <- train(form, data = df_train,
               method="gbm",
               trControl = training_control,
               tuneGrid = tuning_grid,
               verbose=verbose)
  
  end_time <- proc.time()[["elapsed"]]
  sampled_training_minutes <- (end_time - start_time) / 60
  
  retval <- list("test_data" = df_test, "train_data" = df_train, "tunedmodel" = fit, "elapsed" = sampled_training_minutes)
  
  retval
}

#' @title get_sorted_variable_importance
#' @description 
#' Given a fitted model object from caret's train() method, this utility method
#' produces a properly sorted data frame with the predictor names in a column
#' rather than as row.names, renames the columns for use in publications, and sorts
#' by importance value in descending order.  The resulting data frame produces a 
#' good table in pander() and other methods.  
#' 
#' @param model Fitted and tuned caret model object
#' @return data.frame Sorted and rectified variable importance table
#' @export


get_sorted_variable_importance <- function(model) {
  require(dplyr)
  varimp <- varImp(model)
  varimp_df <- varimp[["importance"]]
  varimp_df$Predictor <- row.names(varimp_df)
  names(varimp_df)[names(varimp_df)=="Overall"] <- "Importance"
  arrange(varimp_df, desc(Importance))
}

#' @title get_parsed_binary_confusion_matrix_stats
#' @description
#' Helper function which takes a caret package confusionMatrix object, created by 
#' the confusionMatrix() function, and produces a single-row data frame with common variables
#' parsed from the object.  These include the overall misclassification rate (1-accuracy), accuracy, 
#' Cohen's kappa, ppv and npv, sensitivity and specificity, and a field which indicates the "positive" 
#' label for those stats which treat one label as a "positive identification".  
#' 
#' @param confusionMatrix Caret package confusionMatrix object, produced typically with test data
#' @return data.frame
#' @export

get_parsed_binary_confusion_matrix_stats <- function(cm) {
  total_n <- cm$table[[1]] + cm$table[[2]] + cm$table[[3]] + cm$table[[4]]
  misclassification_rate <- (cm$table[[2]] + cm$table[[3]]) / total_n
  
  stats <- list("misclassification_rate" = misclassification_rate, 
                "accuracy" = cm$overall[["Accuracy"]],
                "kappa" = cm$overall[["Kappa"]],
                "ppv" = cm$byClass[["Pos Pred Value"]],
                "npv" = cm$byClass[["Neg Pred Value"]],
                "sensitivity" = cm$byClass[["Sensitivity"]],
                "specificity" = cm$byClass[["Specificity"]],
                "positive_label"= cm$positive)
  
  as.data.frame(stats)
}






