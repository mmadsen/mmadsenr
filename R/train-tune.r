
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
#' @export
 
plot_multiple_roc <- function(...) {
  require(ROCR)
  require(ggplot2)
  require(ggthemes)
  
  input_list <- list(...)
  
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
  rocr.plot <- rocr.plot + coord_fixed() + labs(color="ROC Analysis")
  rocr.plot <- rocr.plot + xlab("False Positive Rate") + ylab("True Positive Rate") 
  rocr.plot <- rocr.plot + geom_abline(intercept = 0, slope = 1, color="grey")
  rocr.plot <- rocr.plot + theme_pander()
  rocr.plot
}








