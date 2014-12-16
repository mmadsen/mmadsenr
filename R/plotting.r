
#'@title arrange_ggplot2
#'@description
#'Arranges ggplot2 plot objects in a grid using code from Stephen Turner's website.
#'from http://gettinggeneticsdone.blogspot.com/2010/03/arrange-multiple-ggplot2-plots-in-same.html
#'use pdf(); arrange(p1,p2,ncol=1); dev.off() to save the plot to a file
#'@param list list of plot objects
#'@param ncol number of columns, can be null
#'@param nrow number of rows, can be null
#'@param as.table boolean, determines order in grid
#'@export

arrange_ggplot2 <- function(..., nrow=NULL, ncol=NULL, as.table=FALSE) {
  library(ggplot2)
  library(grid)
  vp.layout <- function(x, y) viewport(layout.pos.row=x, layout.pos.col=y)
  
  dots <- list(...)
  n <- length(dots)
  if(is.null(nrow) & is.null(ncol)) { nrow = floor(n/2) ; ncol = ceiling(n/nrow)}
  if(is.null(nrow)) { nrow = ceiling(n/ncol)}
  if(is.null(ncol)) { ncol = ceiling(n/nrow)}
  ## NOTE see n2mfrow in grDevices for possible alternative
  grid.newpage()
  pushViewport(viewport(layout=grid.layout(nrow,ncol) ) )
  ii.p <- 1
  for(ii.row in seq(1, nrow)){
    ii.table.row <- ii.row	
    if(as.table) {ii.table.row <- nrow - ii.table.row + 1}
    for(ii.col in seq(1, ncol)){
      ii.table <- ii.p
      if(ii.p > n) break
      print(dots[[ii.table]], vp=vp.layout(ii.table.row, ii.col))
      ii.p <- ii.p + 1
    }
  }
}

#'@title vertical_dotchart
#'@description
#'Produces a vertical dot chart, with a metric variable giving the length of a horizontal 
#'line segment and a dot colored by a grouping variable.  Each row in the data frame becomes a 
#'row in the dotchart.  The y variable is a label for each row, and rows can be grouped
#'by a grouping variable.  The plot can be sorted in descending order of the x metric variable,
#'or grouped by the grouping variable and then sorted.   
#'
#'@param df Data frame to be plotted
#'@param x_var Name of the metric variable to be plotted, as a string
#'@param x_label Name of the metric variable as a plot legend string
#'@param y_var Name of the categorical variable labeling each row in the data frame
#'@param y_label Name of the categorical variable, as a plot legend string
#'@param y_group_var Name of a categorical variable which groups the rows
#'@param legend_title Title of the legend for groups, defaults to "Experiment Group"
#'@param sort.by.xvar Boolean flag to sort rows by the metric variable, defaults to TRUE
#'@param group.by.ygroup Boolean flag to sort groups of rows by the grouping variable, defaults to TRUE
#'@return ggplot2 object
#'@export

vertical_dotchart <- function(df, 
                              x_var = xvar, 
                              x_label = xlabel, 
                              y_var = yvar, 
                              y_label = "Experiment", 
                              y_group_var = ygroup, 
                              legend_title = "Experiment Group",
                              sort.by.xvar = TRUE, 
                              group.by.ygroup = TRUE) {
  
  require(ggplot2)
  require(ggthemes)

  if(sort.by.xvar == TRUE) {
      df$yvar_sorted <- reorder(df[[y_var]], df[[x_var]])
      plt <- ggplot(df, aes_string(x = x_var, y = "yvar_sorted")) 
    } else {
      plt <- ggplot(df, aes_string(x = x_var, y = y_var))
    }

  plt <- plt + geom_segment(aes(yend = experiments), xend = 0, color = "grey50")
  plt <- plt + ylab(y_label)
  plt <- plt + xlab(x_label)
  plt <- plt + geom_point(size = 3, aes(color = exp_group)) + labs(color = legend_title)
  plt <- plt + theme_pander()
  plt <- plt + theme(panel.grid.major.x = element_blank(),
                     panel.grid.minor.x = element_blank(),
                     strip.background = element_blank(), strip.text = element_blank())
  if(group.by.ygroup == TRUE) {
    form <- as.formula(paste(y_group_var, "~", ".", sep = " "))
    plt <- plt + facet_grid(form, scales = "free_y", space = "free_y")
  }
  
  plt
}







