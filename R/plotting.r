## plotting functions
library(ggplot2)



## Function for arranging ggplots. use png(); arrange(p1, p2, ncol=1); dev.off() to save.
## from http://gettinggeneticsdone.blogspot.com/2010/03/arrange-multiple-ggplot2-plots-in-same.html
# Load the diamonds dataset
# data(diamonds)
# Create a histogram, assign to "plot1"
# plot1 <- qplot(price,data=diamonds,binwidth=1000)
# Create a scatterplot
# plot2 <- qplot(carat,price,data=diamonds)
#
# Arrange and display the plots into a 2x1 grid
# arrange_ggplot2(plot1,plot2,ncol=1)
#


library(grid)
vp.layout <- function(x, y) viewport(layout.pos.row=x, layout.pos.col=y)

arrange_ggplot2 <- function(..., nrow=NULL, ncol=NULL, as.table=FALSE) {
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