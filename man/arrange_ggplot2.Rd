% Generated by roxygen2 (4.0.2): do not edit by hand
\name{arrange_ggplot2}
\alias{arrange_ggplot2}
\title{arrange_ggplot2}
\usage{
arrange_ggplot2(..., nrow = NULL, ncol = NULL, as.table = FALSE)
}
\arguments{
\item{nrow}{number of rows, can be null}

\item{ncol}{number of columns, can be null}

\item{as.table}{boolean, determines order in grid}

\item{list}{list of plot objects}
}
\description{
Arranges ggplot2 plot objects in a grid using code from Stephen Turner's website.
from http://gettinggeneticsdone.blogspot.com/2010/03/arrange-multiple-ggplot2-plots-in-same.html
use pdf(); arrange(p1,p2,ncol=1); dev.off() to save the plot to a file
}

