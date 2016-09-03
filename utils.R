# First we check that the necessary packages are installed

req.packages=c("shinydashboard","shiny","shinyBS","RODBC","DT","plotly","dplyr")
if(any(! req.packages  %in% installed.packages()))
  stop(
    paste0("Not all dependent packages are installed on your computer.\n Please install: ",
           paste(req.packages[!req.packages %in% installed.packages()],collapse=","),
           ". See '?install.packages' for more information on how to install R packages.")
  )

require(shiny)
require(shinyBS)
require(shinydashboard)
require(RODBC)
require(DT)
require(plotly)
require(dplyr)


#' Two custom functions are used in this app (ignoring the simple axis
#' formatting function)
#' 
#' The first custom function produces a density plot using plotly.
#' 
#' As its required arguments, it takes a data frame (dataset) and the
#' corresponding column name (valueCol) from which the density plot is
#' generated.
#' 
#' Additional arguments are the group column name (groupCol- if you want
#' multiple curves on the same plot) and from, which determines starting point
#' of the density plot(s)  ( density plots can be negative on the x-axis even
#' for a set of positive numbers, setting from=0 would prevent such negative
#' values)
#' 
#' The function returns a plotly graph

densityplotly=function(dataset,valueCol,groupCol="",from=NULL){
if(groupCol==""){
  
  if(is.null(from)){
  dens=density(dataset[,valueCol])
  }else{dens=density(dataset[,valueCol],from=from)}
  
  plot_ly(dens, x = x, y = y)

}else{
  
  if(is.null(from)){
  dens=with(dataset, tapply(eval(parse(text = valueCol)), INDEX = eval(parse(text = groupCol)), density))
  }else{dens=with(dataset, tapply(eval(parse(text = valueCol)),
                                INDEX = eval(parse(text = groupCol)),function(x){density(x,from=from)}))}

  df <- data.frame(
  x = unlist(lapply(dens, "[[", "x")),
  y = unlist(lapply(dens, "[[", "y")),
  my_group = rep(names(dens), each = length(dens[[1]]$x))
)

  plot_ly(df, x = x, y = y, color = my_group)
}
}


#' The second custom function produces a cumulative distribution plot using
#' plotly.
#' 
#' As its required arguments, it takes a data frame (dataset) and the
#' corresponding column name (valueCol) from which the cumulative distribution
#' plot is generated.
#' 
#' An additional argument (groupCol) is the group column name (if you want
#' multiple curves on the same plot)
#' 
#' Note: To prevent the function being too computationally expensive, it only calculates the
#' cumulative value at 1000 points, sampled evenly between the max and min values.
#' For most scenarios, I imagine this produces a relatively smooth curve without slowing
#' performance.
#' 
#' The function returns a plotly graph

cumultplotly=function(dataset,valueCol,groupCol=""){
  
  dataset=arrange_(dataset,valueCol)
  plot.min=floor(min(dataset[,valueCol]))
  plot.max=ceiling(max(dataset[,valueCol]))
  plot.range=seq(plot.min,plot.max,(plot.max-plot.min)/1000)
  
  if(groupCol==""){
    
  cudist=ecdf(dataset[,valueCol])
  output=data.frame(x=plot.range,y=cudist(plot.range))
  
  plot_ly(output, x = x, y = y*100)
  
  }else{
    
    cudist=with(dataset, tapply(eval(parse(text = valueCol)), INDEX = eval(parse(text = groupCol)), ecdf))
    
    df <- data.frame(
      x = rep(plot.range,length(names(cudist))),
      y = unlist(lapply(cudist,function(a){a(plot.range)})),
      my_group = rep(names(cudist), each = length(plot.range))
    )
    
    plot_ly(df, x = x, y = y*100, color = my_group)
  }
}