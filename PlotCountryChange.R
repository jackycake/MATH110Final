#' Construct of the PlotCountryChange object
#' 
#' @param ts time series object eg, staples1tsfood
#' 
#' @details A PlotCountryChange object is a melted dataframe
#' that allows for easy plotting using ggplot2
#' with columns Year, variable, and value.
#' 
#' Year are the years from 1961 to 2013,
#' variable is the Item name, and value is 
#' the annual percent change in production.
#' 
#' @return PlotCountry object


PlotCountryChange<- function(ts)
{
  pc<- percentChange(ts)
  #I used the function in the tfplot package to calculate 
  #percent change for production outputs. The package takes a
  #time series vector or matrix.
  df<-data.frame(pc)
  df$Year<-(1962:2013)
  new_df<- df %>% select(Year, everything())
  meltdf <- melt(new_df,id="Year")
  
  return(meltdf)
}