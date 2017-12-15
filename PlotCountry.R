#' Construct of the PlotCountry object
#' 
#' @param df data.frame eg, df
#' @param countrycode string eg, "41"
#' @param element string eg, "Food"
#' @param itemcode vector eg, c("2511","2518")
#' 
#' @details A Plot Country object is a melted dataframe
#' that allows for easy plotting using ggplot2
#' with columns Year, variable, and value.
#' 
#' Year are the years from 1961 to 2013,
#' variable is the Item name, and value is 
#' the recorded production output for a given year.
#' 
#' @return PlotCountry object

PlotCountry<- function(df, countrycode, element, itemcode)
{
  filt_df <- dplyr::filter(df, Area.Code== countrycode)
  filt_df <- dplyr::filter(filt_df, Element == element)
  filt_df <- dplyr::filter(filt_df, Item.Code %in% itemcode)
  
  food_matrix <- dplyr::select(filt_df, -Area, -Area.Abbreviation, 
                               -Area.Code, -Item.Code, -Element, 
                               -Element.Code, -Unit, -latitude, -longitude)
  rev_food_matrix <- setNames(data.frame(t(food_matrix[,-1])), food_matrix[,1])
  new_df<-data.frame(rev_food_matrix)
  new_df$Year<-(1961:2013)
  #I needed to add a Year column because the melt function 
  #melts the data frame so that each row 
  #is a unique id-variable combination,
  #I wanted the id to be the Year.
  newnew_df<- new_df %>% select(Year, everything())
  meltdf <- melt(newnew_df,id="Year")
  return(meltdf)
}
