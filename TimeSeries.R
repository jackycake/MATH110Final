#' Construct of the TimeSeries object
#' 
#' @param df data.frame eg, df
#' @param countrycode string eg, "41"
#' @param element string eg, "Food"
#' @param itemcode vector eg, c("2511","2518")
#' 
#' @details A TimeSeries object is a time series object 
#' with production outputs for each year for every Item from 
#' the years 1961 to 2013. 
#' 
#' @return TimeSeries object
TimeSeries<- function(df, countrycode, element, itemcode)
{
  filt_df <- dplyr::filter(df, Area.Code== countrycode)
  filt_df <- dplyr::filter(filt_df, Element == element)
  filt_df <- dplyr::filter(filt_df, Item.Code %in% itemcode)
  food_matrix <- dplyr::select(filt_df, -Area, -Area.Abbreviation, 
                               -Area.Code, -Item.Code, -Element, 
                               -Element.Code, -Unit, -latitude, -longitude)
  rev_food_matrix <- setNames(data.frame(t(food_matrix[,-1])), food_matrix[,1])
  #^I did this to retain the Item names for each column
  #after I transposed the data frame.
  tseries <- ts(rev_food_matrix, start = 1961, end=2013, frequency = 1)
  return(tseries)
}
