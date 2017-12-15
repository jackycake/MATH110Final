#' Construct of the Countries object
#' 
#' @param data.frame eg, df
#' 
#' @details A Countries object is a data.frame with 
#' columns of Area Abbreviation, Area Code, 
#' and Area.
#' 
#'Area Abbreviation is the abbreviaton 
#' of the Country name, Area Code is the number 
#' assigned to a Country, and Area is the 
#' name of the country.  
#' 
#' df is the path file for the data.
#' 
#' @return Countries object


Countries<- function(df)
{
  filt_df <- dplyr::select(df, Area.Abbreviation, Area.Code, Area)
  filt_df <- unique(filt_df)
  return(filt_df)
}

get_num.Countries <- function(cy)
{
  num<-length(cy$Area.Code)
  return(num)
}

get_CountryCode.Countries<- function(cy, countryname)
{
  filt_row<-cy[grepl(countryname, cy$Area), ]  
  browser()
  x <- filt_row$Area.Code
  return(x)
}
