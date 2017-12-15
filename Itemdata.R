#' Construct of the Itemdata object
#' 
#' @param data.frame eg, df
#' 
#' @details A Itemdata object is a data.frame with 
#' columns of Items, Item codes, 
#' Elements, and Element codes.
#' 
#' An Item is the name of the agriculture 
#' product produced, Item code is the number 
#' assigned to an Item, Element is the type 
#' of agriculture product (Food is an 
#' agriculture product produced for human
#' consumption and Feed is produced for 
#' animal consumption), Element code is 
#' the number assigned to an Element. 
#' 
#' df is the path file for the data.
#' 
#' @return Itemdata object

Itemdata<- function(df)
{
  filt_df <- dplyr::filter(df, Area.Code== "41")
  filt_df <- dplyr::select(filt_df, Item, Item.Code, Element, Element.Code)
  return(filt_df)
}

get_Items.Itemdata <- function(id)
{
  return(unique(id$Item))
}

get_num.Itemdata <- function(id)
{
  num<-length(unique(id$Item))
  return(num)
}

get_ItemCode.Itemdata <- function(id, item)
{
  filt_row<-id[grepl(item, id$Item), ]  
  x <- filt_row$Item.Code
  return(unique(x))
}

get_ElementCode.Itemdata <- function(id, element)
{
  filt_row<-id[grepl(element, id$Element), ]  
  x <- filt_row$Element.Code
  return(unique(x))
}