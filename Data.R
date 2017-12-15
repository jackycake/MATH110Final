load_fao_data <- function()
{
  df <- read.csv(fao_csv, header=T, stringsAsFactors = F)
  return(df)
}

df <- load_fao_data()
