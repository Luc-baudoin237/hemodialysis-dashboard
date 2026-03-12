library(jsonlite)
library(dplyr)

build_dictionary <- function(metadata_file){
  
  metadata <- fromJSON(metadata_file)
  
  if (is.null(metadata$dataElements)) {
    stop("No dataElements found in metadata.json")
  }
  
  dict <- metadata$dataElements %>%
    select(
      dataElement = id,
      name,
      shortName,
      code,
      valueType
    )
  
  return(dict)
}