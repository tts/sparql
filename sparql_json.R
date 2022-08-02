library(tidyverse)

sparql_endpoint <- "http://dbpedia.org/sparql"

# https://medium.com/virtuoso-blog/dbpedia-basic-queries-bc1ac172cc09
q <- "
SELECT ?athlete ?cityName
WHERE 
  {
   ?athlete rdfs:label 'Cristiano Ronaldo'@en ;
            dbo:birthPlace ?place .
   
   ?place a dbo:City ;
            rdfs:label ?cityName .
   
   FILTER ( LANG ( ?cityName ) = 'en' )
}"

# Simplified/modified function from https://github.com/kvasilopoulos/uklr/blob/master/R/query.R
sparql <- function(query, endpoint = sparql_endpoint, ...){
  
  enc_query <- gsub("\\+", "%2B", URLencode(q, reserved = TRUE))
  
  res <- httr::GET(
    paste(endpoint, "?query=", enc_query, sep = ""),
    httr::add_headers("Accept" = "application/sparql-results+json"),
    ...
  )
  
  res
}

process_json <- function(res) {
  res <- jsonlite::parse_json(res, simplifyVector = TRUE)$results$bindings
}

result <- process_json(sparql(q))

result_df <- do.call(data.frame, result) %>% 
  select(ends_with("value"))


