library(shiny)
library(shinydashboard)
library(DT)
library(tidyverse)
library(shinycssloaders)

sparql_endpoint <- "https://ldf.fi/semparl/sparql"
ua <- httr::user_agent("https://github.com/tts/sparql")

sparql <- function(query, endpoint = sparql_endpoint, ...){
  enc_query <- gsub("\\+", "%2B", URLencode(query, reserved = TRUE))
  res <- httr::GET(
    paste(endpoint, "?query=", enc_query, sep = ""),
    httr::add_headers("Accept" = "application/sparql-results+json"),
    ua,
    ...
  )
  res
}

process_json <- function(res) {
  res <- jsonlite::parse_json(res, simplifyVector = TRUE)$results$bindings
}

options(spinner.type  = 7,
        spinner.size  = 0.5,
        spinner.color = "#ff6502")

ui <- function(request) {
  
  sidebar <- dashboardSidebar(
    width = 300,
    sidebarMenu(
      textInput(inputId = "search",
                label = "Hakusana (katkaisumerkki = *)",
                placeholder = "esim. olkiluo*"),
      actionButton("do", "Hae!")
    ))
  
  
  body <- dashboardBody(
    fluidRow(
      column(width = 12,
             height = "300px",
             shinycssloaders::withSpinner(
               DTOutput("table")
               )
             )
    )
  )
  
  dashboardPage(
    dashboardHeader(title = "Parlamenttisampo-haku", titleWidth = "800"),
    sidebar,
    body,
    skin = "black"
  )
  
}


server <- function(input, output, session) {
  
 # Escaping in the query needs 4 backslashes
 # https://github.com/eclipse/rdf4j/issues/1105#issuecomment-652204116
  
  result <- eventReactive(
    input$do, {
      q <- paste0('
  PREFIX dct: <http://purl.org/dc/terms/>
  PREFIX semparls: <http://ldf.fi/schema/semparl/>
  PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
  PREFIX text: <http://jena.apache.org/text#>
  PREFIX facets: <http://ldf.fi/schema/parliamentsampo-portal/>

  SELECT *
  WHERE {
    {
      SELECT DISTINCT ?id ?score ?literal {
        ?id text:query ( semparls:content ','"',input$search,'"',' 10000000  ) .
        VALUES ?facetClass { semparls:Speech }
        ?id a ?facetClass .
        OPTIONAL { ?id dct:date ?orderBy }
      }
      ORDER BY (!BOUND(?orderBy)) desc(?orderBy) # ttso: changed asc->desc
      LIMIT 10 OFFSET 0 # ttso: changed to 10
    }
    FILTER(BOUND(?id))
    # score and literal are used only for Jena full text index, but may slow down the query performance
    ( ?id ?score ?literal ) text:query ( semparls:content ','"',input$search,'"',' 10000000 "highlight:s:<b> | e:</b> | z:150" ) .


  ?id skos:prefLabel ?prefLabel__id .
  BIND(?prefLabel__id as ?prefLabel__prefLabel)
  BIND(CONCAT("/speeches/page/", REPLACE(STR(?id), "^.*\\\\/(.+)", "$1")) AS ?prefLabel__dataProviderUrl)
  BIND(?id as ?uri__id)
  BIND(?id as ?uri__dataProviderUrl)
  BIND(?id as ?uri__prefLabel)
  {
    ?id semparls:speaker ?speaker__id .
    ?speaker__id skos:prefLabel ?speaker__prefLabel .
    BIND(CONCAT("/people/page/", REPLACE(STR(?speaker__id), "^.*\\\\/(.+)", "$1")) AS ?speaker__dataProviderUrl)
  }
  UNION
  {
    ?id semparls:party ?party__id .
    ?party__id skos:prefLabel ?party__prefLabel .
    FILTER(LANG(?party__prefLabel) = "fi")
    BIND(CONCAT("/groups/page/", REPLACE(STR(?party__id), "^.*\\\\/(.+)", "$1")) AS ?party__dataProviderUrl)
  }
  UNION
  {
    ?id semparls:speechType ?speechType__id .
    ?speechType__id skos:prefLabel ?speechType__prefLabel .
  }
  UNION
  {
    ?id dct:language ?language_ .
    BIND(REPLACE(STR(?language_), "http://id.loc.gov/vocabulary/iso639-2/", "") as ?language)
  }
  UNION
  {
    ?id dct:date ?date_ .
    BIND(CONCAT(STR(DAY(?date_)),
                     ".",
                     STR(MONTH(?date_)),
                     ".",
                    STR(YEAR(?date_))) as ?date)
  }
  UNION
  {
    ?id semparls:item ?item__id .
    ?item__id skos:prefLabel ?item__prefLabel .
    BIND(CONCAT("/items/page/", REPLACE(STR(?item__id), "^.*\\\\/(.+)", "$1")) AS ?item__dataProviderUrl) .
  }
  UNION
  {
    ?id semparls:content ?content .
  }

  }')
      res <- process_json(sparql(q))
      
      validate(need(length(res)>0, message = "Ei löytynyt mitään!"))
      
      res_df <- do.call(data.frame, res) %>% 
        select(id.value, prefLabel__id.value, speaker__id.value, 
               party__prefLabel.value, speechType__prefLabel.value,
               date_.value, content.value)
      df_cleaned <- res_df %>% 
        group_by(id.value, prefLabel__id.value, .drop = FALSE) %>% 
        fill(speaker__id.value, .direction = "down") %>% 
        fill(party__prefLabel.value, .direction = "downup") %>% 
        fill(speechType__prefLabel.value, .direction = "downup") %>% 
        fill(date_.value, .direction = "downup") %>% 
        fill(content.value, .direction = "downup") %>% 
        ungroup() %>% 
        rename(id = id.value,
               tapahtuma = prefLabel__id.value,
               puhuja = speaker__id.value,
               puolue = party__prefLabel.value,
               puhetyyppi = speechType__prefLabel.value,
               päiväys = date_.value,
               puhe = content.value) %>% 
        mutate( id = sub("http://ldf.fi/semparl/speeches/", "https://parlamenttisampo.fi/speeches/page/", id),
                puhuja = sub("http://ldf.fi/semparl/people/", "https://parlamenttisampo.fi/people/page/", puhuja),
                id = paste0('<a href="',id,'" target="_blank">', id, '</a>'),
                puhuja = paste0('<a href="', puhuja,'" target="_blank">', puhuja, '</a>'))
      
      df_distinct <- distinct(df_cleaned, id, .keep_all = TRUE)
    })
      
      output$table <- renderDT(
        datatable(result(), 
                  escape = c(TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE), 
                  options = list(columnDefs = list(list(
                    targets = 7,
                    render = JS(
            "function(data, type, row, meta) {",
            "return type === 'display' && data.length > 100 ?",
            "'<span title=\"' + data + '\">' + data.substr(0, 100) + '...</span>' : data;",
            "}")
      ))))
      )
}


shinyApp(ui = ui, server = server)

