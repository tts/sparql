library(shiny)
library(shinydashboard)
library(DT)
library(tidyr)
library(dplyr)
library(shinycssloaders)
library(openai)

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
    width = 200,
    sidebarMenu(
      textInput(inputId = "search",
                label = "Hakusana, katkaisu=*",
                placeholder = "esim. olkiluo*"),
      actionButton("do", "Hae")
    ))
  
  body <- dashboardBody(
    fluidRow(
      column(width = 12,
             height = "600px",
             shinycssloaders::withSpinner(
               DTOutput("table")
             )
      )
    ),
    fluidRow(
      column(width = 3,
             uiOutput("dopic")),
      column(width = 9,
             shinycssloaders::withSpinner(
               uiOutput("pic")
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
      ORDER BY (!BOUND(?orderBy)) desc(?orderBy) # ttso: changed to desc
      LIMIT 5 OFFSET 0 # ttso: changed to 5
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
        select(id.value, content.value)
      
      df_cleaned <- res_df %>% 
        group_by(id.value, .drop = FALSE) %>% 
        fill(content.value, .direction = "downup") %>% 
        ungroup() %>% 
        rename(id = id.value,
               puhe = content.value) %>% 
        mutate(id = sub("http://ldf.fi/semparl/speeches/", "https://parlamenttisampo.fi/speeches/page/", id),
               id = paste0('<a href="',id,'" target="_blank">', id, '</a>'))
      
      df_distinct <- distinct(df_cleaned, id, .keep_all = TRUE)
    })
  
  output$table <- renderDT(
    datatable(result(), 
              escape = c(TRUE, FALSE),
              selection = "single")
  )
  
  prompt <- eventReactive(
    input$table_rows_selected, {
      r <- input$table_rows_selected
      if (length(r)) { t <- toString(result()[r, "puhe"]) }
      t_clean <- gsub("^[^!]*! ", "", t)
      t_clean2 <- gsub("\\[.*?\\]", "", t_clean)
      t1000 <- substr(t_clean2, 1, 1000)
    })
  
  observeEvent(input$table_rows_selected, {
    output$dopic <- renderUI({
      actionButton("dopic", label = "Tee kuva", icon = icon("image"))
    })
  })
  
  picresult <- eventReactive(
    input$dopic, {
      res <- create_image(
        prompt = prompt(),
        size = "512x512"
      )
      tags$img(src = res$data$url) 
    }
  )
  
  output$pic <- renderUI({
    picresult()
  })
  
}


shinyApp(ui = ui, server = server)

