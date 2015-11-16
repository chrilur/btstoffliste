##Inspirasjon: Dean Attali - http://deanattali.com/2015/06/14/mimicking-google-form-shiny/

##Todo:
##Lage edit-funksjon
##Sjekke at ID er numerisk

library(shiny)

alleFelt <- c("sak", "id", "journ", "newsvalue", "avd", "digidate", "digitime", "paperdate", "status")
svarDir <- file.path("saker")
epochTime <- function() {
  as.integer(Sys.time())
}

humanTime <- function() format(Sys.time(), "%d%m%Y-%H%M%OS")

#Obligatoriske felt som må fylles ut før skjemaet kan lagres
oblFelt <- c("sak", "journ", "status")

# Sett en asterisk ved en input label
labelObl <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

appCSS <- ".mandatory_star { color: red; }"

loadData <- function() {
  files <- list.files(file.path(svarDir), full.names = TRUE)
  data <- lapply(files, read.csv, stringsAsFactors = FALSE)
  idag <- as.integer(Sys.Date())
  lgth <- length(data)
  
    for (i in 1:lgth){
      #Fiks dato
      data[[i]][,6] <- as.Date(data[[i]][,6], origin="1970-01-01")
      data[[i]][,8] <- as.Date(data[[i]][,8], origin="1970-01-01")
      #Fiks klokkeslett
      data[[i]][,7] <- ifelse(data[[i]][7] < 10, paste0("0", data[[i]][7]), data[[i]][7])
      data[[i]][,7] <- paste0(data[[i]][,7], ":00")
    }
  data <- dplyr::rbind_all(data)
  data <- subset(data, data[,6] > (idag-6))
  names(data) <- c("Sak", "ID", "Journalist", "Verdi", "Avd.", "Pub. nett", "Tidspkt.", "Pub. avis", "Status", "Tidsmerke")
  data
}

shinyApp(
  ui = fluidPage(
    shinyjs:::useShinyjs(),
    shinyjs::inlineCSS(appCSS),
    titlePanel("BTs stoffliste"),
    DT::dataTableOutput("responsesTable"),
    downloadButton("downloadBtn", "Last ned alle"),
    
    fluidRow(br(),
          div(
              id = "form",
      
            column(3,
                    textInput("sak", labelObl("Sak"), ""),
                    numericInput("id", label="ID", value=NULL),
                    textInput("journ", labelObl("Journalist"), ""),
                    actionButton("submit", "Lagre", class = "btn-primary")),
           
            column(4, offset=1,
                   selectInput("status", "Status",
                               c("",  "I arbeid", "Til red.", "Klar", "Publisert")),
                    selectInput("avd", "Avdeling",
                        c("",  "Nyhet", "Samfunn", "Nett", "Kultur", "Feature", "Sport", "Kommentar", "Debatt")),
                   sliderInput("newsvalue", "Nyhetsverdi", 0, 5, 2, ticks = TRUE)), 
                   
            column(4, 
                    dateInput("digidate", label = "Publiseringsdato nett", 
                               value = Sys.Date(), format = "dd-mm-yyyy", language="no"), 
                    sliderInput("digitime", "Publiseres klokken", 0, 24, 12, ticks = TRUE),
                    dateInput("paperdate", label = "Publiseringsdato avis", value = Sys.Date() + 1, format = "dd-mm-yyyy") 
                    ))),
    
      shinyjs::hidden(
        span(id = "submit_msg", "Oppdaterer..."),
        div(id = "error",
            div(br(), tags$b("#BTfail: "), span(id = "error_msg"))
        ))
    ),
    
      shinyjs::hidden(
        div(
          id = "thankyou_msg",
          h3("Sak registrert"),
          actionLink("submit_another", "Legg inn ny sak")
          )
        ),
  
  server = function(input, output, session) {
    
    observe({
      # sjekk om alle obligatoriske felt har en verdi
      mandatoryFilled <-
        vapply(oblFelt,
               function(x) {
                 !is.null(input[[x]]) && input[[x]] != ""
               },
               logical(1))
      mandatoryFilled <- all(mandatoryFilled)
      
      # enable/disable the submit button
      shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
    })
    
    #Samle alle inputdata - med tidsmerke
    formData <- reactive({
      data <- sapply(alleFelt, function(x) input[[x]])
      data <- c(data, timestamp = epochTime())
      data <- t(data)
      data
    })
    
    #Lagre data
    saveData <- function(data) {
      fileName <- sprintf("%s_%s.csv",
                          humanTime(),
                          digest::digest(data))
      
      write.csv(x = data, file = file.path(svarDir, fileName),
                row.names = FALSE, quote = TRUE)
    }
    
    # Handling når Lagre-knappen trykkes
    observeEvent(input$submit, {
      saveData(formData())
      shinyjs::reset("form")
      shinyjs::hide("form")
      shinyjs::show("thankyou_msg")
      
    })
    
    observeEvent(input$submit_another, {
      shinyjs::show("form")
      shinyjs::hide("thankyou_msg")
    })    
    output$responsesTable <- DT::renderDataTable(
      loadData(),
      rownames = FALSE,
      options = list(searching = FALSE, lengthChange = FALSE)
    )
    
    #Last ned alle data i en data frame
    output$downloadBtn <- downloadHandler(
      filename = function() { 
        sprintf("BT_stoffliste_%s.csv", humanTime())
      },
      content = function(file) {
        write.csv(loadData(), file, row.names = FALSE)
      }
    )
  }
)