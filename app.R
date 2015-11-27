library(shiny)
library(shinyjs)

#Big thanks to Christoph Glur and his blog post:
#http://ipub.com/shiny-crud-app/

GetTableMetadata <- function() {
  fields <- c(id = "Id", 
              sak = "Sak", 
              journ = "Journalist",
              esc = "Esc.ID",
              avd = "Avdeling",
              digidate = "Pub.dato nett",
              digitime = "Pub.tidspkt. nett",
              avisdate = "Pub.dato avis",
              newsvalue = "Nyhetsverdi",
              status = "Status",
              komm = "Kommentar")
  result <- list(fields = fields)
  return (result)
}

# Find the next ID of a new record
# (in mysql, this could be done by an incremental index)
GetNextId <- function() {
  if (exists("responses") && nrow(responses) > 0) {
    max(as.integer(rownames(responses))) + 1
  } else {
    return (1)
  }
}

saveData <- function(){
  tid <- format(Sys.time(), "%d%m%Y-%H%M%OS")
  write.table(responses, paste0("data/",tid, "_", "responses.csv"), sep=",", row.names=FALSE)
}

timeFix <- function(){
  lgth <- length(data)
  
  for (i in 1:lgth){
    #Fiks dato
    data[[i]][,5] <- as.Date(data[[i]][,5], origin="1970-01-01")
    data[[i]][,7] <- as.Date(data[[i]][,7], origin="1970-01-01")
    #Fiks klokkeslett
    data[[i]][,6] <- ifelse(data[[i]][6] < 10, paste0("0", data[[i]][7]), data[[i]][7])
    data[[i]][,6] <- paste0(data[[i]][,6], ":00")       
  } 
}

#C
CreateData <- function(data) {
  
  data <- CastData(data)
  rownames(data) <- GetNextId()
  if (exists("responses")) {
    responses <<- rbind(responses, data)
  } else {
    responses <<- data
  }
  saveData()
}

#R
ReadData <- function() {
  if (exists("responses")) {
    responses
  }
  #timeFix()
}



#U
UpdateData <- function(data) {
  data <- CastData(data)
  responses[row.names(responses) == row.names(data), ] <<- data
  saveData()
}

#D
DeleteData <- function(data) {
  responses <<- responses[row.names(responses) != unname(data["id"]), ]
  saveData()
}

# Cast from Inputs to a one-row data.frame
CastData <- function(data) {
  datar <- data.frame(sak = data["sak"], 
                      journ = data["journ"],
                      esc = data["esc"],
                      avd = data["avd"],
                      digidate = data["digidate"],
                      digitime = data["digitime"],
                      avisdate = data["avisdate"],
                      newsvalue = data["newsvalue"],
                      status = data["status"],
                      komm = data["komm"],
                      stringsAsFactors = FALSE)
  
  rownames(datar) <- data["id"]
  return (datar)
}




# Return an empty, new record
CreateDefaultRecord <- function() {
  mydefault <- CastData(list(id = "0", sak = "", journ = "", esc = "", avd = "", digidate = "", 
                             digitime = "", avisdate = "", newsvalue = "", status = "", 
                             komm = ""))
  return (mydefault)
}

# Fill the input fields with the values of the selected record in the table
UpdateInputs <- function(data, session) {
  updateTextInput(session, "id", value = unname(rownames(data)))
  updateTextInput(session, "sak", value = unname(data["sak"]))
  updateTextInput(session, "journ", value = unname(data["journ"]))
  updateNumericInput(session, "esc", value = unname(data["esc"]))
  updateSelectInput(session, "avd")
  updateDateInput(session, "digidate")
  updateSliderInput(session, "digitime", value = unname(data["digitime"]))
  updateDateInput(session, "avisdate")
  updateSliderInput(session, "newsvalue", value = unname(data["newsvalue"]))
  updateSelectInput(session, "status")  
  updateTextInput(session, "komm", value = unname(data["komm"]))
  }


ui <- fluidPage(
  #use shiny js to disable the ID field
  shinyjs::useShinyjs(),
  titlePanel("BTs stoffliste"),
  #data table
  DT::dataTableOutput("responses", width = "100%"), 
  #action buttons
  actionButton("submit", "Lagre", class="btn-primary"),
  actionButton("new", "Ny sak"),
  actionButton("delete", "Slett"),
  #input fields
  tags$hr(),
  fluidRow(
  column(4,
    shinyjs::disabled(textInput("id", "Id", "0")),
    textInput("sak", "Sak", ""),
    textInput("journ", "Journalist", ""),
    numericInput("esc", "Esc.ID", "")),
  column(4,
    selectInput("avd", "Avdeling",
              c("",  "Nyhet", "Samfunn", "Nett", "Kultur", "Feature", "Sport", "Video", "Kommentar", "Debatt")),
    dateInput("digidate", label = "Pub.dato nett", value = Sys.Date(), format = "dd-mm-yyyy", language="no"),
    sliderInput("digitime", "Publiseres klokken", 0, 24, 12, ticks = TRUE),
    dateInput("avisdate", label = "Pub.dato avis", value = Sys.Date() + 1, format = "dd-mm-yyyy")),
  column(4,
    sliderInput("newsvalue", "Nyhetsverdi", 0, 5, 2, ticks = TRUE),
    selectInput("status", "Status",
                  c("Ikke påbegynt",  "I arbeid", "Til red.", "Klar", "Publisert")),  
    textInput("komm", "Kommentar", "")))
)


server <- function(input, output, session) {
  
  # input fields are treated as a group
  formData <- reactive({
    sapply(names(GetTableMetadata()$fields), function(x) input[[x]])
  })
  
  # Click "Submit" button -> save data
  observeEvent(input$submit, {
    if (input$id != "0") {
      UpdateData(formData())
    } else {
      CreateData(formData())
      UpdateInputs(CreateDefaultRecord(), session)
    }
  }, priority = 1)
  
  # Press "New" button -> display empty record
  observeEvent(input$new, {
    UpdateInputs(CreateDefaultRecord(), session)
  })
  
  # Press "Delete" button -> delete from data
  observeEvent(input$delete, {
    DeleteData(formData())
    UpdateInputs(CreateDefaultRecord(), session)
  }, priority = 1)
  
  # Select row in table -> show details in inputs
  observeEvent(input$responses_rows_selected, {
    if (length(input$responses_rows_selected) > 0) {
      data <- ReadData()[input$responses_rows_selected, ]
      UpdateInputs(data, session)
    }
    
  })
  
  # display table
  output$responses <- DT::renderDataTable({
    #update after submit is clicked
    input$submit
    #update after delete is clicked
    input$delete
    ReadData()
    ##Fikse dato her
  }, server = FALSE, selection = "single",
  colnames = unname(GetTableMetadata()$fields)[-1])}


# Shiny app with 3 fields that the user can submit data for
shinyApp(ui = ui, server = server)