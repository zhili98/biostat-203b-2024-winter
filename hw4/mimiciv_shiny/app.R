library(shiny)
library(tidyverse)
library(readr)

datasetInput <- read_rds("mimiciv_shiny/mimic_icu_cohort.rds")

transfers_tble <- tbl(con_bq, "transfers") |>
  collect()

ui <- fluidPage(
  
  tabsetPanel(
    
    # Pane 1: Patient Characteristics
    tabPanel(
      # Pane 1 title
      title = "Patient Characteristics",
      # Pane 1 sidebar (input)
      sidebarPanel(
        selectInput(inputId = "var",
                    label = "Variable of interest:",
                    choices = c("Summary", "View"),
                    selected = "Summary"),
      ),
      # Pane 1 main panel (output)
      mainPanel(
        plotOutput("dist_plot")
      )
    ),
    
    # Pane 2: Patient's ADT and ICU stay information
    tabPanel(
      # Pane 2 title
      title = "Patient's ADT and ICU stay information",
      # Pane 2 sidebar (input)
      sidebarPanel(
        numericInput(inputId = "sid",
                     label = "Patient ID:",
                     value = 10012055)
      ),
      # Pane 2 main panel (output)
      mainPanel(
        plotOutput("adt_plot")
      )
    )
  )
  
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
  
  output$caption <- renderText({
    input$caption
  })
  
  output$summary <- renderPrint({
    dataset <- datasetInput()
    summary(dataset)
  })
  
  output$view <- renderTable({
    head(datasetInput(), n = input$obs)
  })
  
}

shinyApp(ui, server)


if(FALSE) {
  if(input$var == "Lab Events") {
      datasetInput |> ggplot() +
      geom_boxplot()
  } else if(input$var == "Vitals") { 
    geom_boxplot()
  } else if(is.numeric(datasetInput[1,input$var])) {
          
            geom_boxplot()
  } else {
    geom_bar()
  }
}



