library(shiny)
library(bigrquery)
library(dbplyr)
library(DBI)
library(gt)
library(gtsummary)
library(tidyverse)
library(readr)
library(stringr)
library(pryr)
library(R.utils)
library(GGally)

################################################################################
# Version 2: Fast plotting, Slow 'Run App'

# Difference: This version put the bigquery steps outside of the server function.

# PROS: 
# 1. Fast plotting for ADT plot in Panel 2 since tibbles are all
# collected in advance. No more waiting when switching between patients.

# CONS:
# 1. Takes up a lot of memory and time to run the app.

################################################################################

# Loading in data

# Since default working directory is where the .Rproj file is located, (203b-hw)
# relative path should do the job in this case.
mimic_icu_cohort <- read_rds("mimic_icu_cohort.rds")

# Path to the service account token 
satoken <- "biostat-203b-2024-winter-313290ce47a6.json"

# BigQuery authentication using service account
bq_auth(path = satoken)

# Connect to the BigQuery database `biostat-203b-2024-winter.mimic4_v2_2`
con_bq <- dbConnect(
  bigrquery::bigquery(),
  project = "biostat-203b-2024-winter",
  dataset = "mimic4_v2_2",
  billing = "biostat-203b-2024-winter"
)
con_bq

# Retrieve the data sets to avoid unnecessary query time
transfers_tble <- tbl(con_bq, "transfers") |>
  collect()

diagnoses_icd_tble <- tbl(con_bq, "diagnoses_icd") |>
  collect()

d_icd_diagnoses_tble <- tbl(con_bq, "d_icd_diagnoses") |>
  collect()

procedures_icd_tble <- tbl(con_bq, "procedures_icd") |>
  collect()

d_icd_procedures_tble <- tbl(con_bq, "d_icd_procedures") |>
  collect()

mimic_sid <- mimic_icu_cohort$subject_id

# Filter the labevents_tble to a size that will not exceed the limit of bigquery
labevents_tble <- tbl(con_bq, "labevents") |> # Over 100 million rows
  filter(subject_id %in% mimic_sid) |> # About 60 million rows
  select(subject_id, charttime) |> # About 60 million rows times 2 columns
  group_by(subject_id) |>
  distinct(charttime) |> # Shrinking data size by collapsing duplicates
  ungroup() |> # About 3 million rows times 2 columns
  collect() # Collect only when not exceeding the query limit

labevents_sid <- labevents_tble |>
  distinct(subject_id) |>
  select(subject_id) |>
  arrange()

#-------------------------------------------------------------------------------
# Shiny app
# Define ui
ui <- fluidPage(
  
  tabsetPanel(
    
    # Panel 1: Patient Characteristics
    tabPanel(
      # Panel 1 title
      title = "Patient Characteristics",
      # Panel 1 sidebar (input)
      sidebarPanel(
        helpText("Select a patient."),
        selectInput(inputId = "var",
                    label = "Variable of interest",
                    # Only transform names for categorical variables
                    choices = c("Lab Events",
                                "Vitals",
                                "First care unit" = "first_careunit",
                                "Last care unit" = "last_careunit", 
                                "Length of stay", 
                                "Gender" = "gender",
                                "Age at intime",
                                "Admission type" = "admission_type",
                                "Admission location" = "admission_location",
                                "Discharge location" = "discharge_location",
                                "Insurance" = "insurance", 
                                "Language" = "language",
                                "Marital status" = "marital_status",
                                "Race" = "race", 
                                "Length of stay long or not" = "los_long"),
                    selected = "Lab Events"),
        checkboxInput(inputId = "check",
                      label = "Remove outliers in IQR method for measurements?",
                      value = TRUE)
      ),
      # Panel 1 main panel (output)
      mainPanel(
        tabsetPanel(
          tabPanel("Graphical Summary", plotOutput(outputId = "dist_plot")),
          tabPanel("Numerical Summary", 
                   verbatimTextOutput(outputId = "num_summary"))
        )
      )
    ),
    
    # Panel 2: Patient's ADT and ICU stay information
    tabPanel(
      # Panel 2 title
      title = "Patient's ADT and ICU stay information",
      # Panel 2 sidebar (input)
      sidebarPanel(
        selectInput(inputId = "sid",
                    label = "Patient ID:",
                    choices = labevents_sid,
                    selected = 10012055)
      ),
      # Panel 2 main panel (output)
      mainPanel(
        plotOutput(outputId = "adt_plot")
      )
    )
    
  )
  
)

# Define server
server <- function(input, output) {

  # Plot for Panel 1
  output$dist_plot <- renderPlot({
    if(input$var == "Lab Events" & input$check) { # drop outliers
      mimic_icu_cohort |> 
        select(sodium, potassium, glucose, creatinine, chloride, bicarbonate) |> 
        pivot_longer(everything(),
                     names_to = "variable",
                     values_to = "value") |>
        drop_na() |>
        group_by(variable) |>
        filter(value > quantile(value, 0.25) - 1.5*IQR(value) & 
               value < quantile(value, 0.75) + 1.5*IQR(value)) |>
        ungroup() |>
          ggplot() +
          geom_boxplot(aes(x = variable, y = value)) +
          labs(xlab = "variable", ylab = "value") +
          theme_classic() +
          coord_flip()
    } else if(input$var == "Lab Events" & !input$check){ # do not drop outliers
        mimic_icu_cohort |> 
          select(sodium, potassium, glucose, creatinine,
                 chloride, bicarbonate) |> 
          pivot_longer(everything(),
                       names_to = "variable",
                       values_to = "value") |>
          ggplot() +
          geom_boxplot(aes(x = variable, y = value)) +
          labs(xlab = "variable", ylab = "value") +
          theme_classic() +
          coord_flip()
    }
      else if(input$var == "Vitals" & input$check) { # drop outliers
        mimic_icu_cohort |> 
          select(heart_rate, non_invasive_blood_pressure_diastolic,
                 non_invasive_blood_pressure_systolic, respiratory_rate,
                 temperature_fahrenheit) |> 
          pivot_longer(everything(),
                       names_to = "variable",
                       values_to = "value") |>
          drop_na() |>
          group_by(variable) |>
          filter(value > quantile(value, 0.25) - 1.5*IQR(value) & 
               value < quantile(value, 0.75) + 1.5*IQR(value)) |>
          ungroup() |>
          ggplot() +
          geom_boxplot(aes(x = variable, y = value)) +
          labs(xlab = "variable", ylab = "value") +
          theme_classic() +
          coord_flip()
    } else if(input$var == "Vitals" & !input$check) { # do not drop outliers
        mimic_icu_cohort |> 
          select(heart_rate, non_invasive_blood_pressure_diastolic,
                 non_invasive_blood_pressure_systolic, respiratory_rate,
                 temperature_fahrenheit) |> 
          pivot_longer(everything(),
                       names_to = "variable",
                       values_to = "value") |>
          ggplot() +
          geom_boxplot(aes(x = variable, y = value)) +
          labs(xlab = "variable", ylab = "value") +
          theme_classic() +
          coord_flip()
    } else if(input$var %in% c("Length of stay", "Age at intime")) {
        # Numeric variables
        if(input$var == "Length of stay"){
          value <- mimic_icu_cohort$los
        } else {
          value <- mimic_icu_cohort$age_intime
        }
        # Check if the user wants to remove outliers
        if(input$check) {
          value |> 
            as_tibble() |>
            filter(value > quantile(value, 0.25) - 1.5*IQR(value) & 
                   value < quantile(value, 0.75) + 1.5*IQR(value)) |>
            ggplot() +
            geom_boxplot(aes(x = input$var, y = value)) +
            labs(xlab = "variable", ylab = "value") +
            theme_classic() +
            coord_flip()
        } else {
          value |> 
            as_tibble() |>
            ggplot() +
            geom_boxplot(aes(x = input$var, y = value)) +
            labs(xlab = "variable", ylab = "value") +
            theme_classic() +
            coord_flip()
        }
    } else {
      # Categorical variables
      value <- mimic_icu_cohort[[input$var]]
      value |> 
        as_tibble() |>
        ggplot() +
        geom_bar(aes(x = value)) +
        labs(xlab = "variable",
             ylab = "value",
             title = "Patient count by stay or patient variable group") +
        theme_classic() +
        coord_flip()
    }
  })
  
  # Table for Panel 1
  
  output$num_summary <- renderPrint({
    if(input$var %in% c("Lab Events", "Vitals")) {
      
      if(input$var == "Lab Events") {
        # Lab + remove
        if(input$check){
          mimic_icu_cohort |> 
            select(sodium, potassium, glucose, creatinine, chloride, bicarbonate) |>
            drop_na() |>
            # Remove outliers
            filter(sodium > quantile(sodium, 0.25) - 1.5*IQR(sodium) & 
                     sodium < quantile(sodium, 0.75) + 1.5*IQR(sodium) &
                     potassium > quantile(potassium, 0.25) - 1.5*IQR(potassium) & 
                     potassium < quantile(potassium, 0.75) + 1.5*IQR(potassium) &
                     glucose > quantile(glucose, 0.25) - 1.5*IQR(glucose) & 
                     glucose < quantile(glucose, 0.75) + 1.5*IQR(glucose) &
                     creatinine > quantile(creatinine, 0.25) - 1.5*IQR(creatinine) & 
                     creatinine < quantile(creatinine, 0.75) + 1.5*IQR(creatinine) &
                     chloride > quantile(chloride, 0.25) - 1.5*IQR(chloride) & 
                     chloride < quantile(chloride, 0.75) + 1.5*IQR(chloride) &
                     bicarbonate > 
                     quantile(bicarbonate, 0.25) - 1.5*IQR(bicarbonate) & 
                     bicarbonate < 
                     quantile(bicarbonate, 0.75) + 1.5*IQR(bicarbonate)) |>
            summary()
          # Lab + keep
        } else {
          mimic_icu_cohort |> 
            select(sodium, potassium, glucose, creatinine, chloride, bicarbonate) |> 
            summary()
        }
        # Vitals + remove
      } else {
        if(input$check){
          mimic_icu_cohort |> 
            select(heart_rate, non_invasive_blood_pressure_diastolic,
                   non_invasive_blood_pressure_systolic, respiratory_rate,
                   temperature_fahrenheit) |>
            drop_na() |>
            # Remove outliers
            filter(heart_rate > quantile(heart_rate, 0.25) - 1.5*IQR(heart_rate) & 
                     heart_rate < quantile(heart_rate, 0.75) + 1.5*IQR(heart_rate) &
                     non_invasive_blood_pressure_diastolic > 
                     quantile(non_invasive_blood_pressure_diastolic, 0.25) - 
                     1.5*IQR(non_invasive_blood_pressure_diastolic) & 
                     non_invasive_blood_pressure_diastolic < 
                     quantile(non_invasive_blood_pressure_diastolic, 0.75) + 
                     1.5*IQR(non_invasive_blood_pressure_diastolic) &
                     non_invasive_blood_pressure_systolic > 
                     quantile(non_invasive_blood_pressure_systolic, 0.25) - 
                     1.5*IQR(non_invasive_blood_pressure_systolic) & 
                     non_invasive_blood_pressure_systolic < 
                     quantile(non_invasive_blood_pressure_systolic, 0.75) + 
                     1.5*IQR(non_invasive_blood_pressure_systolic) &
                     respiratory_rate > quantile(respiratory_rate, 0.25) - 
                     1.5*IQR(respiratory_rate) & 
                     respiratory_rate < quantile(respiratory_rate, 0.75) + 
                     1.5*IQR(respiratory_rate) &
                     temperature_fahrenheit > 
                     quantile(temperature_fahrenheit, 0.25) - 
                     1.5*IQR(temperature_fahrenheit) & 
                     temperature_fahrenheit < 
                     quantile(temperature_fahrenheit, 0.75) + 
                     1.5*IQR(temperature_fahrenheit)) |>
            summary()
          # Vitals + keep
        } else {
          mimic_icu_cohort |> 
            select(heart_rate, non_invasive_blood_pressure_diastolic,
                   non_invasive_blood_pressure_systolic, respiratory_rate,
                   temperature_fahrenheit) |>
            summary()
        }
      }
      
    } else if(input$var %in% c("Length of stay", "Age at intime")) {
      # Los
      if(input$var == "Length of stay"){
        value <- mimic_icu_cohort$los
        # los + remove
        if(input$check){
          value |> 
            as_tibble() |>
            drop_na() |>
            filter(value > quantile(value, 0.25) - 1.5*IQR(value) & 
                     value < quantile(value, 0.75) + 1.5*IQR(value)) |>
            summary()
          # los + keep
        } else {
          value |> 
            as_tibble() |>
            summary()
        }
        # Age
      } else {
        value <- mimic_icu_cohort$age_intime
        # age + remove
        if(input$check){
          value |> 
            as_tibble() |>
            drop_na() |>
            filter(value > quantile(value, 0.25) - 1.5*IQR(value) & 
                     value < quantile(value, 0.75) + 1.5*IQR(value)) |>
            summary()
          # age + keep
        } else {
          value |> 
            as_tibble() |>
            summary()
        }
      }
      # Categorical 
    } else {
      value <- mimic_icu_cohort[[input$var]]
      n_tbl <- value |> 
        table() |>
        as_tibble()
      
      prop_tbl <- value |>
        table() |>
        prop.table() |>
        as_tibble() |>
        mutate(prop = round(n, 2)) |>
        select(-n)
      
      joined_tbl <- n_tbl |> left_join(prop_tbl, by = "value")
      
      return(joined_tbl)
    }
  })
  
  # Pre-process the data for Panel 2
  
  sid_adt <- reactive({ 
    transfers_tble |> 
      filter(subject_id == input$sid) |> 
      select(intime, outtime, careunit, eventtype)
  })
  
  age <- reactive({
    mimic_icu_cohort |> 
      filter(subject_id == input$sid) |>
      pull(anchor_age)
  })
  
  sex <- reactive({
    mimic_icu_cohort |> 
      filter(subject_id == input$sid) |>
      pull(gender)
  })
  
  race <- reactive({
    mimic_icu_cohort |> 
      filter(subject_id == input$sid) |>
      pull(race)
  })
  
  top3_diag <- reactive({
    diagnoses_icd_tble |> 
      filter(subject_id == input$sid) |> 
      count(icd_code) |> 
      left_join(d_icd_diagnoses_tble, by = "icd_code") |> 
      arrange(desc(n)) |> 
      pull(long_title) |> 
      head(3)
  })
  
  # Can't use input$sid (shiny reactive object) for filtering a bigquery table 
  # by translating dplyr function into SQL query.
  
  # If I run: 
  
  # sid <- reactive({input$sid})
  # 'filter(subject_id == sid)'
  
  # then an error will popout in R console:
  
  # Warning: Error in :
  # Cannot translate a shiny reactive to SQL.
  
  # Moreover, I tried using '!!' to tell 'dbplyr' to evaluate the shiny reactive
  # expression locally, but it didn't work:
  
  # 'filter(subject_id == !!sid())'
  
  # Then I got:
  
  # Warning: Error in bq_dataset_query: 
  # Job biostat-203b-2024-winter.job_eCmqmkmatoPU_gwZZkShJus_WRw6.us-west2 
  # failed
  # ✖ No matching signature for operator = for argument types: INT64, STRING. 
  # Supported signature: ANY = ANY at [3:8] [invalidQuery]
  
  # Possible solution: version 1 (app.R)
  # Use as.numeric within a reactive({}) to convert the shiny reactive object
  # into a numeric value and save it to a local static variable (i.e. evaluate
  # the shiny reactive object locally).

  sid_lab <- reactive({
    labevents_tble |>
      filter(subject_id == input$sid) |>
      select(charttime) |>
      collect()
  })
  
  sid_procedure <- reactive({
    procedures_icd_tble |>
      filter(subject_id == input$sid) |>
      select(chartdate, icd_code) |>
      left_join(d_icd_procedures_tble, by = "icd_code") |>
      select(chartdate, long_title) |>
      mutate(chartdate = as.POSIXct(chartdate))
  })
  
  
  # Plot for Panel 2
  output$adt_plot <- renderPlot({
    ggplot() +
      # ADT
      geom_segment(data = sid_adt() |>
                     filter(eventtype != "discharge"),
                   aes(x = intime,
                       xend = outtime,
                       y = "ADT",
                       yend = "ADT",
                       color = careunit,
                       linewidth = str_detect(careunit, "(ICU|CCU)"))) +
      # Lab
      geom_point(data = sid_lab(),
                 aes(x = charttime,
                     y = "Lab"),
                 shape = 3,
                 size = 3) +
      # Procedure
      geom_point(data = sid_procedure(),
                 aes(x = chartdate,
                     y = "Procedure",
                     shape = long_title,
                     size = 3)) +
      scale_shape_manual(values = c(1:nrow(sid_procedure()))) +
      guides(linewidth = "none",
             size = "none",
             color = guide_legend(title = "Care Unit"),
             shape = guide_legend(title = "Procedure",
                                  nrow = ceiling(nrow(sid_procedure()) / 2))) +
      labs(
        x = "Calendar Time",
        y = "",
        title = str_c("Patient ", input$sid, ", ", sex(), ", ", age(), 
                      " years old, ", race()),
        subtitle = str_c(top3_diag()[1],"\n", top3_diag()[2],"\n", 
                         top3_diag()[3])
      ) +
      theme_light() +
      theme(legend.position = "bottom",
            legend.box = "vertical") +
      scale_y_discrete(limits = c("Procedure", "Lab", "ADT"))
  })
}

shinyApp(ui = ui, server = server)


