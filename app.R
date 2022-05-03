# TCORS Recruitment (internal) dashboard

library(shiny)
library(shinyWidgets)
library(dplyr)
library(DT)
library(ggplot2)
library(plotly)
library(DiagrammeR)
library(glue)


source("app_functions.R")


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
  titlePanel("UVM TCORS Dashboard"),
  
  navbarPage(
    title = "Navigation",
    fluid = TRUE,
    tabPanel(
      "Recruitment",
      sidebarLayout(
        sidebarPanel(
          width = 4,
          # checkboxGroupInput(
          #   "checkProject",
          #   label = "Project Eligible",
          #   choices = c(
          #     "Project 1" = "Project 1",
          #     "Project 2" = "Project 2",
          #     "Project 3" = "Project 3",
          #     "Projects 1 and 3" = "Project 1 and Project 3",
          #     "Project 4" = "Project 4",
          #     "ineligible" = "ineligible"
          #   ),
          #   selected = c(
          #     "Project 1",
          #     "Project 2",
          #     "Project 3",
          #     "Project 4",
          #     "Project 1 and Project 3",
          #     "ineligible"
          #   )
          # ),
          # materialSwitch("switchScreened",
          #                label = "Show Only In-Person Screenings",
          #                value = FALSE),
          pickerInput(
            "pickSource",
            label = "Recruitment Source",
            choices = rct_source,
            selected = "BuildClinical",
            options = pickerOptions(actionsBox = TRUE),
            multiple = FALSE
          ),
          sliderInput(
            "selectDates",
            label = "Date Range",
            min = as.Date("2019-08-01"),
            max = Sys.Date(),
            value = c(as.Date("2022-01-01"), Sys.Date())
          )
        ),
        mainPanel(
          tabsetPanel(
            type = "tabs",
            tabPanel(
              "Inclusion",
              br(),
              h3("Effectiveness"),
              h5("Recruitment Source"),
              textOutput("primary_source"),
              br(),
              grVizOutput('prescreen_flow1', height = "100%"),
              br(),
              plotOutput("ps_plot_1"),
              h3("Comparison"),
              pickerInput(
                "pickSource2",
                label = "Recruitment Source",
                choices = rct_source,
                selected = "direct_mail",
                options = pickerOptions(actionsBox = TRUE),
                multiple = FALSE
              ),
              grVizOutput('prescreen_flow2', height = "100%"),
              br(),
              plotOutput("ps_plot_2")
            ),
            tabPanel(
              "Exclusion",
              br(),
              br(),
              textOutput("total_excluded_n"),
              br(),
              textOutput("excl_gen_n"),
              br(),
              plotOutput("excl_gen"),
              br(),
              br(),
              textOutput("excl_spec_n"),
              br(),
              tabsetPanel(
                type = "pills",
                tabPanel(
                  "Project 1",
                  plotOutput("excl_p1")
                ),
                tabPanel(
                  "Project 2",
                  plotOutput("excl_p2")
                ),
                tabPanel(
                  "Project 3",
                  plotOutput("excl_p3")
                ),
                tabPanel(
                  "Project 4",
                  plotOutput("excl_p4")
                )
              )
            ),
            tabPanel(
              "Locations",
              br(),
              br(),
              plotlyOutput("location_map"),
              br(),
              br(),
              dataTableOutput("location_table")
            ),
            tabPanel(
              "Location Detail",
              br(),
              br(),
              plotOutput("zip_facet"),
              br(),
              hr(),
              plotOutput("prescreen_overall"),
              br(),
              br()
            )
          )
        )
      )
    ),
    tabPanel(
      "Session Windows",
      sidebarLayout(
        sidebarPanel(
          width = 2,
          checkboxGroupButtons(
            "session_window_proj",
            label = "Project",
            choices = c("Project 1", "Project 2", "Project 3"),
            direction = "vertical",
            selected = c("Project 1", "Project 2", "Project 3"),
            justified = TRUE
          )
        ),
        mainPanel(
          plotOutput("session_window_distribution")
        )
      )
    )
  )
)

  
  ## Footer


# Define server logic
server <- function(input, output) {
  sources <- reactive(input$pickSource)
  sources_compare <- reactive(input$pickSource2)
  date_range <- reactive(input$selectDates)
  
  df_filtered <- reactive(
    filter_source(
      ps_location,
      date_range(),
      sources()
    )
  )
  
  df_filtered_compare <- reactive(
    filter_source(
      ps_location,
      date_range(),
      sources_compare()
    )
  )
  
  df_excl_vector <- reactive(
    parse_eligibility(
    df_filtered(),
    excl_vector
  ))
  
  # Inclusion ----
  # output$prescreen_result <- renderTable(
  #   prescreen_result(df_filtered())
  # )
  output$prescreen_flow1 <- renderGrViz(
    prescreen_flow(df_filtered())
  )
  
  output$prescreen_flow2 <- renderGrViz(
    prescreen_flow(
      df_filtered_compare()
    )
  )

  output$primary_source <- renderText(
    sources()
  )
  output$ps_plot_1 <- renderPlot({
    df_filtered() %>%
      plot_variables(date_range())
  })
  output$ps_plot_2 <- renderPlot({
    df_filtered_compare() %>%
      plot_variables(date_range())
  })
  
  
  # Exclusion ----
  
  output$total_excluded_n <- renderText(
    paste(
    "Total Ineligible =",
    df_excl_vector()[[6]]
    )
  )
  output$excl_gen_n <- renderText(
    paste(
      "Met Exclusion Criteria =",
      df_excl_vector()[[10]]
    )
  )
  output$excl_gen <- renderPlot({
    plot_ps_ineligible(
      df_excl_vector()[[1]],
      "General",
      df_excl_vector()[[8]]
    )
  })
  output$excl_spec_n <- renderText(
    paste(
      "Did not meet inclusion criteria =",
      df_excl_vector()[[7]]
    )
  )
  output$excl_p1 <- renderPlot({
    plot_ps_ineligible(
      df_excl_vector()[[2]],
      "Project 1",
      df_excl_vector()[[9]]
    )
  })
  output$excl_p2 <- renderPlot({
    plot_ps_ineligible(
      df_excl_vector()[[3]],
      "Project 2",
      df_excl_vector()[[9]]
    )
  })
  output$excl_p3 <- renderPlot({
    plot_ps_ineligible(
      df_excl_vector()[[4]],
      "Project 3",
      df_excl_vector()[[9]]
    )
  })
  output$excl_p4 <- renderPlot({
    plot_ps_ineligible(
      df_excl_vector()[[5]],
      "Project 4",
      df_excl_vector()[[9]]
    )
  })
  
  # Locations -------
  output$location_map <- renderPlotly({
    produce_map(df_filtered())
  })

  output$location_table <- renderDataTable({
    produce_map_table(df_filtered())
  })
  
  # Location detail ----
  output$zip_facet <- renderPlot({
    location_facet(
      df_filtered(),
      date_range(),
      source = sources()
    )
  })
  output$prescreen_overall <- renderPlot({
    prescreen_overall(
      df_filtered(),
      date_range(),
      sources()
    )
  })
  
  # Session Windows ----
  session_window_project <- reactive(input$session_window_proj)
  output$session_window_distribution <- renderPlot({
    session_window_distribution(
      ps_location,
      ste = "uvm",
      pjt = session_window_project()
    )
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
