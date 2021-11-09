# Display prescreen location and cost effectiveness. Relies on rct_locations.R
# Tony Barrows 2021-09-28

library(shiny)
library(shinyWidgets)
library(dplyr)
library(plotly)
library(DT)
library(ggplot2)
library(ggalluvial)

## plotting parameters

theme_set(theme_classic(base_size = 15))


ps_location <- read.csv("./out/ps_locations.csv")
no_location_data <- read.csv("./out/no_location_data.csv")

mailer_cost <- read.csv("./data/mailer_cost.csv")
overall_cost <- read.csv("./data/overall_cost.csv")

last_update <- read.table("./out/lastupdate.txt")$V1

mailer_breakdown <-
    function(mailer_cost,
             ps_location,
             project_select,
             filter_screen) {
        df <- mailer_cost %>%
            mutate(city = stringr::str_trim(city)) %>%
            left_join(ps_location, by = "zip_id") %>%
            filter(source == "direct_mail") %>%
            filter(ps_proj_eligible %in% project_select) %>%
            group_by(zip_id, location = city, volume)
        
        if (filter_screen) {
            df <- df %>%
                filter(screened == "yes")
        }
        
        scrn <- df %>%
            filter(screened == "yes") %>%
            count(name = "n_screenings") %>%
            mutate(screening_rate = paste(round(n_screenings / volume * 100, 2), "%", sep = "")) %>%
            ungroup() %>%
            select(zip_id, n_screenings, screening_rate)
        
        ps <- df %>%
            count(name = "n_prescreens") %>%
            mutate(prescreen_rate = paste(round(n_prescreens / volume * 100, 2), "%", sep = ""))
        
        merge(ps, scrn, by = "zip_id") %>%
            datatable(rownames = FALSE)
    }

mailer_effectiveness <-
    function(mailer_cost,
             ps_location,
             project_select,
             filter_screen) {
        cost <- mailer_cost %>%
            summarize(across(
                .cols = c(volume, cost),
                .fns = sum,
                na.rm = TRUE
            ))
        
        if (filter_screen) {
            ps_location <- ps_location %>%
                filter(screened == "yes")
        }
        
        ps_location %>%
            filter(source == "direct_mail",
                   ps_proj_eligible %in% project_select) %>%
            group_by(ps_proj_eligible, screened) %>%
            count() %>%
            mutate(volume = cost$volume,
                   "effectiveness (%)" = round(n / volume * 100, 2))
    }


rct_rate <- function(ps_location) {
    ts <- ps_location %>%
        mutate(recruit_date = as.Date(recruit_date)) %>%
        group_by(source, recruit_date) %>%
        count() %>%
        group_by(source) %>%
        mutate(rollavg = zoo::rollmean(n, 7, fill = NA),
               cumsum = cumsum(n))
    
    rate <-
        ggplot(ts, aes(x = recruit_date, y = rollavg, color = source)) +
        geom_line() +
        labs(y = "Weekly Rolling Average",
             title = "Prescreen Rate by Source")
    
    accrual <-
        ggplot(ts, aes(x = recruit_date, y = cumsum, color = source)) +
        geom_line() +
        labs(y = "Cumulative Sum",
             title = "Prescreen Accrual by Source")
    
    list(rate, accrual)
}


rct_flow <-
    function(ps_location,
             ps_proj_list,
             filter_low_n,
             filter_screened,
             source_list,
             date_range_list,
             remove_source) {
        # n_colors <-  nrow(distinct(df %>% ungroup() %>% select(ps_proj_eligible)))
        n_colors <- 5
        colors <- RColorBrewer::brewer.pal(n_colors, "Dark2")
        
        df <- ps_location %>%
            filter(
                ps_proj_eligible %in% ps_proj_list,
                source %in% source_list,
                recruit_date >= date_range_list[[1]],
                recruit_date <= date_range_list[[2]]
            ) %>%
            group_by(
                source,
                ps_proj_eligible,
                screened,
                recruit_int_summ,
                rand_proj,
                randomized,
                complete,
                sl_status
            ) %>%
            count()
        
        if (filter_low_n) {
            df <- df %>%
                group_by(source) %>%
                mutate(source_sum = sum(n)) %>%
                filter(source_sum > 5) %>%
                select(-source_sum)
        }
        if (filter_screened) {
            df <- df %>%
                filter(screened == "screened")
        }
        
        if (remove_source) {
            p <- ggplot(
                df,
                aes(
                    y = n,
                    axis1 = ps_proj_eligible,
                    axis2 = recruit_int_summ,
                    axis3 = rand_proj,
                    axis4 = sl_status
                )
            ) +
                scale_x_discrete(
                    limits = c(
                        "Prescreen Project",
                        "Prescreen Status",
                        "Randomized Project",
                        "Enrollment Status"
                    ),
                    expand = c(0.06, 0.06)
                )
        } else {
            p <- ggplot(
                df,
                aes(
                    y = n,
                    axis1 = source,
                    axis2 = ps_proj_eligible,
                    axis3 = recruit_int_summ,
                    axis4 = rand_proj,
                    axis5 = sl_status
                )
            ) +
                scale_x_discrete(
                    limits = c(
                        "Source",
                        "Prescreen Project",
                        "Prescreen Status",
                        "Randomized Project",
                        "Enrollment Status"
                    ),
                    expand = c(0.06, 0.06)
                )
            
        }
        p + geom_alluvium(aes(fill = ps_proj_eligible)) +
            geom_stratum(width = 1 / 4,
                         color = "black",
                         alpha = .5) +
            geom_label(
                stat = "stratum",
                aes(label = after_stat(stratum)),
                alpha = .75,
                size = 5
            ) +
            theme(legend.position = "top", ) +
            labs(fill = "Prescreen Project") +
            scale_fill_manual(
                values = c(
                    "ineligible" = colors[1],
                    "Project 1" = colors[2],
                    "Project 1 and Project 3" = colors[3],
                    "Project 2" = colors[4],
                    "Project 3" = colors[5]
                )
            )
    }



rct_cost <-
    function(overall_cost,
             ps_location,
             filter_screen,
             source_list) {
        df <- ps_location %>%
            filter(source %in% source_list) %>%
            left_join(overall_cost, by = "source") %>%
            select(source, screened, amount_spent) %>%
            group_by(source, amount_spent) %>%
            mutate(amount_spent = ifelse(is.na(amount_spent), 0, amount_spent))
        
        if (filter_screen) {
            df <- df %>%
                filter(screened == "yes")
        }
        
        
        scrn <- df %>%
            filter(screened == "yes") %>%
            count(name = "n_screenings") %>%
            mutate(cost_per_screening = amount_spent / n_screenings) %>%
            ungroup() %>%
            select(source, n_screenings, cost_per_screening)
        
        ps <- df %>%
            count(name = "n_prescreens") %>%
            mutate(cost_per_prescreen = amount_spent / n_prescreens)
        
        merge(ps, scrn, all = TRUE) %>%
            arrange(-amount_spent) %>%
            datatable(rownames = FALSE) %>%
            formatCurrency(columns = c(
                "amount_spent",
                "cost_per_prescreen",
                "cost_per_screening"
            ))
    }

rct_source <- c(
    "craigslist" = "craigslist",
    "Referral" = "other_person",
    "Facebook" = "facebook",
    "Instagram" = "instagram",
    "TikTok" = "tiktok",
    "ClinicalTrials.gov" = "clin_trials_web",
    "Google" = "google",
    "Front Porch Forum" = "front_porch_forum",
    "Flyer" = "flyer",
    "Direct Mail" = "direct_mail",
    "Neighborhood Newspaper" = "neighborhood_news",
    "Metro Newspaper" = "metro_news",
    "Television" = "TV",
    "Radio" = "radio",
    "Participated in Other Studies" = "pt_in_other_studies",
    "Bus Advertisement" = "bus_ad",
    "BuildClinical" = "BuildClinical",
    "Other" = "other",
    "Multiple Sources" = "multiple"
)


ui <- fluidPage(
    # Application title
    titlePanel("TCORS Study 3", title = div(img(
        src = "vcbh_logo.png", width = 200
    ))),
    titlePanel("TCORS Study 3 Recruitment Summary"),
    h4("UVM Only"),
    textOutput("last_updated"),
    br(),
    
    sidebarLayout(
        sidebarPanel(
            width = 4,
            checkboxGroupInput(
                "checkProject",
                label = "Project Eligible",
                choices = c(
                    "Project 1" = "Project 1",
                    "Project 2" = "Project 2",
                    "Project 3" = "Project 3",
                    "Projects 1 and 3" = "Project 1 and Project 3",
                    "ineligible" = "ineligible"
                ),
                selected = c(
                    "Project 1",
                    "Project 2",
                    "Project 3",
                    "Project 1 and Project 3",
                    "ineligible"
                )
            ),
            materialSwitch("switchScreened",
                           label = "Show Only In-Person Screenings",
                           value = FALSE),
            pickerInput(
                "pickSource",
                label = "Recruitment Source",
                choices = rct_source,
                selected = rct_source,
                options = pickerOptions(actionsBox = TRUE),
                multiple = TRUE
            ),
            sliderInput(
                "selectDates",
                label = "Date Range",
                min = as.Date("2020-10-01"),
                max = Sys.Date(),
                value = c(as.Date("2020-10-01"), Sys.Date())
            )
            
        ),
        
        mainPanel(
            tabsetPanel(
                type = "tabs",
                tabPanel(
                    "Recruitment Flow",
                    br(),
                    plotOutput("rct_flow_plot",
                               height = "1000px"),
                    br(),
                    checkboxInput("filter_low_n",
                                  label = "Filter n < 5",
                                  value = FALSE),
                    checkboxInput("remove_source",
                                  label = "Remove Source",
                                  value = FALSE)
                ),
                tabPanel(
                    "Recruitment Rates",
                    br(),
                    plotlyOutput("rct_rate"),
                    br(),
                    plotlyOutput("rct_accrual")
                ),
                tabPanel(
                    "Map",
                    br(),
                    plotlyOutput("vt_map"),
                    br(),
                    dataTableOutput("counting_table")
                ),
                # tabPanel("Cost Effectiveness",
                #          br(),
                #          dataTableOutput("rct_cost_table")),
                tabPanel(
                    "Direct Mail Breakdown",
                    br(),
                    dataTableOutput("mailer_cost_table"),
                    br(),
                    tableOutput("mail_effectiveness")
                )
                
            ),
            br(),
            br(),
            hr(),
            br(),
            
        )
    ),
    
)

server <- function(input, output) {
    proj <- reactive(input$checkProject)
    sources <- reactive(input$pickSource)
    date_range <- reactive(input$selectDates)
    
    output$last_updated <- renderText({
        paste("Last Updated", as.character(last_update))
    })
    
    output$rct_flow_plot <- renderPlot({
        rct_flow(
            ps_location,
            proj(),
            input$filter_low_n,
            input$switchScreened,
            sources(),
            date_range(),
            input$remove_source
        )
    })
    
    output$rct_rate <- renderPlotly({
        df <- ps_location %>%
            filter(
                ps_proj_eligible %in% proj(),
                source %in% sources(),
                recruit_date >= date_range()[[1]],
                recruit_date <= date_range()[[2]]
            )
        if (input$switchScreened) {
            df <- df %>% filter(screened == "yes")
        }
        
        rct_rate(df)[[1]] %>%
            ggplotly()
    })
    
    output$rct_accrual <- renderPlotly({
        df <- ps_location %>%
            filter(
                ps_proj_eligible %in% proj(),
                source %in% sources(),
                recruit_date >= date_range()[[1]],
                recruit_date <= date_range()[[2]]
            )
        
        if (input$switchScreened) {
            df <- df %>% filter(screened == "yes")
        }
        
        
        rct_rate(df)[[2]] %>%
            ggplotly()
    })
    
    
    output$vt_map <- renderPlotly({
        lat_foc <- 44.475883
        lon_foc <- -72.575386
        g <- list(
            scope = 'usa',
            projection = list(type = 'albers usa',
                              scale = 8),
            showland = TRUE,
            landcolor = toRGB("gray95"),
            subunitcolor = toRGB("gray85"),
            countrycolor = toRGB("gray85"),
            countrywidth = 0.5,
            subunitwidth = 0.5,
            center = list(lon = lon_foc, lat = lat_foc)
        )
        
        if (input$switchScreened) {
            ps_location <- ps_location %>%
                filter(screened == "yes")
        }
        fig <- ps_location %>%
            filter(
                ps_proj_eligible %in% proj(),
                source %in% sources(),
                recruit_date >= date_range()[[1]],
                recruit_date <= date_range()[[2]]
            ) %>%
            plot_geo(lat = ~ LAT, lon = ~ LNG)
        fig <- fig %>% add_markers(
            text = ~ paste(
                paste("Source:", source),
                paste("City:", city_id),
                paste("State:", state_id),
                paste("Zip:", zip_id),
                ps_proj_eligible,
                sep = "<br />"
            ),
            color = ~ ps_proj_eligible,
            hoverinfo = "text"
        )
        fig <- fig %>% layout(
            title = "Northeast",
            geo = g,
            legend = list(orientation = 'h')
        )
    })
    
    output$counting_table <- renderDataTable({
        if (input$switchScreened) {
            ps_location <- ps_location %>%
                filter(screened == "yes")
        }
        df <- ps_location %>%
            filter(
                ps_proj_eligible %in% proj(),
                source %in% sources(),
                recruit_date >= date_range()[[1]],
                recruit_date <= date_range()[[2]]
            )  %>%
            mutate(city_id = stringr::str_trim(city_id)) %>%
            group_by(zip_id) %>%
            count() %>%
            ungroup() %>%
            mutate("%" = round(n / sum(n) * 100)) %>%
            arrange(-n) %>%
            filter()
        datatable(df,
                  rownames = FALSE,
                  options = list(pageLength = 10))
    })
    
    output$rct_cost_table <- renderDataTable({
        rct_cost(overall_cost,
                 ps_location,
                 input$switchScreened,
                 sources())
    })
    
    output$mailer_cost_table <- renderDataTable({
        mailer_breakdown(mailer_cost,
                         ps_location,
                         proj(),
                         input$switchScreened)
    })
    
    output$mail_effectiveness <- renderTable({
        mailer_effectiveness(mailer_cost,
                             ps_location,
                             proj(),
                             input$switchScreened)
    })
    
    
}

# Run the application
shinyApp(ui = ui, server = server)
