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

# import data

plot_df <- read.csv("./out/plot_df.csv")
ps_location <- read.csv("./out/ps_locations.csv")
no_location_data <- read.csv("./out/no_location_data.csv")

mailer_cost <- read.csv("./data/mailer_cost.csv")
overall_cost <- read.csv("./data/overall_cost.csv")

last_update <- read.table("./out/lastupdate.txt")$V1

# functions --------

mailer_response <- function(ps_location, mailer_cost) {
    
    # we don't know exactly when the mailers are sent, we only get
    # an "order finalized" date
    date_pad <- 14
    
    df <- mailer_cost %>%
       left_join(ps_location, by = "zip_id") %>%
       filter(source == "direct_mail") %>%
       group_by(recruit_date, date_sent, location = city, zip = zip_id) %>%
       count() %>%
       mutate(date_recode = lubridate::mdy(date_sent) - date_pad)
   
   ggplot(df, aes(x = as.Date(recruit_date), y = n)) +
       geom_point() +
       geom_vline(
           aes(xintercept = date_recode),
           linetype = "dashed") +
       geom_label(aes(x = date_recode, y = max(n) - 2, label = date_recode)) +
       facet_wrap(~ location) +
       theme(axis.text.x = element_text(angle = 45, hjust= 1)) +
       labs(
           x = "",
           y = "Prescreens"
       )
}


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
            mutate(
                enrolled = ifelse(sl_status %in% c("Complete", "In Progress"), "enrolled", "not_enrolled"),
                enrolled = factor(enrolled, levels = c("enrolled", "not_enrolled"))
            )  %>%
            group_by(zip_id, location = city, volume)
        
        if (filter_screen) {
            df <- df %>%
                filter(screened == "screened")
        }
        
        # 
        # df %>%
        #     mutate(
        #         enrolled = ifelse(sl_status %in% c("Complete", "In Progress"), "enrolled", "not_enrolled"),
        #         enrolled = factor(enrolled, levels = c("enrolled", "not_enrolled"))
        #     ) %>%
        #     count(enrolled, .drop = FALSE) %>%
        #     filter(enrolled == "enrolled")
            
        
        scrn <- df %>%
            filter(screened == "screened") %>%
            count(name = "screenings") %>%
            ungroup() %>%
            select(zip_id, screenings)
        
        ps <- df %>%
            count(name = "prescreens")
        
        enrolled <- df %>%
            filter(enrolled == "enrolled") %>%
            count(enrolled, name = "enrolled")
        
        full_join(ps, scrn, by = "zip_id") %>%
            left_join(enrolled, by = c("zip_id", "location", "volume")) %>%
            rename("zip" = zip_id) %>%
            mutate(zip = ifelse(nchar(zip) == 4, paste("0", zip, sep = ""), zip)) %>%
            mutate(across(everything(), .fns = ~tidyr::replace_na(., 0))) %>%
            datatable(rownames = FALSE,
                      options = list(
                          columnDefs = list(list(className = 'dt-center', targets = "_all"))
                      ))
    }

mailer_overall <- function(ps_location, mailer_cost) {
    df <- mailer_cost %>%
        left_join(ps_location, by = "zip_id") %>%
        filter(source == "direct_mail") %>%
        mutate(
            enrolled = ifelse(sl_status %in% c("Complete", "In Progress"), "enrolled", "not_enrolled"),
            enrolled = factor(enrolled, levels = c("enrolled", "not_enrolled"))
        )
    
    ps <- df %>% count(name = "prescreens")
    screened <- df %>% filter(screened == "screened") %>% count(name = "screened")
    enrolled <- df %>% filter(enrolled == "enrolled") %>% count(name = "enrolled")
    
    totals <- df %>% select(volume, cost) %>% 
        distinct() %>% 
        summarize(across(everything(), sum))
    
    cbind(totals, ps, screened, enrolled) %>%
        mutate(volume = as.integer(volume)) %>%
        datatable(
            rownames = FALSE,
            options = list(
                columnDefs = list(list(className = 'dt-center', targets = "_all")),
                dom = 't'
            )
        ) %>%
        formatCurrency(columns = "cost")
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

rct_rate_table <- function(ps_location) {
    t <- ps_location %>%
        mutate(recruit_date = as.Date(recruit_date)) %>%
        group_by(recruit_date, source, screened, screen_status, sl_status, ps_proj_eligible, recruit_int_summ, randomized) %>%
        count() %>%
        ungroup()
    
    n_ps <- ps_location %>%
        group_by(source) %>%
        count(name = "n_prescreens")
    
    ps_result <- t %>%
        select(recruit_date, source, recruit_int_summ, n) %>%
        mutate(recruit_int_summ = factor(
            recruit_int_summ, levels = c(
                "screening_scheduled",
                "waiting_list",
                "screening_declined",
                "ineligible",
                "lost_contact"
            )
        )) %>%
        tidyr::complete(recruit_int_summ) %>%
        tidyr::pivot_wider(
            names_from = recruit_int_summ, 
            values_from = n,
            values_fill = 0,
            values_fn = sum
        ) %>%
        rowwise() %>%
        mutate(waiting_list = sum(waiting_list, `NA`)) %>%
        select(-`NA`)
    
    screen_status <- t %>%
        select(recruit_date, source, screen_status, n) %>%
        mutate(screen_status = factor(
            screen_status, levels = c(
                "declined_consent",
                "no-show",
                "screened")
        )) %>%
        tidyr::drop_na() %>%
        tidyr::complete(screen_status) %>%
        tidyr::pivot_wider(
            names_from = screen_status,
            values_from = n,
            values_fill = 0,
            values_fn = sum
        )
    
        # TODO make factors explicit
    sl_status <- t %>%
        select(recruit_date, source, sl_status, n) %>%
        # mutate(sl_status = factor(
        #     sl_status, levels = levels(t$sl_status)
        # )) %>%
        tidyr::drop_na() %>%
        tidyr::complete(sl_status) %>%
        tidyr::pivot_wider(
            names_from = sl_status,
            values_from = n,
            values_fill = 0,
            values_fn = sum
        )
    
    randomized <- t %>%
        select(recruit_date, source, randomized, ps_proj_eligible, n) %>%
        filter(randomized == "randomized") %>%
        select(-randomized) %>%
        rename("randomized" = n)
    
   tmp <- full_join(ps_result, sl_status)
   tmp <- full_join(tmp, randomized)
   tab <- full_join(tmp, screen_status)
   

       
   tab <- tab %>%
       group_by(source) %>%
       summarize(across(.cols = -c(recruit_date, ps_proj_eligible), .fns = sum, na.rm = TRUE)) %>%
       ungroup() %>%
       full_join(n_ps, by = "source") %>%
       rowwise()
   
   tab %>%
       select(
           source, 
           `total\nprescreens*` = n_prescreens,
           `prescreen\nineligible**` = ineligible,
           `screening\ndeclined` = screening_declined,
           `awaiting\nresponse` = waiting_list,
           `lost\ncontact` = lost_contact,
           `no-show`,
           `declined\nconsent` = declined_consent,
           `screening\nineligible` = `Screening Ineligible`,
           randomized
       ) %>%
       mutate(
           across(.cols = -c(source, `total\nprescreens*`),
                     ~ paste(.x, " (", round(.x/`total\nprescreens*` * 100), "%)", sep =  ""))
             ) %>%
       filter(!is.na(source)) %>%
       datatable(rownames = FALSE,
                 options = list(
                     columnDefs = list(list(className = 'dt-center', targets = "_all"))
                 ))
        
}


rct_flow <-
    function(plot_df,
             ps_proj_list,
             filter_low_n,
             filter_screened,
             source_list,
             date_range_list,
             remove_source) {
        # n_colors <-  nrow(distinct(df %>% ungroup() %>% select(ps_proj_eligible)))
        n_colors <- 6
        colors <- RColorBrewer::brewer.pal(n_colors, "Dark2")
        
        df <- plot_df %>%
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
                    "Project 3" = colors[5],
                    "Project 4" = colors[6]
                )
            )
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
    "Participated in Other Studies" = "pt_in_other_studies",
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
                    "Project 4" = "Project 4",
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
                selected = c("BuildClinical", "direct_mail", "facebook"),
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
                    "Recruitment Rates",
                    br(),
                    dataTableOutput("rct_rate_table"),
                    h5("* All percents refer to % of total prescreens."),
                    h5("** Interviewer-determined. May differ from initial eligibility determination.")
                ),
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
                    "Map",
                    br(),
                    plotlyOutput("vt_map"),
                    br(),
                    dataTableOutput("counting_table")
                ),
                tabPanel(
                    "Direct Mail Breakdown",
                    br(),
                    dataTableOutput("mailer_cost_table"),
                    br(),
                    br(),
                    h4("Totals"),
                    dataTableOutput("mailer_total_table"),
                    br(),
                    br()
                ),
                tabPanel(
                    "Mailer Response",
                    br(),
                    plotOutput("mailer_response_plot",
                               height = "800px")
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

    output$rct_rate_table <- renderDataTable({
        df <- ps_location %>%
            filter(
                ps_proj_eligible %in% proj(),
                source %in% sources(),
                recruit_date >= date_range()[[1]],
                recruit_date <= date_range()[[2]]
            )
        rct_rate_table(df)
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
            group_by(city = city_id, zip = zip_id) %>%
            count(name = "n_prescreens") %>%
            ungroup() %>%
            mutate(
                "% from this source" = round(n_prescreens / sum(n_prescreens) * 100),
                zip = ifelse(nchar(zip) == 4, paste("0", zip, sep = ""), zip)
                ) %>%
            arrange(-n_prescreens)
        
        datatable(df,
                  rownames = FALSE,
                  options = list(
                      pageLength = 10,
                      columnDefs = list(list(className = 'dt-center', targets = "_all"))
                  ))
    })

    output$mailer_cost_table <- renderDataTable({
        mailer_breakdown(mailer_cost,
                         ps_location,
                         proj(),
                         input$switchScreened)
    })
    
    output$mailer_total_table <- renderDataTable({
        ps_location %>%
            filter(
                ps_proj_eligible %in% proj(),
                source %in% sources(),
                recruit_date >= date_range()[[1]],
                recruit_date <= date_range()[[2]]
            ) %>%
            mailer_overall(mailer_cost)
            
    })
    
    output$mailer_response_plot <- renderPlot({
        ps_location %>%
            filter(
                ps_proj_eligible %in% proj(),
                source %in% sources(),
                recruit_date >= date_range()[[1]],
                recruit_date <= date_range()[[2]]
            ) %>%
            mailer_response(mailer_cost)
    })
}

# Run the application
shinyApp(ui = ui, server = server)
