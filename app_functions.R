# App Functions

# common ---

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

ps_exclusion_overall <- list(
  "recruit_3" = "not_smoking_daily",
  "recruit_3a" = "low_cpd",
  "recruit_4b" = "daily_ecig_use",
  "recruit_4d" = "other_tobacco_30days",
  "recruit_5" = "nicotine_replacement",
  "recruit_6" = "quit_smoking_medication",
  "recruit_7" = "plans_to_quit",
  "recruit_8" = "rolls_own_cigs",
  "recruit_13" = "prettage",
  "recruit_17a" = "rx_pain_med"
)

ps_exclusion_p1 <- list(
  "recruit_11" = "male",
  "recruit_11a" = "gender_identity",
  "recruit_14" = "education_level",
  "recruit_14a" = "current_degree_program",
  "age_p1_p4" = "age_p1_p4"
)

ps_exclusion_p2 <- list(
  "recruit_15" = "not_receiving_MAT"
)

ps_exclusion_p3 <- list(
  "recruit_16ab_score" = "gad_score",
  "recruit_16cd_score" = "phq_score",
  "recruit_18a" = "current_psych_txt"
)

ps_exclusion_p4 <- list(
  "recruit_12" = "gest_age",
  "age_p1_p4" = "age_p1_p4",
  "education_level" = "education_level",
  "current_degree_program" = "current_degree_program",
  "gender_identity" = "gender_identity",
  "male" = "male",
  "recruit_12" = "not_pregnant"
)

excl_vector <- list(
  ps_exclusion_overall, 
  ps_exclusion_p1,
  ps_exclusion_p2,
  ps_exclusion_p3,
  ps_exclusion_p4
)


load("./out/ps_locations.RData")


# plotting parameters

theme_set(theme_classic(base_size = 15))

# functions
# 

# overall

filter_source <- function(df, date_range, rct_source) {
  
  min_date <- date_range[1]
  max_date <- date_range[2]
  
  df %>%
    filter(
      recruit_date >= min_date & recruit_date <= max_date,
    ) %>%
    mutate(
      has_source = purrr::map(
        recruitment_sources, 
        stringr::str_detect,
        pattern = rct_source
      ),
      has_source = purrr::map_lgl(
        has_source,
        ~ifelse(TRUE %in% .x, TRUE, FALSE)
      )
    ) %>%
    filter(has_source)
}


# Locations ----

produce_map <- function(df) {
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
  
  # if (input$switchScreened) {
  #   ps_location <- ps_location %>%
  #     filter(screened == "yes")
  # }
  fig <- df %>% plot_geo(lat = ~ LAT, lon = ~ LNG)
  fig <- fig %>% add_markers(
    text = ~ paste(
      # paste("Source:", source),
      # paste("City:", city_id),
      # paste("State:", state_id),
      paste("Zip:", zip_id),
      # ps_proj_eligible,
      sep = "<br />"
    ),
    # color = ~ ps_proj_eligible,
    hoverinfo = "text"
  )
  fig %>% layout(title = "Northeast",
                 geo = g,
                 legend = list(orientation = 'h'))
}


produce_map_table <- function(df) {

  df <- df %>%
    # mutate(city_id = stringr::str_trim(city_id)) %>%
    # group_by(city = city_id, zip = zip_id) %>%
    group_by(zip = zip_id) %>%
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
}

# Location Detail ----

location_facet <- function(df, date_range, limit_top_five = TRUE, limit_gtr_ten = TRUE, source) {
  
  df_plot <- df %>%
    mutate(
      ps_eligible = factor(ifelse(
        elig_project_none == 0, "eligible", "ineligible"
        ),
        levels = c("ineligible", "eligible")
        )
      ) %>%
    filter(!is.na(zip_id)) %>%
    group_by(recruit_date, ps_eligible, zip = zip_id) %>%
    count() %>%
    left_join(load_mailer_ledger(), by = "zip")
  
  tally <- df_plot %>%
    group_by(zip) %>%
    summarize(n = sum(n, na.rm = TRUE))
  
  gtr_ten <- tally %>% filter(n > 10)
  top_five <- tally %>% arrange(-n) %>% head(5)
  
  if (limit_top_five) {
    df_plot <- df_plot %>%filter(zip %in% top_five$zip)
  }

  if (limit_gtr_ten) {
    df_plot <- df_plot %>% filter(zip %in% gtr_ten$zip)
  }

  
  p <- ggplot(df_plot, aes(x = recruit_date)) +
    geom_point(aes(y = n, color = ps_eligible)) +
    # geom_line(aes(y = roll_avg, color = ps_eligible), size = 1) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    xlim(date_range[1], date_range[2]) +
    ylim(0, 7) +
    facet_wrap(~zip) +
    labs(
      color = "",
      y = "Prescreens",
      x = "")
  
  if (source %in% "direct_mail") {
    p <- p + geom_vline(aes(xintercept = date_requested))
  }
  p
  
}


prescreen_overall <- function(df, date_range, source) {
  
  df_plot <- df %>%
    mutate(
      ps_eligible = factor(ifelse(
        elig_project_none == 0, "eligible", "ineligible"
      ),
      levels = c("ineligible", "eligible"))) %>%
    group_by(recruit_date, ps_eligible) %>%
    count()
      

  p <- ggplot(df_plot, aes(x = recruit_date)) +
    geom_point(aes(y = n, color = ps_eligible)) +
    # geom_line(aes(y = roll_avg, color = ps_eligible), size = 1) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    xlim(date_range[1], date_range[2]) +
    ylim(0, 7) +
    labs(
      color = "",
      y = "Prescreens",
      x = ""
      )
  if (source %in% "direct_mail") {
    p <- p + geom_vline(
      data = load_mailer_ledger(),
      aes(xintercept = date_requested)
    )
  }
  p
}


load_mailer_ledger <- function() {
  ledger <- read.csv("./data/mailer_ledger.csv")
  
  ledger %>%
    mutate(date_requested = lubridate::mdy(date_requested)) %>%
    select(zip, date_requested)
}

# Prescreen Inclusion ------
# 
# prescreen_result <- function(df) {
#   tab <- df %>% 
#     mutate(
#       recruit_int_summ = ifelse(
#         elig_project_none == 1,
#         "ineligible",
#         recruit_int_summ
#       )
#     ) %>%
#     group_by(recruit_int_summ) %>%
#     count() %>%
#     tidyr::drop_na() %>%
#     tidyr::pivot_wider(
#       names_from = "recruit_int_summ",
#       values_from = "n"
#     )
#   
#   cbind("total" = nrow(df), tab)
# }


prescreen_flow <- function(df) {
  n <- df %>% count(name = "prescreens")
  elig <- df %>%
    summarize(
      across(
        .cols = c(elig_project_1, elig_project_2, elig_project_3, elig_project_4),
        ~sum(., na.rm = TRUE)))
  names(elig) <- c("PSE 1", "PSE 2", "PSE 3", "PSE 4")
  screened <- df %>% filter(screened == "screened") %>% count(name = "screened")
  
  dt <- cbind(n, elig, screened)
  nmes <- names(dt)
  
  DiagrammeR::grViz(glue::glue(
    .open = "{{",
    .close = "}}",
    
    "digraph prescreen_flowchart {
     graph [rankdir = LR]
     
     node[fontsize = 20]
     node [style = filled, fillcolor = steelblue]
        a [label = '@@1']
        f [label = '@@6']
        
    node [style = filled, fillcolor = lightblue]
        b [label = '@@2']
        c [label = '@@3'] 
        d [label = '@@4']
        e [label = '@@5']

      
      edge [arrowhead = none, arrowsize = 2]
      b -> c -> d -> e
      
      edge [arrowhead = vee, arrowsize = 2]
      a -> b
      e -> f
    }
        [1]: '{{nmes[1]}} \\n {{dt$prescreens}}'
        [2]: '{{nmes[2]}} \\n {{dt$`PSE 1`}}'
        [3]: '{{nmes[3]}} \\n {{dt$`PSE 2`}}'
        [4]: '{{nmes[4]}} \\n {{dt$`PSE 3`}}'
        [5]: '{{nmes[5]}} \\n {{dt$`PSE 4`}}'
        [6]: '{{nmes[6]}} \\n {{dt$screened}}'
  ")
  )
    
 
}


compute_rollmean <- function(df) {
  df %>%
    mutate(ps_eligible = ifelse(elig_project_none == 0, "eligible", "ineligible")) %>%
    select(
      redcap_id, recruit_date, ps_eligible, screened, randomized
    ) %>%
    tidyr::pivot_longer(
      -c(redcap_id, recruit_date)
    ) %>%
    filter(!value %in% c("ineligible", "not screened", "not randomized")) %>%
    select(-c(name, redcap_id)) %>%
    group_by(recruit_date, value) %>%
    count() %>%
    tidyr::complete(
      recruit_date = seq.Date(min(recruit_date), Sys.Date(), by = "day"),
      value,
      fill = list(n = 0)
    ) %>%
    group_by(value) %>%
    mutate(
      # value = factor(value, levels = c("eligible", "screened", "randomized")),
      roll_avg = as.double(zoo::rollmean(n, k=7, fill = NA))
      )
}


plot_variables <- function(df,date_range) {
  
  df_plot <- compute_rollmean(df) %>%
    mutate(n = ifelse(n == 0, NA, n))
    
  
  ggplot(df_plot, aes(x = recruit_date)) +
    geom_point(aes(y = n)) +
    geom_line(aes(y = roll_avg), color = "#00BCD8", size = 1) +
    facet_wrap(~value, nrow = 1) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    xlim(date_range[1], date_range[2]) +
    ylim(0, 7)
    
}







# Prescreen Exclusion ------

parse_eligibility <- function(df_sub, excl_vector) {
  
  tally_exclusion <- function(df_sub) {
    as.data.frame(table(unlist(df_sub$reasons_for_exclusion))) %>%
      select(
        "reason" = Var1, "freq" = Freq
      ) %>%
      mutate(
        `%` = round(freq/sum(freq) * 100, 2)
      )
  }

  df_sub <- df_sub %>% filter(elig_project_none == 1)
  
  gen <- df_sub %>%
    filter(gen_excluded)
  df_excl_gen <- gen %>%
    tally_exclusion() %>%
    filter(reason %in% unlist(excl_vector[1], use.names = FALSE))
  
  spec <- df_sub %>%
    filter(!gen_excluded)
  df_excl_spec <- spec %>%
    tally_exclusion()
  
  df_excl_p1 <- df_excl_spec %>%
    filter(reason %in% unlist(excl_vector[2], use.names = FALSE))
  
  df_excl_p2 <- df_excl_spec %>%
    filter(reason %in% unlist(excl_vector[3], use.names = FALSE))
  
  df_excl_p3 <- df_excl_spec %>%
    filter(reason %in% unlist(excl_vector[4], use.names = FALSE))
  
  df_excl_p4 <- df_excl_spec %>%
    filter(reason %in% unlist(excl_vector[5], use.names = FALSE))
  
  list(
    df_excl_gen, df_excl_p1, df_excl_p2, 
    df_excl_p3, df_excl_p4, nrow(df_sub),
    nrow(spec), max(df_excl_gen$`%`),
    max(df_excl_spec$`%`), nrow(gen)
  )
}


plot_ps_ineligible <- function(df, project, max_pct) {
  
  color_list <- list(
    "Project 1" = "#F8766D",
    "Project 2" = "#7CAE00",
    "Project 3" = "#00BFC4",
    "Project 4" = "#C66CFF",
    "General" = "#E08B00"
  )
  
  fill_color <- color_list[[project]]
  
  ggplot(
    df, aes(x = reorder(reason, `%`), y = `%`)
    ) +
    geom_col(show.legend = FALSE, fill = fill_color) +
    coord_flip() +
    ylim(0, max_pct) +
    labs(
      x = "",
      y = "%",
      title = project
    )
}



# Session Window Calendar ----

session_window_distribution <- function(
  ps_location, ste = "uvm", pjt
  ) {
  
  # Compile visit window times, plot for aid in scheduling
  
  window <- ps_location %>%
    filter(
      !is.na(screen_subjectid), 
      sl_status == "In Progress",
      site %in% ste,
      project %in% pjt
    ) %>%
    tidyr::drop_na() %>%
    mutate(
      win_start = as.POSIXct(sl_baseline1_vwstart, format = "%H:%M"),
      win_end = as.POSIXct(sl_baseline1_vwend, format = "%H:%M")
    ) %>%
    select(screen_subjectid, win_start, win_end, project, site)
  
  window <- window %>%
    group_by(win_start, win_end) %>%
    mutate(block = as.factor(cur_group_id()))
  
  ggplot(window, aes(color = block)) +
    geom_segment(
      aes(
        x = win_start, xend = win_end,
        y = reorder(screen_subjectid, win_start), yend = reorder(screen_subjectid, win_end)),
      size = 3,
      show.legend = FALSE
    ) +
    scale_x_time(labels = label_time("%H:%M"), breaks = scales::breaks_width("30 min")) +
    theme(axis.text.x = element_text(hjust = 1, angle = 90)) +
    labs(
      y = "",
      x = "Visit Window"
      )
}



