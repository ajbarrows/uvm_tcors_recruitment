# Make API call from REDCap datasets to gather prescreen location information
# Tony Barrows 2021-09-28

library(dplyr)
library(redcapAPI)
library(ggplot2)

# functions ---------

build_rcon <- function(rc){
  # Wrapper around the redcapAPI::redcapConnection() using
  # a .csv token sheet in the working directory.
  url <- 'https://redcap.med.uvm.edu/api/'
  # import passwords document
  pw <- read.csv("password.csv", stringsAsFactors = FALSE)
  token <- pw$password[pw$username == rc]
  rcon <- redcapConnection(url = url, token = token)
  
  return(rcon)
}

download_rc_dataframe <- function(rcon, fields = NULL, events = NULL, study = "s3"){
  # Download dataframe -----------------
  df <- exportRecords(rcon, fields = c("screen_id", fields), labels = FALSE, survey = FALSE,
                      dag = TRUE, events = events, form_complete_auto = FALSE, 
                      dates = FALSE, factors = FALSE)
  df <- df %>% 
    filter(!grepl("-2", df$screen_id) & nchar(screen_id) == 6)
  
  if (study == "s3"){
    df %>% filter(
      grepl("X-", df$screen_id) |
        grepl("Y-", df$screen_id) |
        grepl("Z-", df$screen_id)
    )
  } else if (study == "s2") {
    df %>% filter(
      grepl("J-", df$screen_id) |
        grepl("K-", df$screen_id) |
        grepl("L-", df$screen_id)
    )
  }
}

pjt_ste <- function(df){
  # impose project and site values on a data frame
  pjt <- substr(df$screen_id, 1, 1)
  ste <- substr(df$screen_id, 3, 3)
  
  df$project <- NA
  df$project[pjt == "X"] <- "Project 1"
  df$project[pjt == "Y"] <- "Project 2"
  df$project[pjt == "Z"] <- "Project 3"
  df$project[pjt == "J"] <- "Project 1"
  df$project[pjt == "K"] <- "Project 2"
  df$project[pjt == "L"] <- "Project 3"
  
  df$site <- NA
  df$site[ste == "A"] <- "uvm"
  df$site[ste == "B"] <- "brown"
  df$site[ste == "C"] <- "jhu"
  df$site <- factor(df$site, levels = c("uvm", "brown", "jhu"))
  
  return(df)
}

pi_prop <- function(df) {
  df$pi_prop <- ifelse(substr(df$screen_id, 4, 4) == 9, "pilot", "proper")
  df
}

pull_status <- function(rcon) {
  fields <- c("sl_status", "is2_date")
  events <- c("screening_arm_1", "baseline_2_arm_1")
  
  df <- download_rc_dataframe(rcon, fields = fields, events = events)
  df %>%
    group_by(screen_id) %>%
    tidyr::fill(is2_date, .direction = "updown") %>%
    filter(redcap_event_name == "screening_arm_1") %>%
    mutate(
      baseline2_date = as.Date(is2_date),
      sl_status = redcapFactorFlip(sl_status)
    ) %>%
    pjt_ste() %>%
    pi_prop() %>%
    select(screen_id, sl_status, baseline2_date, project, site, pi_prop) %>%
    ungroup()
}

download_ps <- function(rcon, fields = NULL, events = NULL) {
  exportRecords(
    rcon, fields = fields, labels = FALSE, survey = FALSE,
    dag = TRUE, events = events, form_complete_auto = FALSE,
    factors = FALSE
  )
}

clean_ps <- function(ps_df) {
  filter_str <- "-|_|copy|incomplete|empty"
  
  ps_df %>%
    filter(
      !stringr::str_detect(redcap_id, filter_str),
      !is.na(recruit_date),
      as.Date(recruit_date) > as.Date("2020-10-01")
    )
}

recode_facebook <- function(ps_df, start_date, end_date) {
  ps_df$recruit_1___20[ps_df$recruit_date >= start_date & 
                        ps_df$recruit_date <= end_date & 
                        ps_df$recruit_1___5 == 1] <- 1
  ps_df$recruit_1___5[ps_df$recruit_date >= start_date & 
                   ps_df$recruit_date <= end_date] <- 0
  ps_df
}

ps_share <- function(df_id, df_ps) {
  # merge identifying information with data 
  id <- df_id
  # arrange to spec
  df_ps_sub <- df_ps %>%
    select(-c(redcap_data_access_group, redcap_event_name))
  
  df <- left_join(df_ps_sub, id, by = c("recruit_identinfo_id" = "online_id"))
  df <- df %>%
    filter(!redcap_id %in% "test") %>%
    select(-c(recruit_identinfo_id))
  df$recruit_date <- as.Date(df$recruit_date)
  return(df)
}


download_ps_data <- function() {
  rcon_ps <- build_rcon("rc_prescreen_uvm")
  rcon_id <- build_rcon("rc_id_info_uvm")
  
  ps_fields <- c(
    "redcap_id", "recruit_identinfo_id", "recruit_date", 
    "elig_project_1", "elig_project_2", "elig_project_3",
    "elig_project_none", "screen_subjectid", "recruit_1",
    "recruit_1b", "recruit_int_summ"
  )
  
  id_fields <- c(
    "online_id", "fname_id", "lname_id", "address_id", "city_id", "state_id",
    "zip_id", "phone_id", "email_id"
  )
  
  df_ps <- download_ps(rcon_ps, fields = ps_fields, NULL)
  df_id <- download_ps(rcon_id, fields = id_fields, NULL)
  df_ps <- clean_ps(df_ps)
  
  ps_share(df_id, df_ps)
}

download_ps_p4 <- function() {
  rcon_ps_p4 <- build_rcon("rc_prescreen_p4")
  # rcon_id_p4 <- build_rcon("rd_id_info_p4")
  
  ps_fields <- c(
    "redcap_id", "recruit_identinfo_id", "recruit_date",
    "recruit_int_summ", "recruit_1",
    "recruit_3", "recruit_4", "recruit_4a", "recruit_5",
    "recruit_6", "recruit_7", "recruit_7a", "recruit_8",
    "recruit_9", "recruit_10", "recruit_11", "recruit_12",
    "recruit_13", "recruit_14", "recruit_15"
  )
  
  df_ps <- download_ps(rcon_ps_p4, fields = ps_fields)
  
  df_ps %>%
    mutate(elig_project_4 = ifelse(
      recruit_3 == 1 & 
        (recruit_4 == 0 |
        (recruit_4 == 1 & recruit_4a < 10)) &
        recruit_5 == 0 & 
        recruit_6 == 0 &
        recruit_7a == 0 & 
        (recruit_8 == 0 | recruit_8 == 7777) &
        recruit_9 == 0 &
        recruit_10 == 0 &
        (recruit_11 > 17 & recruit_11 < 45) &
        recruit_12 == 1 &
        recruit_13 == 1 & 
        recruit_14 < 26 & 
        recruit_15 %in% 1:4,
      1, 0
      )) %>%
    select(redcap_id, recruit_identinfo_id, recruit_date,
           recruit_int_summ, starts_with("recruit_1___"))
  
  # df %>%
  #   select(redcap_id, recruit_int_summ, elig_project_4) %>%
  #   mutate(recruit_int_summ = redcapFactorFlip(recruit_int_summ)) %>%
  #   write.csv("./out/p4_check.csv", row.names = FALSE)
}


rename_source <- function(only_one, n_val) {
  only_one <- only_one %>%
    plyr::rename(
      c(
        "recruit_1___0" = "TV",
        "recruit_1___1" = "radio",
        "recruit_1___2" = "metro_news",
        "recruit_1___3" = "neighborhood_news",
        "recruit_1___4" = "flyer",
        "recruit_1___5" = "facebook",
        "recruit_1___6" = "craigslist",
        "recruit_1___7" = "bus_ad",
        "recruit_1___8" = "pt_in_other_studies",
        "recruit_1___9" = "clin_trials_web",
        "recruit_1___10" = "direct_mail",
        "recruit_1___11" = "other_person",
        "recruit_1___12" = "other",
        "recruit_1___13" = "instagram",
        "recruit_1___14" = "google",
        "recruit_1___15" = "youtube",
        "recruit_1___16" = "front_porch_forum",
        "recruit_1___17" = "reddit",
        "recruit_1___18" = "spotify",
        "recruit_1___19" = "tiktok",
        "recruit_1___20" = "BuildClinical"
      )
    ) 
  
  # wrangle data to take values of column names
  
  w1 <- which(only_one[,1:(ncol(only_one)-n_val)] == 1, arr.ind = TRUE)
  w0 <- which(only_one[,1:(ncol(only_one)-n_val)] == 0, arr.ind = TRUE)
  only_one[w1] <- names(only_one)[w1[,"col"]]
  only_one[w0] <- NA
  
  # create single-value
  only_one %>%
    tidyr::unite(
      col = "source",
      TV, radio, metro_news, neighborhood_news,
      flyer, facebook, craigslist, bus_ad, 
      pt_in_other_studies, clin_trials_web,
      direct_mail, other_person, other,
      instagram, google, youtube, front_porch_forum,
      reddit, spotify, tiktok, BuildClinical,
      na.rm = TRUE
    )
}

gather_ps_data <- function(ps_data) {
  # return a list of prescreen zip codes, project eligible, and whether
  # they attended in-person screening
  
  # clean and recode
  ps_data <- ps_data %>%
    filter(!is.na(recruit_date),!is.na(zip_id)) %>%
    rename(flyer_location = recruit_1b) %>%
    mutate(
      ps_proj_eligible = ifelse(elig_project_none == 1, "ineligible", NA),
      ps_proj_eligible = ifelse(elig_project_2 == 1, "Project 2", ps_proj_eligible),
      ps_proj_eligible = ifelse(
        elig_project_1 == 1 &
          elig_project_3 == 1,
        "Project 1 and Project 3",
        ps_proj_eligible
      ),
      ps_proj_eligible = ifelse(
        is.na(ps_proj_eligible) &
          elig_project_1 == 1,
        "Project 1",
        ps_proj_eligible
      ),
      ps_proj_eligible = ifelse(
        is.na(ps_proj_eligible) &
          elig_project_3 == 1,
        "Project 3",
        ps_proj_eligible
      ),
      screened = ifelse(!is.na(screen_subjectid), "yes", "no"),
      state_id = redcapFactorFlip(state_id),
      num_sources = rowSums(across(starts_with("recruit_1")))
    ) %>%
    select(-c(
      elig_project_1,
      elig_project_2,
      elig_project_3,
      elig_project_none,
      redcap_data_access_group,
      email_id,
      phone_id,
      address_id,
      fname_id,
      lname_id
    ))
  
  more_than_one <- ps_data %>%
    filter(num_sources > 1) %>%
    mutate(source = "multiple") %>%
    select(-starts_with("recruit_1"))
  
  
  only_one <- ps_data %>%
    filter(num_sources == 1)
  
  only_one <- rename_source(only_one, n_val = 9)
  
  # manual replace list:
  
  only_one <- only_one %>%
    mutate(zip_id = recode(zip_id,
      "05852" = "05851",
      "05402" = "05401",
      "05246" = "05346",
      "05378" = "05478",
      "05467" = "05408",
      "05856" = "05647",
      "04503" = "05403",
      "05402" = "05452",
      "05750" = "05735",
      "05462" = "05461",
      "05671" = "05661"
    ))
  
  list(only_one, more_than_one)
}

gather_p4 <- function(p4_data) {
  df <- p4_data %>% 
    mutate(num_sources = rowSums(across(starts_with("recruit_1")), na.rm = TRUE))
  
  only_one <- df %>% filter(num_sources == 1)
  only_one <- rename_source(only_one, n_val = 1)
  
  more_than_one <- df %>% 
    filter(num_sources > 1) %>%
    select(!starts_with("recruit_1___"))
  more_than_one$source <- "multiple"
  
  rbind(only_one, more_than_one)
}

join_crosswalk <- function(ps_sub) {
  ps_sub %>%
    mutate(zip_id = as.numeric(zip_id)) %>%
    left_join(
      read.csv("./data/zipcodes.csv", stringsAsFactors = FALSE),
      by = c("zip_id" = "ZIP")
    )
}

recode_flyer <- function(ps_location) {
  # participants often confuse "flyer" with "direct_mail." recode
  # any flyer_location that suggests direct mailing
  
  pattern <- "mail|mailbox|home|house"
  ps_location %>%
    mutate(source = ifelse(
      !is.na(flyer_location) &
        stringr::str_detect(tolower(flyer_location), pattern), 
      "direct_mail",
      source)
      )
}

merge_trial_data <- function(ps_location) {
  status <- pull_status(build_rcon("rc_proper"))
  ps_location %>%
    left_join(status, by = c("screen_subjectid" = "screen_id")) %>%
    mutate(
      randomized = ifelse(!is.na(baseline2_date), "randomized", "not randomized"),
      complete = ifelse(sl_status == "Complete", "complete", "not complete"),
      complete = ifelse(is.na(complete), "not complete", complete),
      screened = ifelse(screened == "yes", "screened", "not screened"),
      recruit_int_summ = recode(
        recruit_int_summ,
        "1" = "screening_scheduled",
        "2" = "waiting_list",
        "3" = "screening_declined",
        "4" = "ineligible",
        "5" = "lost_contact",
        .default = NA_character_
        ),
      recruit_int_summ = ifelse(
        ps_proj_eligible == "ineligible",
        "ineligible",
        recruit_int_summ
        ),
      rand_proj = ifelse(
        randomized == "randomized",
        paste("rand:", project),
        randomized)
    )
}


# main -----
ps_data <- download_ps_data() # API call
ps_data <- ps_data %>% recode_facebook(as.Date("2021-10-22"), Sys.Date())
p <- gather_ps_data(ps_data)
ps_sub_one <- p[[1]]
ps_sub_multi <- p[[2]]

ps_sub <- rbind(ps_sub_one, ps_sub_multi)

ps_location <- join_crosswalk(ps_sub)
ps_location <- recode_flyer(ps_location)

did_not_merge <- ps_location %>%
  filter(is.na(LAT) & !is.na(zip_id)) %>%
  select(recruit_date, ps_proj_eligible, source, screened, zip_id, city_id, state_id)

# impose trial data
ps_location <- merge_trial_data(ps_location)

# p4 ------

p4_data <- download_ps_p4()
# p4_data %>% recode_facebook(as.Date("2021-10-22"), Sys.Date())
p4_data$recruit_1___20 <- NA

p4 <- gather_p4(p4_data)
# TODO # "recruit_int_summ" issue






# write to disk
write.csv(ps_location, "./out/ps_locations.csv", row.names = FALSE)
write.csv(did_not_merge, "./out/no_location_data.csv", row.names = FALSE)

# timestamp
write.table(Sys.Date(), "./out/lastupdate.txt", row.names = FALSE, col.names = FALSE)



