# Make API call from REDCap datasets to gather prescreen location information
# Tony Barrows 2021-09-28

library(dplyr)
library(redcapAPI)
library(ggplot2)


# common -------
rct_source_vars <-  list(
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

ps_exclusion_overall <- list(
  "recruit_3" = "not_smoking_daily",
  "recruit_3a" = "low_cpd",
  "recruit_4b" = "daily_ecig_use",
  "recruit_4d" = "other_tobacco_30days",
  "recruit_5" = "nicotine_replacement",
  "recruit_6" = "quit_smoking_medication",
  "recruit_7" = "plans_to_quit",
  "recruit_8" = "rolls_own_cigs",
  "recruit_13" = "age",
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
  "recruit_12a" = "gest_age",
  "age_p1_p4" = "age_p1_p4",
  "education_level" = "education_level",
  "current_degree_program" = "current_degree_program",
  "gender_identity" = "gender_identity",
  "male" = "male",
  "recruit_12" = "not_pregnant"
)

excl_vector <- c(
  ps_exclusion_overall, 
  ps_exclusion_p1,
  ps_exclusion_p2,
  ps_exclusion_p3,
  ps_exclusion_p4
)

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
  # Make REDCap API call. Clean subject IDs according to study indication. If study = NULL,
  # no cleaning will take place.
  
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
  } else {
    df
  }
}

pjt_ste <- function(df){
  # impose project and site values on a data frame
  pjt <- substr(df$screen_id, 1, 1)
  ste <- substr(df$screen_id, 3, 3)
  
  df$project <- NA
  df$project[pjt == "X" | pjt == "J"] <- "Project 1"
  df$project[pjt == "Y" | pjt == "K"] <- "Project 2"
  df$project[pjt == "Z" | pjt == "L"] <- "Project 3"
  
  df$site <- NA
  df$site[ste == "A"] <- "uvm"
  df$site[ste == "B"] <- "brown"
  df$site[ste == "C"] <- "jhu"
  df$site <- factor(df$site, levels = c("uvm", "brown", "jhu"))
  
  return(df)
}

pi_prop <- function(df) {
  # impose pilot/proper designation on a data frame
  
  df$pi_prop <- ifelse(substr(df$screen_id, 4, 4) == 9, "pilot", "proper")
  df
}

pull_status <- function(rcon) {
  # return tidy data frame from the TCORS Study 3 "session log" enrollment
  # status
  
  fields <- c(
    "sl_status", "is2_date", 
    "sl_baseline1_vwstart", "sl_baseline1_vwend"
    )
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
    select(
      screen_id, sl_status, baseline2_date, 
      sl_baseline1_vwstart, sl_baseline1_vwend,
      project, site, pi_prop) %>%
    ungroup()
}

download_ps <- function(rcon, fields = NULL, events = NULL) {
  # Make REDCap API call to recruitment projects. 
  exportRecords(
    rcon, fields = fields, labels = FALSE, survey = FALSE,
    dag = TRUE, events = events, form_complete_auto = FALSE,
    factors = FALSE
  )
}

clean_ps <- function(ps_df, start_date = as.Date("2019-8-01")) {
  # Filter out pre-screen records flagged as duplicate, error, or 
  # having a date outside of the acceptable range.
  
  filter_str <- "-|_|copy|incomplete|empty|TEST|test"
  
  ps_df %>%
    filter(
      !stringr::str_detect(redcap_id, filter_str),
      !is.na(recruit_date),
      as.Date(recruit_date) > start_date
    )
}


ps_share <- function(df_id, df_ps) {
  # merge identifying information with pre-screen data 
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
  # Produce merged pre-screen dataframe
  
  rcon_ps <- build_rcon("rc_prescreen_uvm")
  rcon_id <- build_rcon("rc_id_info_uvm")
 
  ps_fields <- NULL
  
  id_fields <- c(
    "online_id", "fname_id", "lname_id", "address_id", "city_id", "state_id",
    "zip_id", "phone_id", "email_id"
  )
  
  df_ps <- download_ps(rcon_ps, fields = ps_fields, NULL)
  df_id <- download_ps(rcon_id, fields = id_fields, NULL)
  df_ps <- clean_ps(df_ps)
  
  ps_share(df_id, df_ps)
}


rename_matrix <- function(df) {
  # make values names of columns
  w1 <- which(df[,1:ncol(df)] == 1, arr.ind = TRUE)
  w0 <- which(df[,1:ncol(df)] == 0, arr.ind = TRUE)
  
  # avoid zero-length error
  if(length(w1 > 0)) {
    df[w1] <- names(df)[w1[,"col"]]
  }
  df[w0] <- NA
  
  df
}

reshape_matrix <- function(df, rename_list, colname, prefix, rename = TRUE) {
  
  if (rename) {
    df_sub <- df %>%
      plyr::rename(rename_list)
  } else {
    df_sub <- df
  }
  
  df_sub <- df_sub %>%
    select(unlist(rename_list, use.names = FALSE)) %>%
    rename_matrix() %>%
    tidyr::unite(
      col = !!colname,
      sep = ",",
      na.rm = TRUE
    )
  
  df_sub[df_sub == ""] <- NA
  
  df %>%
    select(-starts_with(prefix)) %>%
    cbind(df_sub)
}

recode_flyer <- function(df) {
  # participants often confuse "flyer" with "direct_mail." recode
  # any flyer_location that suggests direct mailing
  
  pattern <- "mail|mailbox|home|house"
  df %>%
    mutate(
      recruit_1___10 = ifelse(
      !is.na(recruit_1b) &
        stringr::str_detect(tolower(recruit_1b), pattern),
        1, recruit_1___10
      ),
      recruit_1___4 = ifelse(
        !is.na(recruit_1b) &
          stringr::str_detect(tolower(recruit_1b), pattern),
        0, recruit_1___4
      )
    )
}

detect_exclusion <- function(df_sub, excl_list) {
  df_sub %>%
    mutate(
      gen_excluded = purrr::map(
        reasons_for_exclusion, 
        stringr::str_detect,
        pattern = excl_list
      ),
      gen_excluded = purrr::map_lgl(
        gen_excluded,
        ~ifelse(TRUE %in% .x, TRUE, FALSE)
      )
    )
}

gather_ps_data <- function(df, rct_source_vars, excl_vector) {
  # sources
  df_source <- df %>%
    recode_flyer() %>%
    reshape_matrix(
      rct_source_vars,
      "recruitment_sources",
      "recruit_1_"
    ) %>%
    select(recruitment_sources)
  
  # exclusion
  df_exclusion <- df %>%
    plyr::rename(excl_vector) %>%
    dplyr::mutate(
      not_smoking_daily = ifelse(not_smoking_daily == 1, 1, 0),
      low_cpd = ifelse(low_cpd <= 5, 1, 0),
      daily_ecig_use = ifelse(daily_ecig_use >= 6, 1, 0),
      other_tobacco_30days = ifelse(other_tobacco_30days > 9, 1, 0),
      age = ifelse(age < 21 | age > 70, 1, 0),
      male = ifelse(male == 0, 1, 0),
      gender_identity = ifelse(gender_identity != 1, 1, 0),
      education_level = ifelse(education_level > 4, 1, 0),
      not_receiving_MAT = ifelse(not_receiving_MAT == 0, 1, 0),
      gad_score = ifelse(gad_score < 3, 1, 0),
      phq_score = ifelse(phq_score < 3, 1, 0),
      gest_age = ifelse(gest_age > 26, 1, 0),
      age_p1_p4 = ifelse(age > 44, 1, 0),
      not_pregnant = ifelse(not_pregnant == 0, 1, 0)
    ) %>%
    reshape_matrix(
      excl_vector,
      "reasons_for_exclusion",
      "recruit_",
      rename = FALSE
    ) %>%
    mutate(reasons_for_exclusion = ifelse(
      elig_project_none > 0,
      reasons_for_exclusion,
      NA
    )) %>%
    select(reasons_for_exclusion)
  
  df_out <- df %>%
    select(
      redcap_id,
      recruit_date,
      recruit_int_summ,
      screen_status,
      screen_subjectid,
      zip_id,
      elig_project_1,
      elig_project_2,
      elig_project_3,
      elig_project_4,
      elig_project_none
    ) %>%
    cbind(df_source) %>%
    cbind(df_exclusion)
  
  df_out %>% 
    detect_exclusion(unlist(excl_vector[1], use.names = FALSE))
}


join_crosswalk <- function(ps_sub) {
  ps_sub %>%
    mutate(zip_id = as.numeric(zip_id)) %>%
    left_join(
      read.csv("./data/zipcodes.csv", stringsAsFactors = FALSE),
      by = c("zip_id" = "ZIP")
    )
}



recode_buildclinical <- function(ps_sub, start_date = as.Date("2021-10-22")) {
  # BuildClinical took over digital recruitment for the TCORS
  # main trial on 10/22/2021
  
  pattern <- "facebook|instagram|google"
  
  ps_sub %>%
    mutate(
      recruitment_sources = ifelse(
        as.Date(recruit_date) >= start_date,
        stringr::str_replace(
          recruitment_sources,
          pattern,
          "BuildClinical"
        ),
        recruitment_sources
      )
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
      screened = ifelse(!is.na(screen_subjectid), "screened", "not screened"),
      recruit_int_summ = recode(
        recruit_int_summ,
        "1" = "screening_scheduled",
        "2" = "waiting_list",
        "3" = "screening_declined",
        "4" = "ineligible",
        "5" = "lost_contact",
        .default = NA_character_
        ),
      # recruit_int_summ = ifelse(
      #   ps_proj_eligible == "ineligible",
      #   "ineligible",
      #   recruit_int_summ
      #   ),
      screen_status = recode(
        screen_status,
        "1" = "declined_consent",
        "2" = "no-show",
        "3" = "screened",
        .default = NA_character_
      ),
      rand_proj = ifelse(
        randomized == "randomized",
        paste("rand:", project),
        randomized)
    )
}

vectorize_final <- function(ps_location) {
  ps_location %>%
    mutate(
      recruitment_sources = as.list(strsplit(recruitment_sources, ",")),
      reasons_for_exclusion = as.list(strsplit(reasons_for_exclusion, ","))
    )
}


# main -----
ps_data <- download_ps_data() # API call
# ps_data <- ps_data %>% recode_facebook(as.Date("2021-10-22"), Sys.Date())
ps_sub <- gather_ps_data(
  ps_data,
  rct_source_vars,
  excl_vector
)

ps_sub <- recode_buildclinical(ps_sub)
ps_location <- join_crosswalk(ps_sub)

# impose trial data
ps_location <- merge_trial_data(ps_location)

# vectorize
ps_location <- vectorize_final(ps_location)



# TODO Lay this out so we can pick a timeframe, look at sources, find reasons for ineligiblity, and rates of everything



# write to disk
# write.csv(ps_location, "./out/ps_locations.csv", row.names = FALSE)
# write.csv(did_not_merge, "./out/no_location_data.csv", row.names = FALSE)
# write.csv(plot_df, "./out/plot_df.csv", row.names = FALSE)

save(ps_location, file = "./out/ps_locations.RData")

# timestamp
write.table(Sys.Date(), "./out/lastupdate.txt", row.names = FALSE, col.names = FALSE)



