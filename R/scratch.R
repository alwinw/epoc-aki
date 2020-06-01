
#--------------------------- Screening Log Flowchart ---------------------------

# TBC

#------------------------- Merge Excel Data Collection -------------------------



#' Merge xlsx file sheets together
#' 
#' Takes creatinine or oliguria and joins the demographics and outcomes sheets
#' together. Then, merges date and time columns into a single DateTime column
#' 
#' @param data Is either creatinine or oliguria
#' @param excluded_Pt_Study_no Used to filter out any data collection errors
#' @param data_name Automatically grabs the name of the data passesd in
#' 
#' @return Dataframe of combined sheets
#' 
#' @examples 
#' creatinine = merge_xlsx_sheets(xlsx_data$creatinine, xlsx_data$excluded_Pt_Study_no)

merge_xlsx_sheets <- function(data, excluded_Pt_Study_no,
                              data_name = deparse(substitute(data))) {
  combined_sheets <- data$data_set %>% 
    fill(Pt_Study_no) %>% 
    full_join(., data$demographic, by = "Pt_Study_no") %>% 
    full_join(., data$outcomes   , by = "Pt_Study_no")
  
  if (nrow(combined_sheets) != nrow(data$data_set)) {
    stop(paste0(
      "Inconsistent number of rows after merging data_set with demographics and outcomes. ",
      "Data: "         , data_name          , ", ",
      "Data set rows: ", nrow(data$data_set), ", ",
      "Merged rows: "  , nrow(combined_sheets), ", "
    ))
  }
  
  dttm_col = inner_join(
    find_cols("date", "DateTime", colnames(combined_sheets)),
    find_cols("time", "DateTime", colnames(combined_sheets)), 
    by = "match") %>% 
    select(date, time, match)
  # Instead, consider using mutate_at to convert date and time columns and paste
  # then into the date columns. Then use rename at to conver to a datetime name
  # and then remove the time columns.
  
  combined_sheets <- combined_sheets %>% 
    select(-last_col()) %>% 
    filter(!(Pt_Study_no %in% excluded_Pt_Study_no)) %>% 
    pivot_longer(
      cols = c(dttm_col$date, dttm_col$time),
      names_to = "dttm_name",
      values_to = "dttm"
    ) %>% 
    mutate(
      type = if_else(grepl("date", dttm_name, ignore.case = TRUE), "date", "time"),
      dttm_name = gsub("date|time", "DateTime", dttm_name, ignore.case = TRUE)
    ) %>% 
    pivot_wider(
      names_from = "type",
      values_from = "dttm"
    ) %>% 
    mutate(
      datetime = ifelse(
        (is.na(date) | is.na(time)),
        NA,
        paste(
          format(date, format = "%Y-%m-%d"),
          format(time, format = "%H:%M:%S")
        )),
      date = NULL,
      time = NULL
    ) %>% 
    pivot_wider(
      names_from = "dttm_name",
      values_from = "datetime"
    )
  
  return(combined_sheets)
}

#' Merge creatinine and oliguria data
#' 
#' After each of the demographics and outcomes sheets are merged, merge the 
#' creatinine and oliguria together and with the screening data
#' 
#' @param screening_data Merged creatinine and oliguria screening data
#' @param xlsx_data A list containing all xlsx data
#' 
#' @return A dataframe of merged variables
#' 
#' @examples 
#' merged_data <- merge_xlsx_creatinine_oliguria(screening_data, xlsx_data)

merge_xlsx_creatinine_oliguria <- function(screening_data, xlsx_data) {
  creatinine = merge_xlsx_sheets(xlsx_data$creatinine, xlsx_data$excluded_Pt_Study_no)
  oliguria   = merge_xlsx_sheets(xlsx_data$oliguria  , xlsx_data$excluded_Pt_Study_no)
  
  creatinine <- creatinine %>% 
    mutate(
      ICU_LOS              = as.duration(DateTime_ICU_admit  %--% DateTime_ICU_dc ) / ddays (1),
      Hosp_LOS             = as.duration(DateTime_hosp_admit %--% DateTime_hosp_dc) / ddays (1),
      ICUadmtoAKIDx        = as.duration(DateTime_ICU_admit  %--% DateTime_AKI_Dx ) / dhours(1),
      Time_betw_ABG        = as.duration(`T-4_ABG_DateTime`  %--% T0_ABG_DateTime ) / dhours(1),
      Time_betw_cr_AKI     = as.duration(T0_ABG_DateTime     %--% DateTime_AKI_Dx ) / dhours(1),
      Time_betw_ICU_cr     = as.duration(DateTime_ICU_admit  %--% T0_ABG_DateTime ) / dhours(1),
      
      crchange             = (T0_ABG_Cr - `T-4_ABG_Cr`),
      delta_cr             =  crchange  / Time_betw_ABG,
      percent_delta_cr     = (crchange  / `T-4_ABG_Cr`) * 100 / Time_betw_ABG,
      
      akistagesv2          = ifelse(AKI_ICU == 0, 0, AKI_stage),
      aki_stg2or3          = ifelse(akistagesv2 >= 2, 1, 0),
      cr_aki_in12h         = ifelse(Time_betw_cr_AKI > 12 | AKI_ICU == 0, 0, 1),
      cr_aki_in24h         = ifelse(Time_betw_cr_AKI > 24 | AKI_ICU == 0, 0, 1),
      cr_aki_in48h         = ifelse(Time_betw_cr_AKI > 48 | AKI_ICU == 0, 0, 1),
      cr_aki_2or3_in12h    = ifelse(aki_stg2or3 == 1 & cr_aki_in12h == 1, 1, 0),
      cr_aki_2or3_in24h    = ifelse(aki_stg2or3 == 1 & cr_aki_in24h == 1, 1, 0),
      cr_aki_2or3_in48h    = ifelse(aki_stg2or3 == 1 & cr_aki_in48h == 1, 1, 0),
      
      craki_stg2or3        = ifelse(Cr_defined_AKI_stage >= 2, 1, 0),
      cr_craki_in12h       = ifelse(Time_betw_cr_AKI > 12 | Cr_defined_AKI == 0, 0, 1),
      cr_craki_in24h       = ifelse(Time_betw_cr_AKI > 24 | Cr_defined_AKI == 0, 0, 1),
      cr_craki_in48h       = ifelse(Time_betw_cr_AKI > 48 | Cr_defined_AKI == 0, 0, 1),
      cr_craki_2or3_in12h  = ifelse(craki_stg2or3 == 1 & cr_craki_in12h == 1, 1, 0),
      cr_craki_2or3_in24h  = ifelse(craki_stg2or3 == 1 & cr_craki_in24h == 1, 1, 0),
      cr_craki_2or3_in48h  = ifelse(craki_stg2or3 == 1 & cr_craki_in48h == 1, 1, 0)
    )
  
  oliguria <- oliguria %>% 
    mutate(
      ICU_LOS              = as.duration(DateTime_ICU_admit  %--% DateTime_ICU_dc   ) / ddays (1),
      Hosp_LOS             = as.duration(DateTime_hosp_admit %--% DateTime_hosp_dc  ) / ddays (1),
      ICUadmtoAKIDx        = as.duration(DateTime_ICU_admit  %--% DateTime_AKI_Dx   ) / dhours(1),
      Time_betw_ABG        = as.duration(`T-4_ABG_DateTime`  %--% T0_ABG_DateTime   ) / dhours(1),
      Time_betw_oli_AKI    = as.duration(DateTime_olig_epis  %--% DateTime_AKI_Dx   ) / dhours(1),
      Time_betw_ICU_oli    = as.duration(DateTime_ICU_admit  %--% DateTime_olig_epis) / dhours(1),
      
      crchange             = (T0_ABG_Cr - `T-4_ABG_Cr`),
      delta_cr             =  crchange  / Time_betw_ABG,
      percent_delta_cr     = (crchange  / `T-4_ABG_Cr`) * 100 / Time_betw_ABG,
      T0_UO_wtadjusted     = (T0_UO / 4)/Wt,
      
      akistagesv2          = ifelse(AKI_ICU == 0, 0, AKI_stage),
      aki_stg2or3          = ifelse(akistagesv2 >= 2, 1, 0),
      oli_aki_in12h        = ifelse(Time_betw_oli_AKI > 12 | AKI_ICU == 0, 0, 1),
      oli_aki_in24h        = ifelse(Time_betw_oli_AKI > 24 | AKI_ICU == 0, 0, 1),
      oli_aki_in48h        = ifelse(Time_betw_oli_AKI > 48 | AKI_ICU == 0, 0, 1),
      oli_aki_2or3_in12h   = ifelse(aki_stg2or3 == 1 & oli_aki_in12h == 1, 1, 0),
      oli_aki_2or3_in24h   = ifelse(aki_stg2or3 == 1 & oli_aki_in24h == 1, 1, 0),
      oli_aki_2or3_in48h   = ifelse(aki_stg2or3 == 1 & oli_aki_in48h == 1, 1, 0),
      
      craki_stg2or3        = ifelse(Cr_defined_AKI_stage >= 2, 1, 0),
      oli_craki_in12h      = ifelse(Time_betw_oli_AKI > 12 | Cr_defined_AKI == 0, 0, 1),
      oli_craki_in24h      = ifelse(Time_betw_oli_AKI > 24 | Cr_defined_AKI == 0, 0, 1),
      oli_craki_in48h      = ifelse(Time_betw_oli_AKI > 48 | Cr_defined_AKI == 0, 0, 1),
      oli_craki_2or3_in12h = ifelse(craki_stg2or3 == 1 & oli_craki_in12h == 1, 1, 0),
      oli_craki_2or3_in24h = ifelse(craki_stg2or3 == 1 & oli_craki_in24h == 1, 1, 0),
      oli_craki_2or3_in48h = ifelse(craki_stg2or3 == 1 & oli_craki_in48h == 1, 1, 0)
    )
  
  colnames(creatinine)[1] = "Pt_Study_no_crch"
  colnames(oliguria  )[1] = "Pt_Study_no_olig"
  
  both <- suppressMessages(full_join(creatinine, oliguria))
  
  Total_no_cr_epis = sum(screening_data$Total_no_cr_epis, na.rm = TRUE)
  if (length(unique(c(
    nrow(creatinine), length(which(!is.na(both$Pt_Study_no_crch))), Total_no_cr_epis))) != 1) 
  {
    stop(paste0(
      "Number of creatinine episodes is not consistent. ", 
      "Merged xlsx creatinine data: "   , nrow(creatinine), ", ",
      "Merged creatinine and oliguria: ", length(which(!is.na(both$Pt_Study_no_crch))), ", ",
      "Merged screening log number: "   , Total_no_cr_epis))
  }
  Total_no_olig_epis = sum(screening_data$Total_no_olig_epis, na.rm = TRUE)
  if (length(unique(c(
    nrow(oliguria), length(which(!is.na(both$Pt_Study_no_olig))), Total_no_olig_epis))) != 1) 
  {
    stop(paste0(
      "Number of oliguria episodes is not consistent. ", 
      "Merged xlsx oliguria data: "     , nrow(oliguria), ", ",
      "Merged creatinine and oliguria: ", length(which(!is.na(both$Pt_Study_no_olig))), ", ",
      "Merged screening log number: "   , Total_no_olig_epis))
  }
  
  # nrow(filter(screening_data, is.na(Total_no_cr_epis) & is.na(Total_no_olig_epis)))
  # nrow(filter(screening_data, !is.na(Total_no_cr_epis) & !is.na(Total_no_olig_epis)))
  # Appears that patients with both olig and cr_ch have duplicate records?
  
  # temp = inner_join(screening_data, both)
  # temp_full = full_join(screening_data, both)
  # length(which(!is.na(temp_full$Cr_epis_no)))
  # length(which(!is.na(temp_full$Olig_epis_no)))
  
  merged_data <- suppressMessages(full_join(screening_data, both))
  
  # Need to devise a robust check or check in flow chart of merged_data
  
  return(merged_data)
}

#--------------------------- Screening Log Flowchart ---------------------------

# TBC

#------------------------------ Merge APACHE Data ------------------------------





setdiff(
  filter(screening_log, Excl_criteria_ok == "Y", !is.na(Total_no_cr_epis))$`UR number`, 
  filter(mer$scre,      Excl_criteria_ok == "Y", !is.na(Total_no_cr_epis))$`UR number`
)

length(filter(screening_log, Excl_criteria_ok == "Y", !is.na(Total_no_cr_epis))$`UR number`)
length(filter(mer$scre,      Excl_criteria_ok == "Y", !is.na(Total_no_cr_epis))$`UR number`)

new <- screening_log$full %>% 
  filter(Excl_criteria_ok == "Y", !is.na(Total_no_cr_epis))

old <- mer$scre %>% 
  filter(Excl_criteria_ok == "Y", !is.na(Total_no_cr_epis))

new$`UR number` == old$`UR number`

new$`UR number`[206] == old$`UR number`[206]
new$`UR number`[207] == old$`UR number`[207]

discrepency <-  new$`UR number`[207]

new %>% filter(`UR number` == discrepency)
old %>% filter(`UR number` == discrepency)

screening_log$creatinine %>% 
  filter(`UR number` == discrepency) %>% 
  select(Pt_Study_no, Excl_criteria_ok, Incl_criteria_ok)

screening_log$oliguria %>% 
  filter(`UR number` == discrepency) %>% 
  select(Pt_Study_no, Excl_criteria_ok, Incl_criteria_ok)

screening_log$screen_in %>% 
  filter(`UR number` == discrepency) %>% 
  select(Pt_Study_no_crch, Excl_criteria_ok, Incl_criteria_ok_crch, Incl_criteria_ok_olig)

screening_log$screen_out %>% 
  filter(`UR number` == discrepency)

screening_log$full %>% 
  filter(`UR number` == discrepency) %>% 
  select(Pt_Study_no_crch, Excl_criteria_ok, Incl_criteria_ok_crch, Incl_criteria_ok_olig)
