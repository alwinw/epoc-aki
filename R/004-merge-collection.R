# Merge Excel data collection
# Alwin Wang 2019

# Merge Excel Sheets Together ----
dttm_cols <- function(text, colnames) {
  cols <- data.frame(
    i = grep(paste0("^", text, "|", text, "$"), colnames, ignore.case = TRUE),
    j = grep(paste0("^", text, "|", text, "$"), colnames, ignore.case = TRUE, value = TRUE),
    stringsAsFactors = FALSE
  ) %>% 
    mutate(k = gsub(text, "DateTime", j, ignore.case = TRUE))
  colnames(cols) <- c(paste0(text, "_i"), paste0(text), "match")
  
  return(cols)
}

merge_xlsx_sheets <- function(data,excluded_Pt_Study_no,
                              data_name = deparse(substitute(data))) {
  combined_sheets <-data$data_set %>% 
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
    dttm_cols("date", colnames(combined_sheets)),
    dttm_cols("time", colnames(combined_sheets)), by = "match") %>% 
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

# Merge Excel Files Together ----
merge_xlsx_creatinine_oliguria <- function(analysis_data, xlsx_data) {
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
      
      craki_stg2or3        = ifelse(Cr_defined_AKI_stage >=2, 1, 0),
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
      
      craki_stg2or3        = ifelse(Cr_defined_AKI_stage >=2, 1, 0),
      oli_craki_in12h      = ifelse(Time_betw_oli_AKI >12 | Cr_defined_AKI == 0, 0, 1),
      oli_craki_in24h      = ifelse(Time_betw_oli_AKI >24 | Cr_defined_AKI == 0, 0, 1),
      oli_craki_in48h      = ifelse(Time_betw_oli_AKI >48 | Cr_defined_AKI == 0, 0, 1),
      oli_craki_2or3_in12h = ifelse(craki_stg2or3 == 1 & oli_craki_in12h == 1, 1, 0),
      oli_craki_2or3_in24h = ifelse(craki_stg2or3 == 1 & oli_craki_in24h == 1, 1, 0),
      oli_craki_2or3_in48h = ifelse(craki_stg2or3 == 1 & oli_craki_in48h == 1, 1, 0)
    )
  
  colnames(creatinine)[1] = "Pt_Study_no_crch"
  colnames(oliguria  )[1] = "Pt_Study_no_olig"
  
  both <- suppressMessages(full_join(creatinine, oliguria))
  
  Total_no_cr_epis = sum(analysis_data$Total_no_cr_epis, na.rm = TRUE)
  if (length(unique(c(
    nrow(creatinine), length(which(!is.na(both$Pt_Study_no_crch))), Total_no_cr_epis))) != 1) 
  {
    stop(paste0(
      "Number of creatinine episodes is not consistent. ", 
      "Merged xlsx creatinine data: "   , nrow(creatinine), ", ",
      "Merged creatinine and oliguria: ", length(which(!is.na(both$Pt_Study_no_crch))), ", ",
      "Merged screening log number: "   , Total_no_cr_epis))
  }
  Total_no_olig_epis = sum(analysis_data$Total_no_olig_epis, na.rm = TRUE)
  if (length(unique(c(
    nrow(oliguria), length(which(!is.na(both$Pt_Study_no_olig))), Total_no_olig_epis))) != 1) 
  {
    stop(paste0(
      "Number of oliguria episodes is not consistent. ", 
      "Merged xlsx oliguria data: "     , nrow(oliguria), ", ",
      "Merged creatinine and oliguria: ", length(which(!is.na(both$Pt_Study_no_olig))), ", ",
      "Merged screening log number: "   , Total_no_olig_epis))
  }
  
  analysis_data <- suppressMessages(full_join(analysis_data, both))
  
  return(analysis_data)
}

# analysis_data = merge_xlsx_creatinine_oliguria(analysis_data, xlsx_data)

# GENERATE FLOW CHART AGAIN AND COMPARE THE PAIR
