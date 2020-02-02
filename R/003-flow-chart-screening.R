# Screening Log Flow Chart
# Alwin Wang 2020

# Screening Data Flow Chart ----
screening_data_flow_chart <- function(screening_data) {
  
  all = paste0(
    "Total Unique UR Numbers:      ", length(unique(screening_data$`UR number`)), "\n",
    "Total Admissions:             ", nrow(screening_data), "\n")
  
  
  excluded = paste0(
    "|- Total Excluded Admissions: ", sum(!screening_data$Excl_criteria_ok), "\n",
    "|  |- AKI:                    ", 
    
    "|- Total Included Admissions: ", sum(screening_data$Excl_criteria_ok), "\n"
  )
}


# Screening Data Flow Chart ----
screening_data_flow_chart_tables <- function(screening_data) {
  flow_chart_all <- screening_data %>% 
    summarise(
      `Total Unique UR Numbers`     = length(unique(`UR number`)),
      `Total Admissions`            = n(),
      `|-- Total Excluded Admissions` = length(`UR number`[!Excl_criteria_ok]),
      `°-- Total Eligible Admissions` = length(`UR number`[ Excl_criteria_ok])
    )
  
  flow_chart_excluded <- screening_data %>% 
    filter(!Excl_criteria_ok) %>% 
    summarise(
      `Total Excluded Admissions` = n(),
      `|-- AKI`                = length(na.omit(`UR number`[Already_AKI      ])),
      `|-- Weekend`            = length(na.omit(`UR number`[Admit_weekend    ])),
      `|-- No ICD`             = length(na.omit(`UR number`[No_IDC           ])),
      `|-- ESKD`               = length(na.omit(`UR number`[ESKD             ])),
      `|-- EOLC`               = length(na.omit(`UR number`[EOLC             ])),
      `|-- Kidney transplant`  = length(na.omit(`UR number`[Kidney_transplant])),
      `°-- Child`              = length(na.omit(`UR number`[Child            ]))
    )
  
  flow_chart_included_cr <- screening_data %>% 
    filter(Excl_criteria_ok) %>% 
    group_by(Total_no_cr_epis) %>% 
    summarise(
      `> Total Eligible Admissions` = n()
    )
  
  flow_chart_included_olig <- screening_data %>% 
    filter(Excl_criteria_ok) %>% 
    group_by(Total_no_olig_epis) %>% 
    summarise(
      `> Total Eligible Admissions` = n()
    )
  
  flow_chart_included_both <- screening_data %>% 
    filter(Excl_criteria_ok) %>% 
    mutate(
      crch_ep = ifelse(!is.na(Epis_cr_change) & Epis_cr_change, "Cr Change", "No Cr Change"),
      olig_ep = ifelse(!is.na(Epis_olig) & Epis_olig, "Oliguria", "No Oliguria")
    ) %>% 
    group_by(both = interaction(crch_ep, olig_ep, sep = " & ")) %>% 
    summarise(
      `> Total Elibible Admissions` = n()
    )
}
