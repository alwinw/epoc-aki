# Screening Log Flow Chart
# Alwin Wang 2020

# Screening Data Flow Chart ----
screening_data_flow_chart <- function(screening_data) {
  flow_chart_all <- screening_data %>% 
    summarise(
      `Total Unique UR Numbers`     = length(unique(`UR number`)),
      `Total Admissions`            = n(),
      `> Total Excluded Admissions` = length(`UR number`[!Excl_criteria_ok]),
      `> Total Eligible Admissions` = length(`UR number`[ Excl_criteria_ok])
    )
  
  flow_chart_excluded <- screening_data %>% 
    filter(!Excl_criteria_ok) %>% 
    summarise(
      `| Total Excluded Admissions` = n(),
      `|-- AKI`                = length(`UR number`[Already_AKI      ]),
      `|-- Weekend`            = length(`UR number`[Admit_weekend    ]),
      `|-- No ICD`             = length(`UR number`[No_IDC           ]),
      `|-- ESKD`               = length(`UR number`[ESKD             ]),
      `|-- EOLC`               = length(`UR number`[EOLC             ]),
      `|-- Kidney transplant`  = length(`UR number`[Kidney_transplant]),
      `°-- Child`              = length(`UR number`[Child            ])
    )
}


admissions_flow_chart_excluded <- merged_xlsx_data$screen_log %>% 
  filter(!Excl_criteria_ok) %>% 
  summarise(
    `| Total Excluded Admissions` = n(),
    `|-- AKI`                     = length(`UR number`[Already_AKI      ]),
    `|-- Weekend`                 = length(`UR number`[Admit_weekend    ]),
    `|-- No ICD`                  = length(`UR number`[No_IDC           ]),
    `|-- ESKD`                    = length(`UR number`[ESKD             ]),
    `|-- EOLC`                    = length(`UR number`[EOLC             ]),
    `|-- Kidney transplant`       = length(`UR number`[Kidney_transplant]),
    `°-- Child`                   = length(`UR number`[Child            ])
  ) # could do something more fancy here with gather?

admissions_flow_chart_included_cr <- merged_xlsx_data$screen_log %>% 
  filter(Excl_criteria_ok) %>% 
  group_by(Total_no_cr_epis) %>% 
  summarise(
    `> Total Eligible Admissions` = n()
  )

admissions_flow_chart_included_olig <- merged_xlsx_data$screen_log %>% 
  filter(Excl_criteria_ok) %>% 
  group_by(Total_no_olig_epis) %>% 
  summarise(
    `> Total Eligible Admissions` = n()
  )

admissions_flow_chart_included_all <- merged_xlsx_data$screen_log %>% 
  filter(Excl_criteria_ok) %>% 
  group_by(Total_no_olig_epis) %>% 
  summarise(
    n = n()
  )