## DATA

--------------------------------------------------------------------------------

COMET-Extract-APD 23 Oct 2018.xlsx
* Database from which the Apache scores were extracted from
* Admissions sheet
  - Personably identifiable information 
  - Identification variables (`HRN/NIH` = `UR Number`, `ICU_ADM_DTM`)
  - Apache scores 1 per admission assessment (`AP2score`, `AP3score`)
* Other sheets are empty

Creatinine change in oliguria 27.9.18.xlsx
* Observation study for creatinine change in oliguria
* Patient Demographics sheet
  - Coded LXXX patient study numbers
  - Classification variables
  - Apache scores potentially inconsistent (`APACHE_II`, `APACHE_III`)
* Dataset
  - Oliguria episodes determined by looking at the medical records
  - T-4 variables (4 hours before episode)
  - T-0 variables (at episode)
* AKI & outcomes
  - Binary AKI occurance
  - Type of AKI
  - Other output variables
* Screening log
  - All patients screened for the study
  - Inclusion and exclusion criteria
  - Link between `UR number` and `Pt_Study_no`

Demographics pts screened out.xlsx
* Screened out due to no creatinine or oliguria episode
* Add a `LOxxx` number

ED_ICU_Creatinine_Furosemide.xlsx
* Time series data of creatinine and medication

Record of patients.xlsx
* Scratch working and records of analysis

Small changes in creatinine 27.9.18.xlsx
* Similar to Creatinine sheet

--------------------------------------------------------------------------------

Merged data should contain:
* Coded Patient ID (`Lxx`, `LTxx`, `LOxx`)
* Demographic Variables (`Age`, `Male`, `Wt`, etc)
  - Should match between `Lxx` and `LTxx`
* Hospital Admission
  - `Date_ICU_admit` & `Time_ICU_admit` should match COMET `ICU_ADM_DTM`
  - Lowest creatinine level from Creatinine_Furosemide
* Creatinine Data
  - Potentially multiple events
  - T-4 and T-0 variables
* Oliguria Data
  - Potentially multiple events
  - T-4 and T-0 vaiables
* Creatinine Time Series
  - DTTM (date time month) pathology results
  - From blood gas or biochem
* Furosemide Time Series
  - DTTM medication admission
  - Administration route (IV twice as strong as oral)
* AKI output
  - Outcomes

--------------------------------------------------------------------------------

