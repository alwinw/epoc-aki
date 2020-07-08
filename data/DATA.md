# DATA

## Location

The data is not included in the Git Repository. Please contact the repository owner if you require it.

## Raw Data

Creatinine change in oliguria 4.1.20

- Observation study for creatinine change in oliguria
- Patient Demographics sheet
  - Coded LXXX patient study numbers
  - Classification variables
  - Apache scores potentially inconsistent (`APACHE_II`, `APACHE_III`)
- Dataset
  - Oliguria episodes determined by looking at the medical records
  - T-4 variables (4 hours before episode)
  - T-0 variables (at episode)
- AKI & outcomes
  - Binary AKI occurance
  - Type of AKI
  - Other output variables
- Screening log
  - All patients screened for the study
  - Inclusion and exclusion criteria
  - Link between `UR number` and `Pt_Study_no`
- 04-Jan-20
  - `'Data set'!D1` renamed from `Time_olig_episode` to `Time_olig_epis`
  - `'AKI & Outcomes'!D120` changed from `4/18` to blank
- 08-Jul-20
  - `'AKI & Outcomes'!AI51` changed from `Y incl. not for RRT` to `Y`

Small changes in creatinine 5.6.20.xlsx

- Observation study for small changes in creatinine
- Patient Demographics sheet
  - Coded LTXXX patient study numbers
  - Classification variables
  - Apache scores potentially inconsistent (`APACHE_II`, `APACHE_III`)
- Dataset
  - Creatinine change determined by looking at the medical records
  - T-4 variables (4 hours before episode)
  - T-0 variables (at episode)
- AKI & outcomes
  - Binary AKI occurance
  - Type of AKI
  - Other output variables
- Screening log
  - All patients screened for the study
  - Inclusion and exclusion criteria
  - Link between `UR number` and `Pt_Study_no`
- 05-Jun-20
  - `'Data set'!E347` changed from `10:00` to `12:00`
  - `'Data set'!Q347` changed from `14:00` to `16:00`
  - `'Data set'!D569` changed from `09:10` to `19:10`
- 08-Jul-20
  - `'AKI & Outcomes'!AI70` changed from `Y incl. not for RRT` to `Y`

Demographics pts screened out.xlsx

- Screened out due to no creatinine or oliguria episode
- Add a `LXxxx` number

COMET-Extract-APD 23 Oct 2018.xlsx

- Database from which the Apache scores were extracted from
- Admissions sheet
  - Personably identifiable information
  - Identification variables (`HRN/NIH` = `UR Number`, `ICU_ADM_DTM`)
  - Apache scores 1 per admission assessment (`AP2score`, `AP3score`)
- Other sheets are empty

ED_ICU_Creatinine_Furosemide.xlsx

- Time series data of creatinine and medication

Record of patients.xlsx

- Scratch working and records of analysis

## Merged data

Objectives include

- Coded Patient ID (`Lxx`, `LTxx`, `LXxx`)
- Demographic Variables (`Age`, `Male`, `Wt`, etc)
  - Should match between `Lxx` and `LTxx`
- Hospital Admission
  - `Date_ICU_admit` & `Time_ICU_admit` should match COMET `ICU_ADM_DTM`
  - Lowest creatinine level from Creatinine_Furosemide
- Creatinine Data
  - Potentially multiple events
  - T-4 and T-0 variables
- Oliguria Data
  - Potentially multiple events
  - T-4 and T-0 vaiables
- Creatinine Time Series
  - DTTM (date time month) pathology results
  - From blood gas or biochem
- Furosemide Time Series
  - DTTM medication admission
  - Administration route (IV twice as strong as oral)
- AKI output
  - Outcomes
