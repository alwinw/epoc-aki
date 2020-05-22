#=================================== Set Up ====================================
#                                Alwin Wang 2019

#---------------------------- Easy Library Manager -----------------------------

#' Library manager
#'
#' Loads packages from a specified requirements file and optionally writes
#' them out to a references file
#'
#' @param req_file A file containing the list of packages to load
#' @param bib_file If not NULL, write out the citations to the given file
#'
#' @return None
#'
#' @examples
#' load_library("R-requirements.txt", "references/R-references.bib")

load_library <- function(req_file, bib_file = NULL) {
  packages <- readLines(req_file)
  new.packages <-
    packages[!(packages %in% installed.packages()[, "Package"])]
  
  if (length(new.packages))
    install.packages(new.packages)
  invisible(suppressMessages(suppressWarnings(
    lapply(packages, library, character.only = TRUE)
  )))
  
  
  if (!is.null(bib_file))
    knitr::write_bib(packages, file = bib_file)
  
  return(NULL)
}

#-------------------------------- Data Sources ---------------------------------

oliguria_xlsx_path     = file.path("data", "Creatinine change in oliguria 4.1.20.xlsx")
creatinine_xlsx_path   = file.path("data", "Small changes in creatinine 27.9.18.xlsx")
demographics_xlsx_path = file.path("data", "Demographics pts screened out.xlsx")
apd_extract_xlsx_path  = file.path("data", "COMET-Extract-APD 23 Oct 2018.xlsx")
creat_furo_xlsx_path   = file.path("data", "ED_ICU_Creatinine_Furosemide.xlsx")

#------------------------------- Load Excel Data -------------------------------

#' Convert Excel date to formatted date string
#' 
#' Some columns will be a mix of "43294" and "13/7/18, 16/7/18" characters. 
#' The "43294" columns will be converted into "13/7/18 format
#' 
#' @param vector A vector containing a mix of Excel dates and strings as strings
#' 
#' @return A vector of human readable dates
#' 
#' @examples 
#' excel_date_to_character(c("43294", "13/7/18"))

excel_date_to_character <- function(vector) {
  suppressWarnings(ifelse(
    grepl("/", vector),
    vector,
    as.character(as.Date(as.numeric(vector), origin = "1899-12-30"),
                 format = "%d/%m/%y")
  ))
}

#' Load Excel Files
#'
#' Loads each of the excel files into a single xlsx list
#'
#' @param path add an optional path to the (global) xlsx paths
#'
#' @return List of list of dataframes containing data from xlsx files
#' 
#' @examples
#' xlsx_data <- load_excel_files()

load_excel_files <- function(path = NULL) {
  if (!is.null(path)) {
    oliguria_xlsx_path     = file.path(path, oliguria_xlsx_path)
    creatinine_xlsx_path   = file.path(path, creatinine_xlsx_path)
    demographics_xlsx_path = file.path(path, demographics_xlsx_path)
    creat_furo_xlsx_path   = file.path(path, creat_furo_xlsx_path)
    apd_extract_xlsx_path  = file.path(path, apd_extract_xlsx_path)
  }
  oliguria_xlsx <- list(
    demographic = read_excel(oliguria_xlsx_path, "Patient Demographics"),
    data_set    = read_excel(oliguria_xlsx_path, "Data set"),
    outcomes    = read_excel(oliguria_xlsx_path, "AKI & outcomes"),
    screen_log  = read_excel(oliguria_xlsx_path, "Screening log")
  )
  creatinine_xlsx <- list(
    demographic = read_excel(creatinine_xlsx_path, "Patient Demographics"),
    data_set    = read_excel(creatinine_xlsx_path, "Data set"),
    outcomes    = read_excel(creatinine_xlsx_path, "AKI & outcomes"),
    screen_log  = read_excel(creatinine_xlsx_path, "Screening log")
  )
  demographics_xlsx <- list(
    num_creatinine = read_excel(demographics_xlsx_path, "no cr change"),
    num_oliguria   = read_excel(demographics_xlsx_path, "no oliguria"),
    neither_cr_ol  = read_excel(demographics_xlsx_path, "neither cr nor olig")
  )
  apd_extract_xlsx <- list(
    apd_extract = read_excel(apd_extract_xlsx_path, "Admissions")
  )
  creat_furo_xlsx <- list(
    blood_gas    = read_excel(creat_furo_xlsx_path, "Blood Gas"),
    bio_chem     = read_excel(creat_furo_xlsx_path, "BioChem"),
    lowest_creat = read_excel(creat_furo_xlsx_path, "Lowest Creatinine Level"),
    furosemide   = read_excel(creat_furo_xlsx_path, "Medication")
  )
  
  names(creatinine_xlsx$screen_log)[ncol(creatinine_xlsx$screen_log)] <-
    "Comment"
  names(oliguria_xlsx$screen_log)[ncol(oliguria_xlsx$screen_log)] <-
    "Comment"
  
  oliguria_xlsx$screen_log   %<>% arrange(`UR number`, Dates_screened) %>%
    mutate(Dates_screened = excel_date_to_character(Dates_screened))
  creatinine_xlsx$screen_log %<>% arrange(`UR number`, Dates_screened) %>%
    mutate(Dates_screened = excel_date_to_character(Dates_screened))
  
  xlsx_data <- list(
    oliguria     = oliguria_xlsx,
    creatinine   = creatinine_xlsx,
    demographics = demographics_xlsx,
    apd_extract  = apd_extract_xlsx,
    time_series  = creat_furo_xlsx
  )
  return(xlsx_data)
}
