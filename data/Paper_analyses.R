#--- Load Packages ----
library(tidyverse)
library(readxl)
library(lubridate)
library(epiR)
library(pROC)
library(OptimalCutpoints)
library(RColorBrewer)
library(reshape2)
library(nricens)
library(Publish)

#--- Set Working Directory ----
# Use rstudioapi to get saved location of this file
# and set it as the working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # Requres devtools, rstudioapi

#--- Load Databases ----
# Assume that the excel sheets are in the same folder
# Print xlsx files in folder
print(list.files(pattern = ".xlsx"))
# Oliguria
olixlsx = "Creatinine change in oliguria 27.9.18.xlsx"
oli <- list(
  demo = read_excel(olixlsx, "Patient Demographics"),
  data = read_excel(olixlsx, "Data set"),
  outc = read_excel(olixlsx, "AKI & outcomes"),
  scre = read_excel(olixlsx, "Screening log")
)
# Creatinine
crexlsx = "Small changes in creatinine 27.9.18.xlsx"
cre <- list(
  demo = read_excel(crexlsx, "Patient Demographics"),
  data = read_excel(crexlsx, "Data set"),
  outc = read_excel(crexlsx, "AKI & outcomes"),
  scre = read_excel(crexlsx, "Screening log")
)
# Load Demographics of pts screened out
demxlsx = "Demographics pts screened out.xlsx"
scr_out <- list(
  nocr = read_excel(demxlsx, "no cr change"),
  nool = read_excel(demxlsx, "no oliguria"),
  neit = read_excel(demxlsx, "neither cr nor olig")
)

#--- Merge screening log ----
# Use the screening log to merge oli and cr databases
# Format the excel numbers as date characters
cre$scre <- cre$scre %>%
  arrange(`UR number`, Dates_screened)
cremultscree <- grepl("/", cre$scre$Dates_screened)
cre$scre$Dates_screened[!cremultscree] <-
  as.character(as.Date(as.numeric(
    cre$scre$Dates_screened[!cremultscree]),
    origin = "1899-12-30"), format = "%d/%m/%y")
oli$scre <- oli$scre %>%
  arrange(`UR number`, Dates_screened)
olimultscree <- grepl("/", oli$scre$Dates_screened)
oli$scre$Dates_screened[!olimultscree] <-
  as.character(as.Date(as.numeric(
    oli$scre$Dates_screened[!olimultscree]),
    origin = "1899-12-30"), format = "%d/%m/%y")
# Check if the screening of patients was the same in cr and oli
asdf <- select(cre$scre, `UR number`, Dates_screened, Excl_criteria_ok) ==
  select(oli$scre, `UR number`, Dates_screened, Excl_criteria_ok)
creerr <- cre$scre[!(asdf[,2] & asdf[,3]),]
olierr <- oli$scre[!(asdf[,2] & asdf[,3]),]
# Use View(creerr) and View(olierr) to see the rows that aren't correct
View(creerr)
View(olierr)

# Patients to exclude!!
exclUR <- c("169498" , "2304764", "2306908",
            "2338149", "2341758", "475126") #6 UR numbers but 7 admissions total
exclL  <- c("L7", "L4")
exclLT <- c("LT14", "LT15", "LT31")
# View patients that need to be excluded
asdf <- filter(cre$scre, `UR number` %in% exclUR)
qwer <- filter(oli$scre, `UR number` %in% exclUR)
View(asdf)
View(qwer)
#exclude pts from scr_out no oli
scr_out$nool <- scr_out$nool %>%
  filter(!(UR %in% exclUR))
# Merge screening logs of oliguria and cr change
mercols <- colnames(cre$scre)[colnames(cre$scre) %in% colnames(oli$scre)]
mercols <- mercols[!(mercols %in% c("Incl_criteria_ok", "Pt_Study_no", "X__1"))]
mer <- list(
  scre = full_join(
    filter(cre$scre, !(`UR number` %in% exclUR)),
    filter(oli$scre, !(`UR number` %in% exclUR)),
    by = mercols, suffix = c("_cre", "_oli"))
)
# Make sure no errors from the join!
if (sum(is.na(mer$scre$`UR number`)) != 0)
  warning("NA in UR Numbers")
if ((nrow(filter(cre$scre, !(`UR number` %in% exclUR))) != nrow(mer$scre)) |
    (nrow(filter(oli$scre, !(`UR number` %in% exclUR))) != nrow(mer$scre)))
  warning("Number of rows is different, PANIC!")

#--- Flow Chart of Admissions ----
# remove any unnecessary variables from earlier code
rm(asdf, creerr, olierr, qwer,
   cremultscree, crexlsx, demxlsx, mercols, olimultscree, olixlsx)
# Print the result
cat(paste("\n",
          "Total Admissions:         ", nrow(mer$scre), "\n",
          "Total Unique UR Number:   ", length(unique(mer$scre$`UR number`)), "\n",
          "Total Excluded Admissions:", nrow(filter(mer$scre, Excl_criteria_ok == "N")), "\n",
          "Total Eligible Admissions:", nrow(filter(mer$scre, Excl_criteria_ok == "Y")), "\n",
          "Excluded Branch\n",
          "  AKI:                    ", nrow(filter(mer$scre, Excl_criteria_ok =="N", Already_AKI == "Y")), "\n",
          "  Weekend:                ", nrow(filter(mer$scre, Excl_criteria_ok =="N", Admit_weekend == "Y")), "\n",
          "  No IDC:                 ", nrow(filter(mer$scre, Excl_criteria_ok =="N", No_IDC == "Y")), "\n",
          "  ESKD:                   ", nrow(filter(mer$scre, Excl_criteria_ok =="N", ESKD == "Y")), "\n",
          "  EOLC:                   ", nrow(filter(mer$scre, Excl_criteria_ok =="N", EOLC == "Y")), "\n",
          "  Kidney transplant:      ", nrow(filter(mer$scre, Excl_criteria_ok =="N", Kidney_transplant == "Y")), "\n",
          "  Child:                  ", nrow(filter(mer$scre, Excl_criteria_ok =="N", Child == "Y")), "\n",
          "Included Cr Branch\n",
          "  Cr change episode:      ", nrow(filter(mer$scre, Excl_criteria_ok =="Y", Epis_cr_change == "Y")), "\n",
          "     1 Cr episode:        ", nrow(filter(mer$scre, Excl_criteria_ok =="Y", Total_no_cr_epis ==  1)), "\n",
          "     2 Cr episode:        ", nrow(filter(mer$scre, Excl_criteria_ok =="Y", Total_no_cr_epis ==  2)), "\n",
          "     3 Cr episode:        ", nrow(filter(mer$scre, Excl_criteria_ok =="Y", Total_no_cr_epis ==  3)), "\n",
          "     4 Cr episode:        ", nrow(filter(mer$scre, Excl_criteria_ok =="Y", Total_no_cr_epis ==  4)), "\n",
          "     5 Cr episode:        ", nrow(filter(mer$scre, Excl_criteria_ok =="Y", Total_no_cr_epis ==  5)), "\n",
          "     6 Cr episode:        ", nrow(filter(mer$scre, Excl_criteria_ok =="Y", Total_no_cr_epis ==  6)), "\n",
          "     7 Cr episode:        ", nrow(filter(mer$scre, Excl_criteria_ok =="Y", Total_no_cr_epis ==  7)), "\n",
          "     8 Cr episode:        ", nrow(filter(mer$scre, Excl_criteria_ok =="Y", Total_no_cr_epis ==  8)), "\n",
          "     9 Cr episode:        ", nrow(filter(mer$scre, Excl_criteria_ok =="Y", Total_no_cr_epis ==  9)), "\n",
          "    10 Cr episode:        ", nrow(filter(mer$scre, Excl_criteria_ok =="Y", Total_no_cr_epis == 10)), "\n",
          "    Total Cr episodes:    ", sum(mer$scre$Total_no_cr_epis, na.rm = TRUE), "\n",
          "  No Cr change episode:   ", nrow(filter(mer$scre, Excl_criteria_ok =="Y", is.na(Epis_cr_change))), "\n",
          "Included Olig Branch\n",
          "  Olig episode:           ", nrow(filter(mer$scre, Excl_criteria_ok =="Y", Epis_olig == "Y")), "\n",
          "     1 Olig episode:      ", nrow(filter(mer$scre, Excl_criteria_ok =="Y", Total_no_olig_epis ==  1)), "\n",
          "     2 Olig episode:      ", nrow(filter(mer$scre, Excl_criteria_ok =="Y", Total_no_olig_epis ==  2)), "\n",
          "     3 Olig episode:      ", nrow(filter(mer$scre, Excl_criteria_ok =="Y", Total_no_olig_epis ==  3)), "\n",
          "     4 Olig episode:      ", nrow(filter(mer$scre, Excl_criteria_ok =="Y", Total_no_olig_epis ==  4)), "\n",
          "     Total Olig episodes: ", sum(mer$scre$Total_no_olig_epis, na.rm = TRUE), "\n",
          "  No Olig episode:        ", nrow(filter(mer$scre, Excl_criteria_ok =="Y", is.na(Epis_olig))), "\n",
          "Cr change and Olig\n",
          "  Both Cr and Olig:       ", nrow(filter(mer$scre, Excl_criteria_ok =="Y", Epis_cr_change == "Y", Epis_olig == "Y")), "\n",
          "  Neither Cr nor Olig:    ", nrow(filter(mer$scre, Excl_criteria_ok =="Y", is.na(Epis_cr_change), is.na(Epis_olig))), "\n",
          "  Cr change only:         ", nrow(filter(mer$scre, Excl_criteria_ok =="Y", Epis_cr_change == "Y", is.na(Epis_olig))), "\n",
          "  Olig only:              ", nrow(filter(mer$scre, Excl_criteria_ok =="Y", is.na(Epis_cr_change), Epis_olig == "Y")), "\n"
))

#must remember to add on 7 admissions (6 unique patients) who were excluded due to AKI already

#number of cr eps not adding up to data sheet
#identify which pt is the problem
test <- cre$data %>%
  fill(Pt_Study_no) %>%
  group_by(Pt_Study_no) %>% top_n(1, Cr_epis_no) %>%
  filter(!(Pt_Study_no %in% exclLT)) %>%
  select(Pt_Study_no, Cr_epis_no)
test <- arrange(test, Pt_Study_no)
View(test)
test2 <- mer$scre %>% filter(!is.na(Pt_Study_no_cre)) %>%
  select(Pt_Study_no_cre, Total_no_cr_epis)
colnames(test2) <- c("Pt_Study_no", "Cr_epis_no")
test2 <- arrange(test2, Pt_Study_no)
View(test2)
nrow(test)
nrow(test2)
test == test2
test[37,]
rm(test,test2)
#problem fixed in database

#--- Merge Cr Change and Olig sheets ----
# Function to convert date and time
DateTime <- function(date, time) {
  if (is.na(date) | is.na(time)) return(NA)
  else return(as.POSIXct(paste(date, format(time, format = "%H:%M:%S"))))
}
# Cr change database making
#  Join the Cr datatable together
mer$cre <- cre$data %>%
  fill(Pt_Study_no)
mer$cre <- full_join(mer$cre, cre$demo, by="Pt_Study_no")
mer$cre <- full_join(mer$cre, cre$outc, by="Pt_Study_no")
if (nrow(mer$cre) != nrow(cre$data))
  warning("Incorrect number of rows in merged Cr change!")
#  Make the datetime fields using POSIXct
mer$cre <- mer$cre %>%
  filter(!(Pt_Study_no %in% exclLT)) %>%
  rowwise() %>%
  mutate(
    DateTime_Cr_epis    = DateTime(Date_Cr_epis,    Time_Cr_epis),
    DateTime_hosp_admit = DateTime(Date_hosp_admit, Time_hosp_admit),
    DateTime_ICU_admit  = DateTime(Date_ICU_admit,  Time_ICU_admit),
    DateTime_ICU_dc     = DateTime(Date_ICU_dc,     Time_ICU_dc),
    DateTime_hosp_dc    = DateTime(Date_hosp_dc,    Time_hosp_dc),
    DateTime_AKI_dx     = DateTime(Date_AKI_Dx,     Time_AKI_Dx),
    DateTime_pre_ABG    = DateTime(`T-4_ABG_date`, `T-4_ABG_time`),
    DateTime_post_ABG   = DateTime(T0_ABG_date,     T0_ABG_time),
    DateTime_max_cr     = DateTime(Max_Cr_Date,     Max_Cr_Time)
  ) %>%
  ungroup() %>%
  mutate(
    ICU_LOS = as.duration(DateTime_ICU_admit %--% DateTime_ICU_dc) / ddays(1),
    Hosp_LOS = as.duration(DateTime_hosp_admit %--% DateTime_hosp_dc) / ddays(1),
    ICUadmtoAKIDx = as.duration(DateTime_ICU_admit %--% DateTime_AKI_dx) / dhours(1),
    Time_betw_ABG = as.duration(DateTime_pre_ABG %--% DateTime_post_ABG) / dhours(1),
    Time_betw_cr_AKI = as.duration(DateTime_post_ABG %--% DateTime_AKI_dx) / dhours(1),
    Time_betw_ICU_cr = as.duration(DateTime_ICU_admit %--% DateTime_post_ABG) / dhours(1),
    crchange = (T0_ABG_Cr - `T-4_ABG_Cr`),
    delta_cr = (T0_ABG_Cr - `T-4_ABG_Cr`)/Time_betw_ABG,
    percent_delta_cr = (((T0_ABG_Cr - `T-4_ABG_Cr`)/`T-4_ABG_Cr`) *100 ) / Time_betw_ABG,
    akistagesv2= ifelse(AKI_ICU == 0, 0, AKI_stage),
    aki2or3 = ifelse(akistagesv2>=2, 1, 0),
    aki12h = ifelse(Time_betw_cr_AKI>12 | AKI_ICU == 0, 0, 1),
    aki24h = ifelse(Time_betw_cr_AKI>24 | AKI_ICU == 0, 0, 1),
    aki48h = ifelse(Time_betw_cr_AKI>48 | AKI_ICU == 0, 0, 1),
    aki2or3in12h = ifelse(aki2or3 == 1 & aki12h ==1, 1, 0),
    aki2or3in24h = ifelse(aki2or3 == 1 & aki24h ==1, 1, 0),
    aki2or3in48h = ifelse(aki2or3 == 1 & aki48h ==1, 1, 0),
    craki2or3 = ifelse(Cr_defined_AKI_stage >=2, 1, 0),
    craki12h = ifelse(Time_betw_cr_AKI>12 | Cr_defined_AKI == 0, 0, 1),
    craki24h = ifelse(Time_betw_cr_AKI>24 | Cr_defined_AKI == 0, 0, 1),
    craki48h = ifelse(Time_betw_cr_AKI>48 | Cr_defined_AKI == 0, 0, 1),
    craki2or3in12h = ifelse(craki2or3 == 1 & craki12h ==1, 1, 0),
    craki2or3in24h = ifelse(craki2or3 == 1 & craki24h ==1, 1, 0),
    craki2or3in48h = ifelse(craki2or3 == 1 & craki48h ==1, 1, 0))
#  Check the output
# View(mer$cre[, grep("Date", colnames(mer$cre))])
#  Determine which Cr changes occur in ICU
mer$cre <- mer$cre %>%
  mutate(Cr_ICU = ifelse(DateTime_Cr_epis - DateTime_ICU_admit > 0, 1, 0))

# Olig database making
#  Join the Olig datatable together
mer$oli <- oli$data %>%
  fill(Pt_Study_no)
mer$oli <- full_join(mer$oli, oli$demo, by="Pt_Study_no")
mer$oli <- full_join(mer$oli, oli$outc, by="Pt_Study_no")
if (nrow(mer$oli) != nrow(oli$data))
  warning("Incorrect number of rows in merged Cr change!")
#  Make the datetime fields using POSIXct
mer$oli <- mer$oli %>%
  filter(!(Pt_Study_no %in% exclL)) %>%
  rowwise() %>%
  mutate(
    DateTime_olig_epis  = DateTime(Date_olig_epis,  Time_olig_episode),
    DateTime_hosp_admit = DateTime(Date_hosp_admit, Time_hosp_admit),
    DateTime_ICU_admit  = DateTime(Date_ICU_admit,  Time_ICU_admit),
    DateTime_ICU_dc     = DateTime(Date_ICU_dc,     Time_ICU_dc),
    DateTime_hosp_dc    = DateTime(Date_hosp_dc,    Time_hosp_dc),
    DateTime_AKI_dx     = DateTime(Date_AKI_Dx,     Time_AKI_Dx),
    DateTime_pre_ABG    = DateTime(`T-4_ABG_date`, `T-4_ABG_time`),
    DateTime_post_ABG   = DateTime(T0_ABG_date, T0_ABG_time),
    DateTime_max_cr = DateTime(Max_Cr_Date, Max_Cr_Time)
  ) %>%
  ungroup() %>%
  mutate(
    ICU_LOS = as.duration(DateTime_ICU_admit %--% DateTime_ICU_dc) / ddays(1),
    Hosp_LOS = as.duration(DateTime_hosp_admit %--% DateTime_hosp_dc) / ddays(1),
    ICUadmtoAKIDx = as.duration(DateTime_ICU_admit %--% DateTime_AKI_dx) / dhours(1),
    Time_betw_ABG = as.duration(DateTime_pre_ABG %--% DateTime_post_ABG) / dhours(1),
    Time_betw_oli_ep_AKI = as.duration(DateTime_olig_epis %--% DateTime_AKI_dx) / dhours(1),
    Time_betw_ICU_oli = as.duration(DateTime_ICU_admit %--% DateTime_olig_epis) / dhours(1),
    crchange = (T0_ABG_Cr - `T-4_ABG_Cr`),
    delta_cr = (T0_ABG_Cr - `T-4_ABG_Cr`)/Time_betw_ABG,
    percent_delta_cr = (((T0_ABG_Cr - `T-4_ABG_Cr`)/`T-4_ABG_Cr`) *100 ) / Time_betw_ABG,
    T0_UO_wtadjusted = (T0_UO/4)/Wt,
    akistagesv2= ifelse(AKI_ICU == 0, 0, AKI_stage),
    aki2or3 = ifelse(akistagesv2>=2, 1, 0),
    aki12h = ifelse(Time_betw_oli_ep_AKI>12 | AKI_ICU == 0, 0, 1),
    aki24h = ifelse(Time_betw_oli_ep_AKI>24 | AKI_ICU == 0, 0, 1),
    aki48h = ifelse(Time_betw_oli_ep_AKI>48 | AKI_ICU == 0, 0, 1),
    aki2or3in12h = ifelse(aki2or3 == 1 & aki12h ==1, 1, 0),
    aki2or3in24h = ifelse(aki2or3 == 1 & aki24h ==1, 1, 0),
    aki2or3in48h = ifelse(aki2or3 == 1 & aki48h ==1, 1, 0),
    craki2or3 = ifelse(Cr_defined_AKI_stage >=2, 1, 0),
    craki12h = ifelse(Time_betw_oli_ep_AKI>12 | Cr_defined_AKI == 0, 0, 1),
    craki24h = ifelse(Time_betw_oli_ep_AKI>24 | Cr_defined_AKI == 0, 0, 1),
    craki48h = ifelse(Time_betw_oli_ep_AKI>48 | Cr_defined_AKI == 0, 0, 1),
    craki2or3in12h = ifelse(craki2or3 == 1 & craki12h ==1, 1, 0),
    craki2or3in24h = ifelse(craki2or3 == 1 & craki24h ==1, 1, 0),
    craki2or3in48h = ifelse(craki2or3 == 1 & craki48h ==1, 1, 0))
#  Check the output
# View(mer$oli[, grep("Date", colnames(mer$oli))])

# Check the numbers are ok of cr and oli full dataframes!
if (sum(mer$scre$Total_no_cr_epis, na.rm = TRUE) != nrow(mer$cre)) {
  warning("Wrong number of rows between merged screening and Cr change xlsx")
  crcheck1 <- mer$cre %>%
    group_by(Pt_Study_no) %>%
    top_n(1, Cr_epis_no) %>%
    arrange(Pt_Study_no) %>%
    select(Pt_Study_no, Cr_epis_no)
  crcheck2 <- mer$scre %>%
    filter(!is.na(Pt_Study_no_cre)) %>%
    arrange(Pt_Study_no_cre) %>%
    select(Pt_Study_no_cre, Total_no_cr_epis)
  crcheck <- data.frame(
    crcheck1[,1], crcheck1[,2] - crcheck2[,2])
  sum(crcheck[,2])
}
if (sum(mer$scre$Total_no_olig_epis, na.rm = TRUE) != nrow(mer$oli)) {
  warning("Wrong number of rows between merged screening and Olig xlsx")
  olicheck1 <- mer$oli %>%
    group_by(Pt_Study_no) %>%
    top_n(1, Olig_epis_no) %>%
    arrange(Pt_Study_no) %>%
    select(Pt_Study_no, Olig_epis_no)
  olicheck2 <- mer$scre %>%
    filter(!is.na(Pt_Study_no_oli)) %>%
    arrange(Pt_Study_no_oli) %>%
    select(Pt_Study_no_oli, Total_no_olig_epis)
  olicheck <- data.frame(
    olicheck1[,1], olicheck1[,2] - olicheck2[,2])
  sum(olicheck[,2])
}

#--- dataframe of patients with both cr and oliguria ----
mer$both <- filter(mer$scre, Excl_criteria_ok =="Y", Epis_cr_change == "Y", Epis_olig == "Y")
nrow(mer$both)
mer$both <- mer$both %>% arrange(Pt_Study_no_cre)
creboth <- filter(mer$cre, Pt_Study_no %in% mer$both$Pt_Study_no_cre)
oliboth <- filter(mer$oli, Pt_Study_no %in% mer$both$Pt_Study_no_oli)
nrow(creboth)
nrow(oliboth)
#creboth and oliboth have different nrow() due to different number of episodes in each
#for descriptive stats use either of these following dataframes
creoliboth1 <- creboth %>% group_by(Pt_Study_no) %>% top_n(-1, Cr_epis_no)
creoliboth2 <- oliboth %>% group_by(Pt_Study_no) %>% top_n(-1, Olig_epis_no)
creoliboth2 <- creoliboth2 %>% arrange(Pt_Study_no)
nrow(creoliboth1)
nrow(creoliboth2)
creoliboth1 <- creoliboth1 %>% arrange(Pt_Study_no)
creoliboth1$Pt_Study_no_oli <- NA
creoliboth1$Pt_Study_no_oli <- mer$both$Pt_Study_no_oli
creoliboth1 <- creoliboth1 %>% arrange(Pt_Study_no_oli)
creoliboth1$Pt_Study_no_oli == creoliboth2$Pt_Study_no
#creoliboth1 and 2 should be identical for the demo and outcomes columns

#number of cr and oli epis for pts with both types of ep
totalcreboth <- creboth %>% group_by(Pt_Study_no) %>%
  top_n(1, Cr_epis_no)
table(totalcreboth$Cr_epis_no)
totaloliboth <- oliboth %>% group_by(Pt_Study_no) %>%
  top_n(1, Olig_epis_no)
table(totaloliboth$Olig_epis_no)

creepismax <- max(totalcreboth$Cr_epis_no)
oliepismax <-  max(totaloliboth$Olig_epis_no)
heatmap <- list()
heatmap$matrix <- matrix(
  0, nrow = oliepismax, ncol = creepismax)
heatmap$id <- select(mer$scre, Pt_Study_no_cre, Pt_Study_no_oli) %>%
  filter(!is.na(Pt_Study_no_cre) | !is.na(Pt_Study_no_oli))
heatmap$cre <- totalcreboth %>%
  select(Pt_Study_no, Cr_epis_no) %>%
  rename(Pt_Study_no_cre = Pt_Study_no)
heatmap$cre <- left_join(heatmap$cre, heatmap$id, by = "Pt_Study_no_cre")
heatmap$oli <- totaloliboth %>%
  select(Pt_Study_no, Olig_epis_no) %>%
  rename(Pt_Study_no_oli = Pt_Study_no)
heatmap$oli <- left_join(heatmap$oli, heatmap$id, by = "Pt_Study_no_oli")
heatmap$join <- full_join(
  heatmap$cre, heatmap$oli, by = c("Pt_Study_no_cre", "Pt_Study_no_oli"))

for (c in 1:creepismax) {
  for (o in 1:oliepismax) {
    heatmap$matrix[o, c] <- nrow(filter(
      heatmap$join, Cr_epis_no == c, Olig_epis_no == o
    ))
  }
}
heatmap$matrix
rowSums(heatmap$matrix)
colSums(heatmap$matrix)

heatmap$plot <- melt(heatmap$matrix)
colnames(heatmap$plot) <- c("Oliguria", "Creatinine", "Number")

ggplot(heatmap$plot, aes(Creatinine, Oliguria)) +
  geom_point(aes(size = Number, colour = Number)) +
  # geom_label(aes(size = Number, label = Number)) +
  scale_y_reverse()

#ggplot the heatmap
croliepsplot <- ggplot(heatmap$plot, aes(Creatinine, Oliguria)) +
  geom_tile(aes(colour = Number, fill = Number)) +
  geom_text(aes(label = Number), colour = "black") +
  scale_y_reverse(expand = c(0,0)) +
  scale_fill_gradientn(colours=(rev(brewer.pal(3, "RdYlGn")))) +
  scale_colour_gradientn(colours=rev((brewer.pal(3, "RdYlGn"))),
                         guide = "none") +
  scale_x_continuous(position = "top", breaks = 1:7, expand = c(0,0)) +
  labs(fill = "Scale") +
  theme_bw() +
  labs(title = "Number of patients with various combinations of
       creatinine change and oliguria episodes",
       x = "Number of creatinine change episodes",
       y = "Number of oliguria episodes") +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        axis.ticks = element_blank())
croliepsplot
ggsave("Combo cr and oli eps number plot.png", croliepsplot,
       dpi = 300, width = 12, height = 8, scale = 1.2, units = "cm")

# use rev() to reverse the order of the colour e.g. rev(brewer.pal(n, palette))

# For compatability with old code, make new dataframes ----
cr_full  <- mer$cre
oli_full <- mer$oli
scr_full <- mer$scre
cr_first <- cr_full %>%
  group_by(Pt_Study_no) %>%
  top_n(-1, Cr_epis_no)
nrow(cr_first)
oli_first <- oli_full %>%
  group_by(Pt_Study_no) %>%
  top_n(-1, Olig_epis_no)
nrow(oli_first)
cr_first_AKI <- cr_first %>% filter(AKI_ICU == 1)
nrow(cr_first_AKI)
cr_first_noAKI <- cr_first %>% filter(AKI_ICU == 0)
nrow(cr_first_noAKI)
oli_first_AKI <- oli_first %>% filter(AKI_ICU == 1)
nrow(oli_first_AKI)
oli_first_noAKI <- oli_first %>% filter(AKI_ICU == 0)
nrow(oli_first_noAKI)
#oliguria only
mer$olionly <- filter(mer$scre, Incl_criteria_ok_cre == "N", Epis_olig == "Y")
nrow(mer$olionly)
olionly <- filter(mer$oli, Pt_Study_no %in% mer$olionly$Pt_Study_no_oli)
nrow(olionly)
olionly_first <- olionly %>% filter(Olig_epis_no == 1)
nrow(olionly_first)
#creatinine only
mer$cronly <- filter(mer$scre, Incl_criteria_ok_oli == "N", Epis_cr_change == "Y")
nrow(mer$cronly)
cronly <- filter(mer$cre, Pt_Study_no %in% mer$cronly$Pt_Study_no_cre)
nrow(cronly)
cronly_first <- cronly %>% filter(Cr_epis_no == 1)
nrow(cronly_first)
#new variables for pts with no cr change
scr_out$nocr %>% filter(is.na(Date_ICU_dc)) %>% nrow()
scr_out$nocr <- scr_out$nocr %>%
  rowwise() %>%
  mutate(
    DateTime_hosp_admit = DateTime(Date_hosp_admit, Time_hosp_admit),
    DateTime_ICU_admit  = DateTime(Date_ICU_admit,  Time_ICU_admit),
    DateTime_ICU_dc     = DateTime(Date_ICU_dc,     Time_ICU_dc),
    DateTime_hosp_dc    = DateTime(Date_hosp_dc,    Time_hosp_dc)
  ) %>%
  ungroup() %>%
  mutate(
    ICU_LOS = as.duration(DateTime_ICU_admit %--% DateTime_ICU_dc) / ddays(1),
    Hosp_LOS = as.duration(DateTime_hosp_admit %--% DateTime_hosp_dc) / ddays(1),
    dc_ICU_alive = ifelse((DateTime_ICU_dc == DateTime_hosp_dc & Dc_destination == "Mortuary"), 0, 1),
    dc_hosp_alive = ifelse(Dc_destination == "Mortuary",0,1)
  )
#new variables for pts without oli
scr_out$nool %>% filter(is.na(Date_ICU_dc)) %>% nrow()
scr_out$nool <- scr_out$nool %>%
  rowwise() %>%
  mutate(
    DateTime_hosp_admit = DateTime(Date_hosp_admit, Time_hosp_admit),
    DateTime_ICU_admit  = DateTime(Date_ICU_admit,  Time_ICU_admit),
    DateTime_ICU_dc     = DateTime(Date_ICU_dc,     Time_ICU_dc),
    DateTime_hosp_dc    = DateTime(Date_hosp_dc,    Time_hosp_dc)
  ) %>%
  ungroup() %>%
  mutate(
    ICU_LOS = as.duration(DateTime_ICU_admit %--% DateTime_ICU_dc) / ddays(1),
    Hosp_LOS = as.duration(DateTime_hosp_admit %--% DateTime_hosp_dc) / ddays(1),
    dc_ICU_alive = ifelse((DateTime_ICU_dc == DateTime_hosp_dc & Dc_destination == "Mortuary"), 0, 1),
    dc_hosp_alive = ifelse(Dc_destination == "Mortuary",0,1)
  )
#new variables for pts with neither
scr_out$neit %>% filter(is.na(Date_ICU_dc)) %>% nrow()
scr_out$neit <- scr_out$neit %>%
  rowwise() %>%
  mutate(
    DateTime_hosp_admit = DateTime(Date_hosp_admit, Time_hosp_admit),
    DateTime_ICU_admit  = DateTime(Date_ICU_admit,  Time_ICU_admit),
    DateTime_ICU_dc     = DateTime(Date_ICU_dc,     Time_ICU_dc),
    DateTime_hosp_dc    = DateTime(Date_hosp_dc,    Time_hosp_dc)
  ) %>%
  ungroup() %>%
  mutate(
    ICU_LOS = as.duration(DateTime_ICU_admit %--% DateTime_ICU_dc) / ddays(1),
    Hosp_LOS = as.duration(DateTime_hosp_admit %--% DateTime_hosp_dc) / ddays(1),
    dc_ICU_alive = ifelse((DateTime_ICU_dc == DateTime_hosp_dc & Dc_destination == "Mortuary"), 0, 1),
    dc_hosp_alive = ifelse(Dc_destination == "Mortuary",0,1)
  )
#last cr episode
cr_last <- cr_full %>% group_by(Pt_Study_no) %>% top_n(1,Cr_epis_no) %>% ungroup()
cr_last_aki <- cr_last %>% filter(AKI_ICU == 1)
cr_last_no_aki <- cr_last %>% filter(AKI_ICU == 0)
#last oli episode
oli_last <- oli_full %>% group_by(Pt_Study_no) %>% top_n(1, Olig_epis_no) %>% ungroup()
oli_last_aki <- oli_last %>% filter(AKI_ICU == "1")
oli_last_no_aki <- oli_last %>% filter(AKI_ICU == "0")
#cr eps with oli at time T0
oliattimeofcrep <- cr_full %>%
  mutate(T0oliguria = ifelse(T0_UO < 2 * Wt , 1, 0)) %>%
  filter(T0oliguria == 1)
nrow(oliattimeofcrep) #103
#cr eps with oli at T-4 and T0
oliaroundcrep <- cr_full %>%
  mutate( T0oliguria = ifelse(T0_UO < 2 * Wt , 1, 0),
          T4oliguria = ifelse(`T-4_UO`<2 * Wt, 1, 0)) %>%
  filter(T0oliguria == 1 & T4oliguria == 1)
nrow(oliaroundcrep) #19
#for AUROCs of oliguria episodes, exclude pts where AKI occurred before oli ep
oli_full1 <- oli_full %>%
  filter(Pt_Study_no != "L11") %>%
  filter(Pt_Study_no != "L17") %>%
  filter(Pt_Study_no != "L50") %>%
  filter(Pt_Study_no != "L81") %>%
  filter(Pt_Study_no != "L97")
oli_first1 <- oli_first %>%
  filter(Pt_Study_no != "L11") %>%
  filter(Pt_Study_no != "L17") %>%
  filter(Pt_Study_no != "L50") %>%
  filter(Pt_Study_no != "L81") %>%
  filter(Pt_Study_no != "L97")
oli_last1 <- oli_last %>%
  filter(Pt_Study_no != "L11") %>%
  filter(Pt_Study_no != "L17") %>%
  filter(Pt_Study_no != "L50") %>%
  filter(Pt_Study_no != "L81") %>%
  filter(Pt_Study_no != "L97")
#change in UO ml/kg/h from T-4 to T0 for all oliguria eps for AUROCs
oli_full1 <- oli_full1 %>%
  mutate(deltaUO = ((`T-4_UO`/4)/Wt) - ((T0_UO/4)/Wt))
oli_full1 %>% filter(deltaUO <=0) %>%
  select(Pt_Study_no, Olig_epis_no, `T-4_UO`, T0_UO, deltaUO) %>% View()
oli_deltaUO <- oli_full1 %>%
  filter(!(Pt_Study_no %in% c("L20", "L33", "L54", "L100", "L107", "L124")))
#--- DEMOGRAPHICS ----
#demographics - compare those with cr ep to those without 279 vs 108 ----
#Age
shapiro.test(cr_first$Age)
summary(cr_first$Age)
shapiro.test(scr_out$nocr$Age)
summary(scr_out$nocr$Age)
wilcox.test(cr_first$Age, scr_out$nocr$Age)
#Male
sum(cr_first$Male)
sum(is.na(cr_first$Male))
sum(cr_first$Male)/nrow(cr_first) *100
sum(scr_out$nocr$Male)
sum(is.na(scr_out$nocr$Male))
sum(scr_out$nocr$Male)/nrow(scr_out$nocr) *100
gendertable <- as.table(rbind(c(sum(scr_out$nocr$Male),
                                sum(cr_first$Male)),
                              c(nrow(scr_out$nocr)-sum(scr_out$nocr$Male),
                                nrow(cr_first)-sum(cr_first$Male))))
dimnames(gendertable) <- list(c("M", "F"), c("no cr", "cr"))
gendertable
chisq.test(gendertable, correct = FALSE)
#Weight
shapiro.test(cr_first$Wt)
summary(cr_first$Wt)
shapiro.test(scr_out$nocr$Wt)
summary(scr_out$nocr$Wt)
wilcox.test(cr_first$Wt, scr_out$nocr$Wt)
#weight measured
sum(cr_first$Wtmeasured)
sum(is.na(cr_first$Wtmeasured))
sum(cr_first$Wtmeasured) / nrow(cr_first) *100
sum(na.omit(scr_out$nocr$Wtmeasured))
sum(is.na(scr_out$nocr$Wtmeasured))
sum(na.omit(scr_out$nocr$Wtmeasured))/ nrow(scr_out$nocr) *100
wtmeasuredtable <- as.table(rbind(c(sum(na.omit(scr_out$nocr$Wtmeasured)),
                                    sum(cr_first$Wtmeasured)),
                                  c(nrow(scr_out$nocr)-sum(is.na(scr_out$nocr$Wtmeasured))-
                                      sum(na.omit(scr_out$nocr$Wtmeasured)),
                                    nrow(cr_first)-sum(cr_first$Wtmeasured))))
dimnames(wtmeasuredtable) <- list(c("Measured", "Estimated"), c("no cr", "cr"))
wtmeasuredtable
chisq.test(wtmeasuredtable, correct = FALSE)
#mech vent
sum(cr_first$Mecvenadm)
sum(is.na(cr_first$Mecvenadm))
sum(cr_first$Mecvenadm) / nrow(cr_first) *100
sum(scr_out$nocr$Mecvenadm)
sum(is.na(scr_out$nocr$Mecvenadm))
sum(scr_out$nocr$Mecvenadm) / nrow(scr_out$nocr) *100
mvtable <- as.table(rbind(c(sum(scr_out$nocr$Mecvenadm),
                            sum(cr_first$Mecvenadm)),
                          c(nrow(scr_out$nocr)-sum(scr_out$nocr$Mecvenadm),
                            nrow(cr_first)-sum(cr_first$Mecvenadm))))
dimnames(mvtable) <- list(c("MV", "Not MV"), c("no cr", "cr"))
mvtable
chisq.test(mvtable, correct = FALSE)
#APACHE II
summary(cr_first$APACHE_II)
shapiro.test(cr_first$APACHE_II)
summary(scr_out$nocr$APACHE_II)
shapiro.test(scr_out$nocr$APACHE_II)
wilcox.test(cr_first$APACHE_II, scr_out$nocr$APACHE_II)
#APACHE III
summary(cr_first$APACHE_III)
shapiro.test(cr_first$APACHE_III)
summary(scr_out$nocr$APACHE_III)
shapiro.test(scr_out$nocr$APACHE_III)
wilcox.test(cr_first$APACHE_III, scr_out$nocr$APACHE_III)
#surg vs med adm
sum(cr_first$Surgadmission)
sum(is.na(cr_first$Surgadmission))
sum(cr_first$Surgadmission) / nrow(cr_first) *100
nrow(cr_first) - sum(cr_first$Surgadmission)
(nrow(cr_first) - sum(cr_first$Surgadmission))/nrow(cr_first) *100
sum(scr_out$nocr$Surgadmission)
sum(is.na(scr_out$nocr$Surgadmission))
sum(scr_out$nocr$Surgadmission) / nrow(scr_out$nocr) *100
nrow(scr_out$nocr) - sum(scr_out$nocr$Surgadmission)
(nrow(scr_out$nocr) - sum(scr_out$nocr$Surgadmission))/nrow(scr_out$nocr) *100
surgadmtable <- as.table(rbind(c(sum(scr_out$nocr$Surgadmission),
                                 sum(cr_first$Surgadmission)),
                               c(nrow(scr_out$nocr)-sum(scr_out$nocr$Surgadmission),
                                 nrow(cr_first)-sum(cr_first$Surgadmission))))
dimnames(surgadmtable) <- list(c("Surgical", "Medical"), c("no cr", "cr"))
surgadmtable
chisq.test(surgadmtable, correct = FALSE)
#HOPC ICU admission
sum(cr_first$PCm_cardio)
sum(cr_first$PCm_resp)
sum(cr_first$PCm_GI)
sum(cr_first$PCm_neuro)
sum(cr_first$PCm_sepsis)
sum(cr_first$PCm_trauma)
sum(cr_first$PCm_metabolic)
sum(cr_first$PCm_Haem)
sum(cr_first$PCm_renal)
sum(cr_first$PCm_other)
sum(cr_first$PCm_msk)
sum(cr_first$PCm_cardio) +
  sum(cr_first$PCm_resp) +
  sum(cr_first$PCm_GI) +
  sum(cr_first$PCm_neuro) +
  sum(cr_first$PCm_sepsis) +
  sum(cr_first$PCm_trauma) +
  sum(cr_first$PCm_metabolic) +
  sum(cr_first$PCm_Haem) +
  sum(cr_first$PCm_renal) +
  sum(cr_first$PCm_other) +
  sum(cr_first$PCm_msk)
nrow(cr_first) - sum(cr_first$Surgadmission)
sum(cr_first$PCs_cardio)
sum(cr_first$PCs_resp)
sum(cr_first$PCs_GI)
sum(cr_first$PCs_neuro)
sum(cr_first$PCs_trauma)
sum(cr_first$PCs_renal)
sum(cr_first$PCs_gynae)
sum(cr_first$PCs_msk)
sum(cr_first$PCs_haem)
sum(cr_first$PCs_metabolic)
sum(cr_first$PCs_cardio) +
  sum(cr_first$PCs_resp) +
  sum(cr_first$PCs_GI) +
  sum(cr_first$PCs_neuro) +
  sum(cr_first$PCs_trauma) +
  sum(cr_first$PCs_renal) +
  sum(cr_first$PCs_gynae) +
  sum(cr_first$PCs_msk) +
  sum(cr_first$PCs_haem) +
  sum(cr_first$PCs_metabolic)
sum(cr_first$Surgadmission)
sum(scr_out$nocr$PCm_cardio)
sum(scr_out$nocr$PCm_resp)
sum(scr_out$nocr$PCm_GI)
sum(scr_out$nocr$PCm_neuro)
sum(scr_out$nocr$PCm_sepsis)
sum(scr_out$nocr$PCm_trauma)
sum(scr_out$nocr$PCm_metabolic)
sum(scr_out$nocr$PCm_Haem)
sum(scr_out$nocr$PCm_renal)
sum(scr_out$nocr$PCm_other)
sum(scr_out$nocr$PCm_msk)
sum(scr_out$nocr$PCm_cardio) +
  sum(scr_out$nocr$PCm_resp) +
  sum(scr_out$nocr$PCm_GI) +
  sum(scr_out$nocr$PCm_neuro) +
  sum(scr_out$nocr$PCm_sepsis) +
  sum(scr_out$nocr$PCm_trauma) +
  sum(scr_out$nocr$PCm_metabolic) +
  sum(scr_out$nocr$PCm_Haem) +
  sum(scr_out$nocr$PCm_renal) +
  sum(scr_out$nocr$PCm_other) +
  sum(scr_out$nocr$PCm_msk)
nrow(scr_out$nocr) - sum(scr_out$nocr$Surgadmission)
sum(scr_out$nocr$PCs_cardio)
sum(scr_out$nocr$PCs_resp)
sum(scr_out$nocr$PCs_GI)
sum(scr_out$nocr$PCs_neuro)
sum(scr_out$nocr$PCs_trauma)
sum(scr_out$nocr$PCs_renal)
sum(scr_out$nocr$PCs_gynae)
sum(scr_out$nocr$PCs_msk)
sum(scr_out$nocr$PCs_haem)
sum(scr_out$nocr$PCs_metabolic)
sum(scr_out$nocr$PCs_cardio) +
  sum(scr_out$nocr$PCs_resp) +
  sum(scr_out$nocr$PCs_GI) +
  sum(scr_out$nocr$PCs_neuro) +
  sum(scr_out$nocr$PCs_trauma) +
  sum(scr_out$nocr$PCs_renal) +
  sum(scr_out$nocr$PCs_gynae) +
  sum(scr_out$nocr$PCs_msk) +
  sum(scr_out$nocr$PCs_haem) +
  sum(scr_out$nocr$PCs_metabolic)
sum(scr_out$nocr$Surgadmission)
PCtable <- as.table(rbind(c(sum(scr_out$nocr$PCm_cardio),
                            sum(cr_first$PCm_cardio)),
                          c(nrow(scr_out$nocr) - sum(scr_out$nocr$PCm_cardio),
                            nrow(cr_first) - sum(cr_first$PCm_cardio))))
dimnames(PCtable) <- list(c("mCardio", "other"), c("nocr", "cr"))
PCtable
chisq.test(PCtable, correct = FALSE)
PCtable <- as.table(rbind(c(sum(scr_out$nocr$PCm_resp),
                            sum(cr_first$PCm_resp)),
                          c(nrow(scr_out$nocr) - sum(scr_out$nocr$PCm_resp),
                            nrow(cr_first) - sum(cr_first$PCm_resp))))
dimnames(PCtable) <- list(c("mResp", "other"), c("nocr", "cr"))
PCtable
chisq.test(PCtable, correct = FALSE)
PCtable <- as.table(rbind(c(sum(scr_out$nocr$PCm_GI),
                            sum(cr_first$PCm_GI)),
                          c(nrow(scr_out$nocr) - sum(scr_out$nocr$PCm_GI),
                            nrow(cr_first) - sum(cr_first$PCm_GI))))
dimnames(PCtable) <- list(c("mGI", "other"), c("nocr", "cr"))
PCtable
fisher.test(PCtable)
PCtable <- as.table(rbind(c(sum(scr_out$nocr$PCm_neuro),
                            sum(cr_first$PCm_neuro)),
                          c(nrow(scr_out$nocr) - sum(scr_out$nocr$PCm_neuro),
                            nrow(cr_first) - sum(cr_first$PCm_neuro))))
dimnames(PCtable) <- list(c("mNeuro", "other"), c("nocr", "cr"))
PCtable
chisq.test(PCtable, correct = FALSE)
PCtable <- as.table(rbind(c(sum(scr_out$nocr$PCm_sepsis),
                            sum(cr_first$PCm_sepsis)),
                          c(nrow(scr_out$nocr) - sum(scr_out$nocr$PCm_sepsis),
                            nrow(cr_first) - sum(cr_first$PCm_sepsis))))
dimnames(PCtable) <- list(c("mSepsis", "other"), c("nocr", "cr"))
PCtable
chisq.test(PCtable, correct = FALSE)
PCtable <- as.table(rbind(c(sum(scr_out$nocr$PCm_trauma),
                            sum(cr_first$PCm_trauma)),
                          c(nrow(scr_out$nocr) - sum(scr_out$nocr$PCm_trauma),
                            nrow(cr_first) - sum(cr_first$PCm_trauma))))
dimnames(PCtable) <- list(c("mTrauma", "other"), c("nocr", "cr"))
PCtable
fisher.test(PCtable)
PCtable <- as.table(rbind(c(sum(scr_out$nocr$PCm_metabolic),
                            sum(cr_first$PCm_metabolic)),
                          c(nrow(scr_out$nocr) - sum(scr_out$nocr$PCm_metabolic),
                            nrow(cr_first) - sum(cr_first$PCm_metabolic))))
dimnames(PCtable) <- list(c("mMetabolic", "other"), c("nocr", "cr"))
PCtable
chisq.test(PCtable, correct = FALSE)
PCtable <- as.table(rbind(c(sum(scr_out$nocr$PCm_renal),
                            sum(cr_first$PCm_renal)),
                          c(nrow(scr_out$nocr) - sum(scr_out$nocr$PCm_renal),
                            nrow(cr_first) - sum(cr_first$PCm_renal))))
dimnames(PCtable) <- list(c("mRenal", "other"), c("nocr", "cr"))
PCtable
fisher.test(PCtable)
PCtable <- as.table(rbind(c(sum(scr_out$nocr$PCs_cardio),
                            sum(cr_first$PCs_cardio)),
                          c(nrow(scr_out$nocr) - sum(scr_out$nocr$PCs_cardio),
                            nrow(cr_first) - sum(cr_first$PCs_cardio))))
dimnames(PCtable) <- list(c("sCardio", "other"), c("nocr", "cr"))
PCtable
chisq.test(PCtable, correct = FALSE)
PCtable <- as.table(rbind(c(sum(scr_out$nocr$PCs_resp),
                            sum(cr_first$PCs_resp)),
                          c(nrow(scr_out$nocr) - sum(scr_out$nocr$PCs_resp),
                            nrow(cr_first) - sum(cr_first$PCs_resp))))
dimnames(PCtable) <- list(c("sResp", "other"), c("nocr", "cr"))
PCtable
chisq.test(PCtable, correct = FALSE)
PCtable <- as.table(rbind(c(sum(scr_out$nocr$PCs_GI),
                            sum(cr_first$PCs_GI)),
                          c(nrow(scr_out$nocr) - sum(scr_out$nocr$PCs_GI),
                            nrow(cr_first) - sum(cr_first$PCs_GI))))
dimnames(PCtable) <- list(c("sGI", "other"), c("nocr", "cr"))
PCtable
chisq.test(PCtable, correct = FALSE)
PCtable <- as.table(rbind(c(sum(scr_out$nocr$PCs_neuro),
                            sum(cr_first$PCs_neuro)),
                          c(nrow(scr_out$nocr) - sum(scr_out$nocr$PCs_neuro),
                            nrow(cr_first) - sum(cr_first$PCs_neuro))))
dimnames(PCtable) <- list(c("sNeuro", "other"), c("nocr", "cr"))
PCtable
chisq.test(PCtable, correct = FALSE)
PCtable <- as.table(rbind(c(sum(scr_out$nocr$PCs_renal),
                            sum(cr_first$PCs_renal)),
                          c(nrow(scr_out$nocr) - sum(scr_out$nocr$PCs_renal),
                            nrow(cr_first) - sum(cr_first$PCs_renal))))
dimnames(PCtable) <- list(c("sRenal", "other"), c("nocr", "cr"))
PCtable
fisher.test(PCtable)
PCtable <- as.table(rbind(c(sum(scr_out$nocr$PCs_gynae),
                            sum(cr_first$PCs_gynae)),
                          c(nrow(scr_out$nocr) - sum(scr_out$nocr$PCs_gynae),
                            nrow(cr_first) - sum(cr_first$PCs_gynae))))
dimnames(PCtable) <- list(c("sGynae", "other"), c("nocr", "cr"))
PCtable
fisher.test(PCtable)
PCtable <- as.table(rbind(c(sum(scr_out$nocr$PCs_msk),
                            sum(cr_first$PCs_msk)),
                          c(nrow(scr_out$nocr) - sum(scr_out$nocr$PCs_msk),
                            nrow(cr_first) - sum(cr_first$PCs_msk))))
dimnames(PCtable) <- list(c("sMSK", "other"), c("nocr", "cr"))
PCtable
chisq.test(PCtable, correct = FALSE)
PCtable <- as.table(rbind(c(sum(scr_out$nocr$PCs_metabolic),
                            sum(cr_first$PCs_metabolic)),
                          c(nrow(scr_out$nocr) - sum(scr_out$nocr$PCs_metabolic),
                            nrow(cr_first) - sum(cr_first$PCs_metabolic))))
dimnames(PCtable) <- list(c("sMetabolic", "other"), c("nocr", "cr"))
PCtable
fisher.test(PCtable)
#demographics - compare those with oli ep to those without 165 v 222 ----
#Age
shapiro.test(oli_first$Age)
summary(oli_first$Age)
shapiro.test(scr_out$nool$Age)
summary(scr_out$nool$Age)
wilcox.test(oli_first$Age, scr_out$nool$Age)
#Male
sum(oli_first$Male)
sum(is.na(oli_first$Male))
sum(oli_first$Male)/nrow(oli_first) *100
sum(scr_out$nool$Male)
sum(is.na(scr_out$nool$Male))
sum(scr_out$nool$Male)/nrow(scr_out$nool) *100
gendertable <- as.table(rbind(c(sum(scr_out$nool$Male),
                                sum(oli_first$Male)),
                              c(nrow(scr_out$nool)-sum(scr_out$nool$Male),
                                nrow(oli_first)-sum(oli_first$Male))))
dimnames(gendertable) <- list(c("M", "F"), c("no oli", "oli"))
gendertable
chisq.test(gendertable, correct = FALSE)
#Weight
shapiro.test(oli_first$Wt)
summary(oli_first$Wt)
shapiro.test(scr_out$nool$Wt)
summary(scr_out$nool$Wt)
wilcox.test(oli_first$Wt, scr_out$nool$Wt)
#weight measured
sum(oli_first$Wtmeasured)
sum(is.na(oli_first$Wtmeasured))
sum(oli_first$Wtmeasured) / nrow(oli_first) *100
sum(na.omit(scr_out$nool$Wtmeasured))
sum(is.na(scr_out$nool$Wtmeasured))
sum(na.omit(scr_out$nool$Wtmeasured))/ nrow(scr_out$nool) *100
wtmeasuredtable <- as.table(rbind(c(sum(na.omit(scr_out$nool$Wtmeasured)),
                                    sum(oli_first$Wtmeasured)),
                                  c(nrow(scr_out$nool)-sum(is.na(scr_out$nool$Wtmeasured))-
                                      sum(na.omit(scr_out$nool$Wtmeasured)),
                                    nrow(oli_first)-sum(oli_first$Wtmeasured))))
dimnames(wtmeasuredtable) <- list(c("Measured", "Estimated"), c("no oli", "oli"))
wtmeasuredtable
chisq.test(wtmeasuredtable, correct = FALSE)
#mech vent
sum(oli_first$Mecvenadm)
sum(is.na(oli_first$Mecvenadm))
sum(oli_first$Mecvenadm) / nrow(oli_first) *100
sum(scr_out$nool$Mecvenadm)
sum(is.na(scr_out$nool$Mecvenadm))
sum(scr_out$nool$Mecvenadm) / nrow(scr_out$nool) *100
mvtable <- as.table(rbind(c(sum(scr_out$nool$Mecvenadm),
                            sum(oli_first$Mecvenadm)),
                          c(nrow(scr_out$nool)-sum(scr_out$nool$Mecvenadm),
                            nrow(oli_first)-sum(oli_first$Mecvenadm))))
dimnames(mvtable) <- list(c("MV", "Not MV"), c("no oli", "oli"))
mvtable
chisq.test(mvtable, correct = FALSE)
#APACHE II
summary(oli_first$APACHE_II)
shapiro.test(oli_first$APACHE_II)
summary(scr_out$nool$APACHE_II)
shapiro.test(scr_out$nool$APACHE_II)
wilcox.test(oli_first$APACHE_II, scr_out$nool$APACHE_II)
#APACHE III
summary(oli_first$APACHE_III)
shapiro.test(oli_first$APACHE_III)
summary(scr_out$nool$APACHE_III)
shapiro.test(scr_out$nool$APACHE_III)
wilcox.test(oli_first$APACHE_III, scr_out$nool$APACHE_III)
#surg vs med adm
sum(oli_first$Surgadmission)
sum(is.na(oli_first$Surgadmission))
sum(oli_first$Surgadmission) / nrow(oli_first) *100
nrow(oli_first) - sum(oli_first$Surgadmission)
(nrow(oli_first) - sum(oli_first$Surgadmission))/nrow(oli_first) *100
sum(scr_out$nool$Surgadmission)
sum(is.na(scr_out$nool$Surgadmission))
sum(scr_out$nool$Surgadmission) / nrow(scr_out$nool) *100
nrow(scr_out$nool) - sum(scr_out$nool$Surgadmission)
(nrow(scr_out$nool) - sum(scr_out$nool$Surgadmission))/nrow(scr_out$nool) *100
surgadmtable <- as.table(rbind(c(sum(scr_out$nool$Surgadmission),
                                 sum(oli_first$Surgadmission)),
                               c(nrow(scr_out$nool)-sum(scr_out$nool$Surgadmission),
                                 nrow(oli_first)-sum(oli_first$Surgadmission))))
dimnames(surgadmtable) <- list(c("Surgical", "Medical"), c("no oli", "oli"))
surgadmtable
chisq.test(surgadmtable, correct = FALSE)
#HOPC ICU admission
sum(oli_first$PCm_cardio)
sum(oli_first$PCm_resp)
sum(oli_first$PCm_GI)
sum(oli_first$PCm_neuro)
sum(oli_first$PCm_sepsis)
sum(oli_first$PCm_trauma)
sum(oli_first$PCm_metabolic)
sum(oli_first$PCm_Haem)
sum(oli_first$PCm_renal)
sum(oli_first$PCm_other)
sum(oli_first$PCm_msk)
sum(oli_first$PCm_cardio) +
  sum(oli_first$PCm_resp) +
  sum(oli_first$PCm_GI) +
  sum(oli_first$PCm_neuro) +
  sum(oli_first$PCm_sepsis) +
  sum(oli_first$PCm_trauma) +
  sum(oli_first$PCm_metabolic) +
  sum(oli_first$PCm_Haem) +
  sum(oli_first$PCm_renal) +
  sum(oli_first$PCm_other) +
  sum(oli_first$PCm_msk)
nrow(oli_first) - sum(oli_first$Surgadmission)
sum(oli_first$PCs_cardio)
sum(oli_first$PCs_resp)
sum(oli_first$PCs_GI)
sum(oli_first$PCs_neuro)
sum(oli_first$PCs_trauma)
sum(oli_first$PCs_renal)
sum(oli_first$PCs_gynae)
sum(oli_first$PCs_msk)
sum(oli_first$PCs_haem)
sum(oli_first$PCs_metabolic)
sum(oli_first$PCs_cardio) +
  sum(oli_first$PCs_resp) +
  sum(oli_first$PCs_GI) +
  sum(oli_first$PCs_neuro) +
  sum(oli_first$PCs_trauma) +
  sum(oli_first$PCs_renal) +
  sum(oli_first$PCs_gynae) +
  sum(oli_first$PCs_msk) +
  sum(oli_first$PCs_haem) +
  sum(oli_first$PCs_metabolic)
sum(oli_first$Surgadmission)
sum(scr_out$nool$PCm_cardio)
sum(scr_out$nool$PCm_resp)
sum(scr_out$nool$PCm_GI)
sum(scr_out$nool$PCm_neuro)
sum(scr_out$nool$PCm_sepsis)
sum(scr_out$nool$PCm_trauma)
sum(scr_out$nool$PCm_metabolic)
sum(scr_out$nool$PCm_Haem)
sum(scr_out$nool$PCm_renal)
sum(scr_out$nool$PCm_other)
sum(scr_out$nool$PCm_msk)
sum(scr_out$nool$PCm_cardio) +
  sum(scr_out$nool$PCm_resp) +
  sum(scr_out$nool$PCm_GI) +
  sum(scr_out$nool$PCm_neuro) +
  sum(scr_out$nool$PCm_sepsis) +
  sum(scr_out$nool$PCm_trauma) +
  sum(scr_out$nool$PCm_metabolic) +
  sum(scr_out$nool$PCm_Haem) +
  sum(scr_out$nool$PCm_renal) +
  sum(scr_out$nool$PCm_other) +
  sum(scr_out$nool$PCm_msk)
nrow(scr_out$nool) - sum(scr_out$nool$Surgadmission)
sum(scr_out$nool$PCs_cardio)
sum(scr_out$nool$PCs_resp)
sum(scr_out$nool$PCs_GI)
sum(scr_out$nool$PCs_neuro)
sum(scr_out$nool$PCs_trauma)
sum(scr_out$nool$PCs_renal)
sum(scr_out$nool$PCs_gynae)
sum(scr_out$nool$PCs_msk)
sum(scr_out$nool$PCs_haem)
sum(scr_out$nool$PCs_metabolic)
sum(scr_out$nool$PCs_cardio) +
  sum(scr_out$nool$PCs_resp) +
  sum(scr_out$nool$PCs_GI) +
  sum(scr_out$nool$PCs_neuro) +
  sum(scr_out$nool$PCs_trauma) +
  sum(scr_out$nool$PCs_renal) +
  sum(scr_out$nool$PCs_gynae) +
  sum(scr_out$nool$PCs_msk) +
  sum(scr_out$nool$PCs_haem) +
  sum(scr_out$nool$PCs_metabolic)
sum(scr_out$nool$Surgadmission)
PCtable <- as.table(rbind(c(sum(scr_out$nool$PCm_cardio),
                            sum(oli_first$PCm_cardio)),
                          c(nrow(scr_out$nool) - sum(scr_out$nool$PCm_cardio),
                            nrow(oli_first) - sum(oli_first$PCm_cardio))))
dimnames(PCtable) <- list(c("mCardio", "other"), c("nool", "oli"))
PCtable
chisq.test(PCtable, correct = FALSE)
PCtable <- as.table(rbind(c(sum(scr_out$nool$PCm_resp),
                            sum(oli_first$PCm_resp)),
                          c(nrow(scr_out$nool) - sum(scr_out$nool$PCm_resp),
                            nrow(oli_first) - sum(oli_first$PCm_resp))))
dimnames(PCtable) <- list(c("mResp", "other"), c("nool", "oli"))
PCtable
chisq.test(PCtable, correct = FALSE)
PCtable <- as.table(rbind(c(sum(scr_out$nool$PCm_GI),
                            sum(oli_first$PCm_GI)),
                          c(nrow(scr_out$nool) - sum(scr_out$nool$PCm_GI),
                            nrow(oli_first) - sum(oli_first$PCm_GI))))
dimnames(PCtable) <- list(c("mGI", "other"), c("nool", "oli"))
PCtable
fisher.test(PCtable)
PCtable <- as.table(rbind(c(sum(scr_out$nool$PCm_neuro),
                            sum(oli_first$PCm_neuro)),
                          c(nrow(scr_out$nool) - sum(scr_out$nool$PCm_neuro),
                            nrow(oli_first) - sum(oli_first$PCm_neuro))))
dimnames(PCtable) <- list(c("mNeuro", "other"), c("nool", "oli"))
PCtable
chisq.test(PCtable, correct = FALSE)
PCtable <- as.table(rbind(c(sum(scr_out$nool$PCm_sepsis),
                            sum(oli_first$PCm_sepsis)),
                          c(nrow(scr_out$nool) - sum(scr_out$nool$PCm_sepsis),
                            nrow(oli_first) - sum(oli_first$PCm_sepsis))))
dimnames(PCtable) <- list(c("mSepsis", "other"), c("nool", "oli"))
PCtable
chisq.test(PCtable, correct = FALSE)
PCtable <- as.table(rbind(c(sum(scr_out$nool$PCm_trauma),
                            sum(oli_first$PCm_trauma)),
                          c(nrow(scr_out$nool) - sum(scr_out$nool$PCm_trauma),
                            nrow(oli_first) - sum(oli_first$PCm_trauma))))
dimnames(PCtable) <- list(c("mTrauma", "other"), c("nool", "oli"))
PCtable
fisher.test(PCtable)
PCtable <- as.table(rbind(c(sum(scr_out$nool$PCm_metabolic),
                            sum(oli_first$PCm_metabolic)),
                          c(nrow(scr_out$nool) - sum(scr_out$nool$PCm_metabolic),
                            nrow(oli_first) - sum(oli_first$PCm_metabolic))))
dimnames(PCtable) <- list(c("mMetabolic", "other"), c("nool", "oli"))
PCtable
fisher.test(PCtable)
PCtable <- as.table(rbind(c(sum(scr_out$nool$PCm_renal),
                            sum(oli_first$PCm_renal)),
                          c(nrow(scr_out$nool) - sum(scr_out$nool$PCm_renal),
                            nrow(oli_first) - sum(oli_first$PCm_renal))))
dimnames(PCtable) <- list(c("mRenal", "other"), c("nool", "oli"))
PCtable
fisher.test(PCtable)
PCtable <- as.table(rbind(c(sum(scr_out$nool$PCs_cardio),
                            sum(oli_first$PCs_cardio)),
                          c(nrow(scr_out$nool) - sum(scr_out$nool$PCs_cardio),
                            nrow(oli_first) - sum(oli_first$PCs_cardio))))
dimnames(PCtable) <- list(c("sCardio", "other"), c("nool", "oli"))
PCtable
chisq.test(PCtable, correct = FALSE)
PCtable <- as.table(rbind(c(sum(scr_out$nool$PCs_resp),
                            sum(oli_first$PCs_resp)),
                          c(nrow(scr_out$nool) - sum(scr_out$nool$PCs_resp),
                            nrow(oli_first) - sum(oli_first$PCs_resp))))
dimnames(PCtable) <- list(c("sResp", "other"), c("nool", "oli"))
PCtable
chisq.test(PCtable, correct = FALSE)
PCtable <- as.table(rbind(c(sum(scr_out$nool$PCs_GI),
                            sum(oli_first$PCs_GI)),
                          c(nrow(scr_out$nool) - sum(scr_out$nool$PCs_GI),
                            nrow(oli_first) - sum(oli_first$PCs_GI))))
dimnames(PCtable) <- list(c("sGI", "other"), c("nool", "oli"))
PCtable
chisq.test(PCtable, correct = FALSE)
PCtable <- as.table(rbind(c(sum(scr_out$nool$PCs_neuro),
                            sum(oli_first$PCs_neuro)),
                          c(nrow(scr_out$nool) - sum(scr_out$nool$PCs_neuro),
                            nrow(oli_first) - sum(oli_first$PCs_neuro))))
dimnames(PCtable) <- list(c("sNeuro", "other"), c("nool", "oli"))
PCtable
fisher.test(PCtable)
PCtable <- as.table(rbind(c(sum(scr_out$nool$PCs_renal),
                            sum(oli_first$PCs_renal)),
                          c(nrow(scr_out$nool) - sum(scr_out$nool$PCs_renal),
                            nrow(oli_first) - sum(oli_first$PCs_renal))))
dimnames(PCtable) <- list(c("sRenal", "other"), c("nool", "oli"))
PCtable
fisher.test(PCtable)
PCtable <- as.table(rbind(c(sum(scr_out$nool$PCs_gynae),
                            sum(oli_first$PCs_gynae)),
                          c(nrow(scr_out$nool) - sum(scr_out$nool$PCs_gynae),
                            nrow(oli_first) - sum(oli_first$PCs_gynae))))
dimnames(PCtable) <- list(c("sGynae", "other"), c("nool", "oli"))
PCtable
fisher.test(PCtable)
PCtable <- as.table(rbind(c(sum(scr_out$nool$PCs_msk),
                            sum(oli_first$PCs_msk)),
                          c(nrow(scr_out$nool) - sum(scr_out$nool$PCs_msk),
                            nrow(oli_first) - sum(oli_first$PCs_msk))))
dimnames(PCtable) <- list(c("sMSK", "other"), c("nool", "oli"))
PCtable
chisq.test(PCtable, correct = FALSE)
PCtable <- as.table(rbind(c(sum(scr_out$nool$PCs_metabolic),
                            sum(oli_first$PCs_metabolic)),
                          c(nrow(scr_out$nool) - sum(scr_out$nool$PCs_metabolic),
                            nrow(oli_first) - sum(oli_first$PCs_metabolic))))
dimnames(PCtable) <- list(c("sMetabolic", "other"), c("nool", "oli"))
PCtable
fisher.test(PCtable)

#demographics - Compare those with cr ep who did and didn't get AKI 185 v 94 ----
#age
summary(cr_first_AKI$Age)
summary(cr_first_noAKI$Age)
shapiro.test(cr_first_AKI$Age)
shapiro.test(cr_first_noAKI$Age)
wilcox.test(cr_first_AKI$Age, cr_first_noAKI$Age)
#male
sum(cr_first_AKI$Male)
sum(is.na(cr_first_AKI$Male))
sum(cr_first_AKI$Male)/nrow(cr_first_AKI) *100
sum(cr_first_noAKI$Male)
gendertable <- as.table(rbind(c(sum(cr_first_AKI$Male),
                                sum(cr_first_noAKI$Male)),
                              c(nrow(cr_first_AKI)-sum(cr_first_AKI$Male),
                                nrow(cr_first_noAKI)-sum(cr_first_noAKI$Male))))
dimnames(gendertable) <- list(c("M", "F"), c("AKI", "no AKI"))
gendertable
chisq.test(gendertable, correct = FALSE)
#weight
summary(cr_first_AKI$Wt)
summary(cr_first_noAKI$Wt)
shapiro.test(cr_first_AKI$Wt)
shapiro.test(cr_first_noAKI$Wt)
wilcox.test(cr_first_AKI$Wt, cr_first_noAKI$Wt)
#weight measured
sum(cr_first_AKI$Wtmeasured)
sum(is.na(cr_first_AKI$Wtmeasured))
sum(cr_first_AKI$Wtmeasured)/nrow(cr_first_AKI) *100
sum(cr_first_noAKI$Wtmeasured)
wtmeasuredtable <- as.table(rbind(c(sum(na.omit(cr_first_AKI$Wtmeasured)),
                                    sum(cr_first_noAKI$Wtmeasured)),
                                  c(nrow(cr_first_AKI)- sum(na.omit(cr_first_AKI$Wtmeasured)),
                                    nrow(cr_first_noAKI)-sum(cr_first_noAKI$Wtmeasured))))
dimnames(wtmeasuredtable) <- list(c("Measured", "Estimated"), c("AKI", "no AKI"))
wtmeasuredtable
chisq.test(wtmeasuredtable, correct = FALSE)
#mech vent
sum(cr_first_AKI$Mecvenadm)
sum(is.na(cr_first_AKI$Mecvenadm))
sum(cr_first_AKI$Mecvenadm) / nrow(cr_first_AKI) *100
sum(cr_first_noAKI$Mecvenadm)
mvtable <- as.table(rbind(c(sum(cr_first_AKI$Mecvenadm),
                            sum(cr_first_noAKI$Mecvenadm)),
                          c(nrow(cr_first_AKI)-sum(cr_first_AKI$Mecvenadm),
                            nrow(cr_first_noAKI)-sum(cr_first_noAKI$Mecvenadm))))
dimnames(mvtable) <- list(c("MV", "Not MV"), c("AKI", "no AKI"))
mvtable
chisq.test(mvtable, correct = FALSE)
#APACHE II
summary(cr_first_AKI$APACHE_II)
summary(cr_first_noAKI$APACHE_II)
shapiro.test(cr_first_AKI$APACHE_II)
shapiro.test(cr_first_noAKI$APACHE_II)
wilcox.test(cr_first_AKI$APACHE_II, cr_first_noAKI$APACHE_II)
#APACHE III
summary(cr_first_AKI$APACHE_III)
summary(cr_first_noAKI$APACHE_III)
shapiro.test(cr_first_AKI$APACHE_III)
shapiro.test(cr_first_noAKI$APACHE_III)
wilcox.test(cr_first_AKI$APACHE_III, cr_first_noAKI$APACHE_III)
#surgadm
sum(cr_first_AKI$Surgadmission)
sum(is.na(cr_first_AKI$Surgadmission))
sum(cr_first_AKI$Surgadmission)/nrow(cr_first_AKI) *100
nrow(cr_first_AKI) - sum(cr_first_AKI$Surgadmission)
(nrow(cr_first_AKI) - sum(cr_first_AKI$Surgadmission)) /nrow(cr_first_AKI) *100
sum(cr_first_noAKI$Surgadmission)
surgadmtable <- as.table(rbind(c(sum(cr_first_AKI$Surgadmission),
                                 sum(cr_first_noAKI$Surgadmission)),
                               c(nrow(cr_first_AKI)-sum(cr_first_AKI$Surgadmission),
                                 nrow(cr_first_noAKI)-sum(cr_first_noAKI$Surgadmission))))
dimnames(surgadmtable) <- list(c("Surgical", "Medical"), c("AKI", "no AKI"))
surgadmtable
chisq.test(surgadmtable, correct = FALSE)
#HOPC ICU admission
sum(cr_first_AKI$PCm_cardio)
sum(cr_first_AKI$PCm_resp)
sum(cr_first_AKI$PCm_GI)
sum(cr_first_AKI$PCm_neuro)
sum(cr_first_AKI$PCm_sepsis)
sum(cr_first_AKI$PCm_trauma)
sum(cr_first_AKI$PCm_metabolic)
sum(cr_first_AKI$PCm_Haem)
sum(cr_first_AKI$PCm_renal)
sum(cr_first_AKI$PCm_other)
sum(cr_first_AKI$PCm_msk)
sum(cr_first_AKI$PCm_cardio) +
  sum(cr_first_AKI$PCm_resp) +
  sum(cr_first_AKI$PCm_GI) +
  sum(cr_first_AKI$PCm_neuro) +
  sum(cr_first_AKI$PCm_sepsis) +
  sum(cr_first_AKI$PCm_trauma) +
  sum(cr_first_AKI$PCm_metabolic) +
  sum(cr_first_AKI$PCm_Haem) +
  sum(cr_first_AKI$PCm_renal) +
  sum(cr_first_AKI$PCm_other) +
  sum(cr_first_AKI$PCm_msk)
nrow(cr_first_AKI) - sum(cr_first_AKI$Surgadmission)
sum(cr_first_AKI$PCs_cardio)
sum(cr_first_AKI$PCs_resp)
sum(cr_first_AKI$PCs_GI)
sum(cr_first_AKI$PCs_neuro)
sum(cr_first_AKI$PCs_trauma)
sum(cr_first_AKI$PCs_renal)
sum(cr_first_AKI$PCs_gynae)
sum(cr_first_AKI$PCs_msk)
sum(cr_first_AKI$PCs_haem)
sum(cr_first_AKI$PCs_metabolic)
sum(cr_first_AKI$PCs_cardio) +
  sum(cr_first_AKI$PCs_resp) +
  sum(cr_first_AKI$PCs_GI) +
  sum(cr_first_AKI$PCs_neuro) +
  sum(cr_first_AKI$PCs_trauma) +
  sum(cr_first_AKI$PCs_renal) +
  sum(cr_first_AKI$PCs_gynae) +
  sum(cr_first_AKI$PCs_msk) +
  sum(cr_first_AKI$PCs_haem) +
  sum(cr_first_AKI$PCs_metabolic)
sum(cr_first_AKI$Surgadmission)
sum(cr_first_noAKI$PCm_cardio)
sum(cr_first_noAKI$PCm_resp)
sum(cr_first_noAKI$PCm_GI)
sum(cr_first_noAKI$PCm_neuro)
sum(cr_first_noAKI$PCm_sepsis)
sum(cr_first_noAKI$PCm_trauma)
sum(cr_first_noAKI$PCm_metabolic)
sum(cr_first_noAKI$PCm_Haem)
sum(cr_first_noAKI$PCm_renal)
sum(cr_first_noAKI$PCm_other)
sum(cr_first_noAKI$PCm_msk)
sum(cr_first_noAKI$PCm_cardio) +
  sum(cr_first_noAKI$PCm_resp) +
  sum(cr_first_noAKI$PCm_GI) +
  sum(cr_first_noAKI$PCm_neuro) +
  sum(cr_first_noAKI$PCm_sepsis) +
  sum(cr_first_noAKI$PCm_trauma) +
  sum(cr_first_noAKI$PCm_metabolic) +
  sum(cr_first_noAKI$PCm_Haem) +
  sum(cr_first_noAKI$PCm_renal) +
  sum(cr_first_noAKI$PCm_other) +
  sum(cr_first_noAKI$PCm_msk)
nrow(cr_first_noAKI) - sum(cr_first_noAKI$Surgadmission)
sum(cr_first_noAKI$PCs_cardio)
sum(cr_first_noAKI$PCs_resp)
sum(cr_first_noAKI$PCs_GI)
sum(cr_first_noAKI$PCs_neuro)
sum(cr_first_noAKI$PCs_trauma)
sum(cr_first_noAKI$PCs_renal)
sum(cr_first_noAKI$PCs_gynae)
sum(cr_first_noAKI$PCs_msk)
sum(cr_first_noAKI$PCs_haem)
sum(cr_first_noAKI$PCs_metabolic)
sum(cr_first_noAKI$PCs_cardio) +
  sum(cr_first_noAKI$PCs_resp) +
  sum(cr_first_noAKI$PCs_GI) +
  sum(cr_first_noAKI$PCs_neuro) +
  sum(cr_first_noAKI$PCs_trauma) +
  sum(cr_first_noAKI$PCs_renal) +
  sum(cr_first_noAKI$PCs_gynae) +
  sum(cr_first_noAKI$PCs_msk) +
  sum(cr_first_noAKI$PCs_haem) +
  sum(cr_first_noAKI$PCs_metabolic)
sum(cr_first_noAKI$Surgadmission)
PCtable <- as.table(rbind(c(sum(cr_first_noAKI$PCm_cardio),
                            sum(cr_first_AKI$PCm_cardio)),
                          c(nrow(cr_first_noAKI) - sum(cr_first_noAKI$PCm_cardio),
                            nrow(cr_first_AKI) - sum(cr_first_AKI$PCm_cardio))))
dimnames(PCtable) <- list(c("mCardio", "other"), c("no AKI", "AKI"))
PCtable
chisq.test(PCtable, correct = FALSE)
PCtable <- as.table(rbind(c(sum(cr_first_noAKI$PCm_resp),
                            sum(cr_first_AKI$PCm_resp)),
                          c(nrow(cr_first_noAKI) - sum(cr_first_noAKI$PCm_resp),
                            nrow(cr_first_AKI) - sum(cr_first_AKI$PCm_resp))))
dimnames(PCtable) <- list(c("mResp", "other"), c("no AKI", "AKI"))
PCtable
chisq.test(PCtable, correct = FALSE)
PCtable <- as.table(rbind(c(sum(cr_first_noAKI$PCm_GI),
                            sum(cr_first_AKI$PCm_GI)),
                          c(nrow(cr_first_noAKI) - sum(cr_first_noAKI$PCm_GI),
                            nrow(cr_first_AKI) - sum(cr_first_AKI$PCm_GI))))
dimnames(PCtable) <- list(c("mGI", "other"), c("no AKI", "AKI"))
PCtable
fisher.test(PCtable)
PCtable <- as.table(rbind(c(sum(cr_first_noAKI$PCm_neuro),
                            sum(cr_first_AKI$PCm_neuro)),
                          c(nrow(cr_first_noAKI) - sum(cr_first_noAKI$PCm_neuro),
                            nrow(cr_first_AKI) - sum(cr_first_AKI$PCm_neuro))))
dimnames(PCtable) <- list(c("mNeuro", "other"), c("no AKI", "AKI"))
PCtable
chisq.test(PCtable, correct = FALSE)
PCtable <- as.table(rbind(c(sum(cr_first_noAKI$PCm_sepsis),
                            sum(cr_first_AKI$PCm_sepsis)),
                          c(nrow(cr_first_noAKI) - sum(cr_first_noAKI$PCm_sepsis),
                            nrow(cr_first_AKI) - sum(cr_first_AKI$PCm_sepsis))))
dimnames(PCtable) <- list(c("mSepsis", "other"), c("no AKI", "AKI"))
PCtable
fisher.test(PCtable)
PCtable <- as.table(rbind(c(sum(cr_first_noAKI$PCm_trauma),
                            sum(cr_first_AKI$PCm_trauma)),
                          c(nrow(cr_first_noAKI) - sum(cr_first_noAKI$PCm_trauma),
                            nrow(cr_first_AKI) - sum(cr_first_AKI$PCm_trauma))))
dimnames(PCtable) <- list(c("mTrauma", "other"), c("no AKI", "AKI"))
PCtable
fisher.test(PCtable)
PCtable <- as.table(rbind(c(sum(cr_first_noAKI$PCm_metabolic),
                            sum(cr_first_AKI$PCm_metabolic)),
                          c(nrow(cr_first_noAKI) - sum(cr_first_noAKI$PCm_metabolic),
                            nrow(cr_first_AKI) - sum(cr_first_AKI$PCm_metabolic))))
dimnames(PCtable) <- list(c("mMetabolic", "other"), c("no AKI", "AKI"))
PCtable
chisq.test(PCtable, correct = FALSE)
PCtable <- as.table(rbind(c(sum(cr_first_noAKI$PCm_renal),
                            sum(cr_first_AKI$PCm_renal)),
                          c(nrow(cr_first_noAKI) - sum(cr_first_noAKI$PCm_renal),
                            nrow(cr_first_AKI) - sum(cr_first_AKI$PCm_renal))))
dimnames(PCtable) <- list(c("mRenal", "other"), c("no AKI", "AKI"))
PCtable
fisher.test(PCtable)
PCtable <- as.table(rbind(c(sum(cr_first_noAKI$PCs_cardio),
                            sum(cr_first_AKI$PCs_cardio)),
                          c(nrow(cr_first_noAKI) - sum(cr_first_noAKI$PCs_cardio),
                            nrow(cr_first_AKI) - sum(cr_first_AKI$PCs_cardio))))
dimnames(PCtable) <- list(c("sCardio", "other"), c("no AKI", "AKI"))
PCtable
chisq.test(PCtable, correct = FALSE)
PCtable <- as.table(rbind(c(sum(cr_first_noAKI$PCs_resp),
                            sum(cr_first_AKI$PCs_resp)),
                          c(nrow(cr_first_noAKI) - sum(cr_first_noAKI$PCs_resp),
                            nrow(cr_first_AKI) - sum(cr_first_AKI$PCs_resp))))
dimnames(PCtable) <- list(c("sResp", "other"), c("no AKI", "AKI"))
PCtable
fisher.test(PCtable)
PCtable <- as.table(rbind(c(sum(cr_first_noAKI$PCs_GI),
                            sum(cr_first_AKI$PCs_GI)),
                          c(nrow(cr_first_noAKI) - sum(cr_first_noAKI$PCs_GI),
                            nrow(cr_first_AKI) - sum(cr_first_AKI$PCs_GI))))
dimnames(PCtable) <- list(c("sGI", "other"), c("no AKI", "AKI"))
PCtable
chisq.test(PCtable, correct = FALSE)
PCtable <- as.table(rbind(c(sum(cr_first_noAKI$PCs_neuro),
                            sum(cr_first_AKI$PCs_neuro)),
                          c(nrow(cr_first_noAKI) - sum(cr_first_noAKI$PCs_neuro),
                            nrow(cr_first_AKI) - sum(cr_first_AKI$PCs_neuro))))
dimnames(PCtable) <- list(c("sNeuro", "other"), c("no AKI", "AKI"))
PCtable
fisher.test(PCtable)
PCtable <- as.table(rbind(c(sum(cr_first_noAKI$PCs_renal),
                            sum(cr_first_AKI$PCs_renal)),
                          c(nrow(cr_first_noAKI) - sum(cr_first_noAKI$PCs_renal),
                            nrow(cr_first_AKI) - sum(cr_first_AKI$PCs_renal))))
dimnames(PCtable) <- list(c("sRenal", "other"), c("no AKI", "AKI"))
PCtable
fisher.test(PCtable)
PCtable <- as.table(rbind(c(sum(cr_first_noAKI$PCs_msk),
                            sum(cr_first_AKI$PCs_msk)),
                          c(nrow(cr_first_noAKI) - sum(cr_first_noAKI$PCs_msk),
                            nrow(cr_first_AKI) - sum(cr_first_AKI$PCs_msk))))
dimnames(PCtable) <- list(c("sMSK", "other"), c("no AKI", "AKI"))
PCtable
fisher.test(PCtable)

#demographics - compare those with oli ep who did and didn't get AKI 128 v 37  ----
#age
summary(oli_first_AKI$Age)
summary(oli_first_noAKI$Age)
shapiro.test(oli_first_AKI$Age)
shapiro.test(oli_first_noAKI$Age)
wilcox.test(oli_first_AKI$Age, oli_first_noAKI$Age)
#male
sum(oli_first_AKI$Male)
sum(is.na(oli_first_AKI$Male))
sum(oli_first_AKI$Male)/nrow(oli_first_AKI) *100
sum(oli_first_noAKI$Male)
gendertable <- as.table(rbind(c(sum(oli_first_AKI$Male),
                                sum(oli_first_noAKI$Male)),
                              c(nrow(oli_first_AKI)-sum(oli_first_AKI$Male),
                                nrow(oli_first_noAKI)-sum(oli_first_noAKI$Male))))
dimnames(gendertable) <- list(c("M", "F"), c("AKI", "no AKI"))
gendertable
chisq.test(gendertable, correct = FALSE)
#weight
summary(oli_first_AKI$Wt)
summary(oli_first_noAKI$Wt)
shapiro.test(oli_first_AKI$Wt)
shapiro.test(oli_first_noAKI$Wt)
wilcox.test(oli_first_AKI$Wt, oli_first_noAKI$Wt)
#weight measured
sum(oli_first_AKI$Wtmeasured)
sum(is.na(oli_first_AKI$Wtmeasured))
sum(oli_first_AKI$Wtmeasured)/nrow(oli_first_AKI) *100
sum(oli_first_noAKI$Wtmeasured)
wtmeasuredtable <- as.table(rbind(c(sum(na.omit(oli_first_AKI$Wtmeasured)),
                                    sum(oli_first_noAKI$Wtmeasured)),
                                  c(nrow(oli_first_AKI)- sum(na.omit(oli_first_AKI$Wtmeasured)),
                                    nrow(oli_first_noAKI)-sum(oli_first_noAKI$Wtmeasured))))
dimnames(wtmeasuredtable) <- list(c("Measured", "Estimated"), c("AKI", "no AKI"))
wtmeasuredtable
chisq.test(wtmeasuredtable, correct = FALSE)
#mech vent
sum(oli_first_AKI$Mecvenadm)
sum(is.na(oli_first_AKI$Mecvenadm))
sum(oli_first_AKI$Mecvenadm) / nrow(oli_first_AKI) *100
sum(oli_first_noAKI$Mecvenadm)
mvtable <- as.table(rbind(c(sum(oli_first_AKI$Mecvenadm),
                            sum(oli_first_noAKI$Mecvenadm)),
                          c(nrow(oli_first_AKI)-sum(oli_first_AKI$Mecvenadm),
                            nrow(oli_first_noAKI)-sum(oli_first_noAKI$Mecvenadm))))
dimnames(mvtable) <- list(c("MV", "Not MV"), c("AKI", "no AKI"))
mvtable
chisq.test(mvtable, correct = FALSE)
#APACHE II
summary(oli_first_AKI$APACHE_II)
summary(oli_first_noAKI$APACHE_II)
shapiro.test(oli_first_AKI$APACHE_II)
shapiro.test(oli_first_noAKI$APACHE_II)
wilcox.test(oli_first_AKI$APACHE_II, oli_first_noAKI$APACHE_II)
#APACHE III
summary(oli_first_AKI$APACHE_III)
summary(oli_first_noAKI$APACHE_III)
shapiro.test(oli_first_AKI$APACHE_III)
shapiro.test(oli_first_noAKI$APACHE_III)
wilcox.test(oli_first_AKI$APACHE_III, oli_first_noAKI$APACHE_III)
#surgadm
sum(oli_first_AKI$Surgadmission)
sum(is.na(oli_first_AKI$Surgadmission))
sum(oli_first_AKI$Surgadmission)/nrow(oli_first_AKI) *100
nrow(oli_first_AKI) - sum(oli_first_AKI$Surgadmission)
(nrow(oli_first_AKI) - sum(oli_first_AKI$Surgadmission)) /nrow(oli_first_AKI) *100
sum(oli_first_noAKI$Surgadmission)
surgadmtable <- as.table(rbind(c(sum(oli_first_AKI$Surgadmission),
                                 sum(oli_first_noAKI$Surgadmission)),
                               c(nrow(oli_first_AKI)-sum(oli_first_AKI$Surgadmission),
                                 nrow(oli_first_noAKI)-sum(oli_first_noAKI$Surgadmission))))
dimnames(surgadmtable) <- list(c("Surgical", "Medical"), c("AKI", "no AKI"))
surgadmtable
chisq.test(surgadmtable, correct = FALSE)
#HOPC ICU admission
sum(oli_first_AKI$PCm_cardio)
sum(oli_first_AKI$PCm_resp)
sum(oli_first_AKI$PCm_GI)
sum(oli_first_AKI$PCm_neuro)
sum(oli_first_AKI$PCm_sepsis)
sum(oli_first_AKI$PCm_trauma)
sum(oli_first_AKI$PCm_metabolic)
sum(oli_first_AKI$PCm_Haem)
sum(oli_first_AKI$PCm_renal)
sum(oli_first_AKI$PCm_other)
sum(oli_first_AKI$PCm_msk)
sum(oli_first_AKI$PCm_cardio) +
  sum(oli_first_AKI$PCm_resp) +
  sum(oli_first_AKI$PCm_GI) +
  sum(oli_first_AKI$PCm_neuro) +
  sum(oli_first_AKI$PCm_sepsis) +
  sum(oli_first_AKI$PCm_trauma) +
  sum(oli_first_AKI$PCm_metabolic) +
  sum(oli_first_AKI$PCm_Haem) +
  sum(oli_first_AKI$PCm_renal) +
  sum(oli_first_AKI$PCm_other) +
  sum(oli_first_AKI$PCm_msk)
nrow(oli_first_AKI) - sum(oli_first_AKI$Surgadmission)
sum(oli_first_AKI$PCs_cardio)
sum(oli_first_AKI$PCs_resp)
sum(oli_first_AKI$PCs_GI)
sum(oli_first_AKI$PCs_neuro)
sum(oli_first_AKI$PCs_trauma)
sum(oli_first_AKI$PCs_renal)
sum(oli_first_AKI$PCs_gynae)
sum(oli_first_AKI$PCs_msk)
sum(oli_first_AKI$PCs_haem)
sum(oli_first_AKI$PCs_metabolic)
sum(oli_first_AKI$PCs_cardio) +
  sum(oli_first_AKI$PCs_resp) +
  sum(oli_first_AKI$PCs_GI) +
  sum(oli_first_AKI$PCs_neuro) +
  sum(oli_first_AKI$PCs_trauma) +
  sum(oli_first_AKI$PCs_renal) +
  sum(oli_first_AKI$PCs_gynae) +
  sum(oli_first_AKI$PCs_msk) +
  sum(oli_first_AKI$PCs_haem) +
  sum(oli_first_AKI$PCs_metabolic)
sum(oli_first_AKI$Surgadmission)
sum(oli_first_noAKI$PCm_cardio)
sum(oli_first_noAKI$PCm_resp)
sum(oli_first_noAKI$PCm_GI)
sum(oli_first_noAKI$PCm_neuro)
sum(oli_first_noAKI$PCm_sepsis)
sum(oli_first_noAKI$PCm_trauma)
sum(oli_first_noAKI$PCm_metabolic)
sum(oli_first_noAKI$PCm_Haem)
sum(oli_first_noAKI$PCm_renal)
sum(oli_first_noAKI$PCm_other)
sum(oli_first_noAKI$PCm_msk)
sum(oli_first_noAKI$PCm_cardio) +
  sum(oli_first_noAKI$PCm_resp) +
  sum(oli_first_noAKI$PCm_GI) +
  sum(oli_first_noAKI$PCm_neuro) +
  sum(oli_first_noAKI$PCm_sepsis) +
  sum(oli_first_noAKI$PCm_trauma) +
  sum(oli_first_noAKI$PCm_metabolic) +
  sum(oli_first_noAKI$PCm_Haem) +
  sum(oli_first_noAKI$PCm_renal) +
  sum(oli_first_noAKI$PCm_other) +
  sum(oli_first_noAKI$PCm_msk)
nrow(oli_first_noAKI) - sum(oli_first_noAKI$Surgadmission)
sum(oli_first_noAKI$PCs_cardio)
sum(oli_first_noAKI$PCs_resp)
sum(oli_first_noAKI$PCs_GI)
sum(oli_first_noAKI$PCs_neuro)
sum(oli_first_noAKI$PCs_trauma)
sum(oli_first_noAKI$PCs_renal)
sum(oli_first_noAKI$PCs_gynae)
sum(oli_first_noAKI$PCs_msk)
sum(oli_first_noAKI$PCs_haem)
sum(oli_first_noAKI$PCs_metabolic)
sum(oli_first_noAKI$PCs_cardio) +
  sum(oli_first_noAKI$PCs_resp) +
  sum(oli_first_noAKI$PCs_GI) +
  sum(oli_first_noAKI$PCs_neuro) +
  sum(oli_first_noAKI$PCs_trauma) +
  sum(oli_first_noAKI$PCs_renal) +
  sum(oli_first_noAKI$PCs_gynae) +
  sum(oli_first_noAKI$PCs_msk) +
  sum(oli_first_noAKI$PCs_haem) +
  sum(oli_first_noAKI$PCs_metabolic)
sum(oli_first_noAKI$Surgadmission)
PCtable <- as.table(rbind(c(sum(oli_first_noAKI$PCm_cardio),
                            sum(oli_first_AKI$PCm_cardio)),
                          c(nrow(oli_first_noAKI) - sum(oli_first_noAKI$PCm_cardio),
                            nrow(oli_first_AKI) - sum(oli_first_AKI$PCm_cardio))))
dimnames(PCtable) <- list(c("mCardio", "other"), c("no AKI", "AKI"))
PCtable
fisher.test(PCtable)
PCtable <- as.table(rbind(c(sum(oli_first_noAKI$PCm_resp),
                            sum(oli_first_AKI$PCm_resp)),
                          c(nrow(oli_first_noAKI) - sum(oli_first_noAKI$PCm_resp),
                            nrow(oli_first_AKI) - sum(oli_first_AKI$PCm_resp))))
dimnames(PCtable) <- list(c("mResp", "other"), c("no AKI", "AKI"))
PCtable
fisher.test(PCtable)
PCtable <- as.table(rbind(c(sum(oli_first_noAKI$PCm_GI),
                            sum(oli_first_AKI$PCm_GI)),
                          c(nrow(oli_first_noAKI) - sum(oli_first_noAKI$PCm_GI),
                            nrow(oli_first_AKI) - sum(oli_first_AKI$PCm_GI))))
dimnames(PCtable) <- list(c("mGI", "other"), c("no AKI", "AKI"))
PCtable
fisher.test(PCtable)
PCtable <- as.table(rbind(c(sum(oli_first_noAKI$PCm_neuro),
                            sum(oli_first_AKI$PCm_neuro)),
                          c(nrow(oli_first_noAKI) - sum(oli_first_noAKI$PCm_neuro),
                            nrow(oli_first_AKI) - sum(oli_first_AKI$PCm_neuro))))
dimnames(PCtable) <- list(c("mNeuro", "other"), c("no AKI", "AKI"))
PCtable
chisq.test(PCtable, correct = FALSE)
PCtable <- as.table(rbind(c(sum(oli_first_noAKI$PCm_sepsis),
                            sum(oli_first_AKI$PCm_sepsis)),
                          c(nrow(oli_first_noAKI) - sum(oli_first_noAKI$PCm_sepsis),
                            nrow(oli_first_AKI) - sum(oli_first_AKI$PCm_sepsis))))
dimnames(PCtable) <- list(c("mSepsis", "other"), c("no AKI", "AKI"))
PCtable
fisher.test(PCtable)
PCtable <- as.table(rbind(c(sum(oli_first_noAKI$PCm_trauma),
                            sum(oli_first_AKI$PCm_trauma)),
                          c(nrow(oli_first_noAKI) - sum(oli_first_noAKI$PCm_trauma),
                            nrow(oli_first_AKI) - sum(oli_first_AKI$PCm_trauma))))
dimnames(PCtable) <- list(c("mTrauma", "other"), c("no AKI", "AKI"))
PCtable
fisher.test(PCtable)
PCtable <- as.table(rbind(c(sum(oli_first_noAKI$PCm_metabolic),
                            sum(oli_first_AKI$PCm_metabolic)),
                          c(nrow(oli_first_noAKI) - sum(oli_first_noAKI$PCm_metabolic),
                            nrow(oli_first_AKI) - sum(oli_first_AKI$PCm_metabolic))))
dimnames(PCtable) <- list(c("mMetabolic", "other"), c("no AKI", "AKI"))
PCtable
fisher.test(PCtable)
PCtable <- as.table(rbind(c(sum(oli_first_noAKI$PCm_renal),
                            sum(oli_first_AKI$PCm_renal)),
                          c(nrow(oli_first_noAKI) - sum(oli_first_noAKI$PCm_renal),
                            nrow(oli_first_AKI) - sum(oli_first_AKI$PCm_renal))))
dimnames(PCtable) <- list(c("mRenal", "other"), c("no AKI", "AKI"))
PCtable
fisher.test(PCtable)
PCtable <- as.table(rbind(c(sum(oli_first_noAKI$PCs_cardio),
                            sum(oli_first_AKI$PCs_cardio)),
                          c(nrow(oli_first_noAKI) - sum(oli_first_noAKI$PCs_cardio),
                            nrow(oli_first_AKI) - sum(oli_first_AKI$PCs_cardio))))
dimnames(PCtable) <- list(c("sCardio", "other"), c("no AKI", "AKI"))
PCtable
chisq.test(PCtable, correct = FALSE)
PCtable <- as.table(rbind(c(sum(oli_first_noAKI$PCs_resp),
                            sum(oli_first_AKI$PCs_resp)),
                          c(nrow(oli_first_noAKI) - sum(oli_first_noAKI$PCs_resp),
                            nrow(oli_first_AKI) - sum(oli_first_AKI$PCs_resp))))
dimnames(PCtable) <- list(c("sResp", "other"), c("no AKI", "AKI"))
PCtable
fisher.test(PCtable)
PCtable <- as.table(rbind(c(sum(oli_first_noAKI$PCs_GI),
                            sum(oli_first_AKI$PCs_GI)),
                          c(nrow(oli_first_noAKI) - sum(oli_first_noAKI$PCs_GI),
                            nrow(oli_first_AKI) - sum(oli_first_AKI$PCs_GI))))
dimnames(PCtable) <- list(c("sGI", "other"), c("no AKI", "AKI"))
PCtable
fisher.test(PCtable)
PCtable <- as.table(rbind(c(sum(oli_first_noAKI$PCs_neuro),
                            sum(oli_first_AKI$PCs_neuro)),
                          c(nrow(oli_first_noAKI) - sum(oli_first_noAKI$PCs_neuro),
                            nrow(oli_first_AKI) - sum(oli_first_AKI$PCs_neuro))))
dimnames(PCtable) <- list(c("sNeuro", "other"), c("no AKI", "AKI"))
PCtable
fisher.test(PCtable)
PCtable <- as.table(rbind(c(sum(oli_first_noAKI$PCs_renal),
                            sum(oli_first_AKI$PCs_renal)),
                          c(nrow(oli_first_noAKI) - sum(oli_first_noAKI$PCs_renal),
                            nrow(oli_first_AKI) - sum(oli_first_AKI$PCs_renal))))
dimnames(PCtable) <- list(c("sRenal", "other"), c("no AKI", "AKI"))
PCtable
fisher.test(PCtable)
PCtable <- as.table(rbind(c(sum(oli_first_noAKI$PCs_gynae),
                            sum(oli_first_AKI$PCs_gynae)),
                          c(nrow(oli_first_noAKI) - sum(oli_first_noAKI$PCs_gynae),
                            nrow(oli_first_AKI) - sum(oli_first_AKI$PCs_gynae))))
dimnames(PCtable) <- list(c("sRenal", "other"), c("no AKI", "AKI"))
PCtable
fisher.test(PCtable)
PCtable <- as.table(rbind(c(sum(oli_first_noAKI$PCs_msk),
                            sum(oli_first_AKI$PCs_msk)),
                          c(nrow(oli_first_noAKI) - sum(oli_first_noAKI$PCs_msk),
                            nrow(oli_first_AKI) - sum(oli_first_AKI$PCs_msk))))
dimnames(PCtable) <- list(c("sMSK", "other"), c("no AKI", "AKI"))
PCtable
fisher.test(PCtable)
#demographics - Compare those with Cr ep to those with neither change 279 vs 74 ----
#Age
summary(cr_first$Age)
shapiro.test(scr_out$neit$Age)
summary(scr_out$neit$Age)
wilcox.test(cr_first$Age, scr_out$neit$Age)
#Male
sum(scr_out$neit$Male)
sum(is.na(scr_out$neit$Male))
sum(scr_out$neit$Male)/nrow(scr_out$neit) *100
gendertable <- as.table(rbind(c(sum(scr_out$neit$Male),
                                sum(cr_first$Male)),
                              c(nrow(scr_out$neit)-sum(scr_out$neit$Male),
                                nrow(cr_first)-sum(cr_first$Male))))
dimnames(gendertable) <- list(c("M", "F"), c("neither", "cr"))
gendertable
chisq.test(gendertable, correct = FALSE)
#Weight
shapiro.test(scr_out$neit$Wt)
summary(scr_out$neit$Wt)
wilcox.test(cr_first$Wt, scr_out$neit$Wt)
#weight measured
sum(na.omit(scr_out$neit$Wtmeasured))
sum(is.na(scr_out$neit$Wtmeasured))
sum(na.omit(scr_out$neit$Wtmeasured))/ nrow(scr_out$neit) *100
wtmeasuredtable <- as.table(rbind(c(sum(na.omit(scr_out$neit$Wtmeasured)),
                                    sum(cr_first$Wtmeasured)),
                                  c(nrow(scr_out$neit)-sum(is.na(scr_out$neit$Wtmeasured))-
                                      sum(na.omit(scr_out$neit$Wtmeasured)),
                                    nrow(cr_first)-sum(cr_first$Wtmeasured))))
dimnames(wtmeasuredtable) <- list(c("Measured", "Estimated"), c("neither", "cr"))
wtmeasuredtable
chisq.test(wtmeasuredtable, correct = FALSE)
#mech vent
sum(scr_out$neit$Mecvenadm)
sum(is.na(scr_out$neit$Mecvenadm))
sum(scr_out$neit$Mecvenadm) / nrow(scr_out$neit) *100
mvtable <- as.table(rbind(c(sum(scr_out$neit$Mecvenadm),
                            sum(cr_first$Mecvenadm)),
                          c(nrow(scr_out$neit)-sum(scr_out$neit$Mecvenadm),
                            nrow(cr_first)-sum(cr_first$Mecvenadm))))
dimnames(mvtable) <- list(c("MV", "Not MV"), c("neither", "cr"))
mvtable
chisq.test(mvtable, correct = FALSE)
#surg vs med adm
sum(scr_out$neit$Surgadmission)
sum(is.na(scr_out$neit$Surgadmission))
sum(scr_out$neit$Surgadmission) / nrow(scr_out$neit) *100
nrow(scr_out$neit) - sum(scr_out$neit$Surgadmission)
(nrow(scr_out$neit) - sum(scr_out$neit$Surgadmission))/nrow(scr_out$neit) *100
surgadmtable <- as.table(rbind(c(sum(scr_out$neit$Surgadmission),
                                 sum(cr_first$Surgadmission)),
                               c(nrow(scr_out$neit)-sum(scr_out$neit$Surgadmission),
                                 nrow(cr_first)-sum(cr_first$Surgadmission))))
dimnames(surgadmtable) <- list(c("Surgical", "Medical"), c("neither", "cr"))
surgadmtable
chisq.test(surgadmtable, correct = FALSE)
#APACHE II
summary(scr_out$neit$APACHE_II)
shapiro.test(scr_out$neit$APACHE_II)
wilcox.test(cr_first$APACHE_II, scr_out$neit$APACHE_II)
#APACHE III
summary(scr_out$neit$APACHE_III)
shapiro.test(scr_out$neit$APACHE_III)
wilcox.test(cr_first$APACHE_III, scr_out$neit$APACHE_III)
#HOPC ICU admission
sum(scr_out$neit$PCm_cardio)
sum(scr_out$neit$PCm_resp)
sum(scr_out$neit$PCm_GI)
sum(scr_out$neit$PCm_neuro)
sum(scr_out$neit$PCm_sepsis)
sum(scr_out$neit$PCm_trauma)
sum(scr_out$neit$PCm_metabolic)
sum(scr_out$neit$PCm_Haem)
sum(scr_out$neit$PCm_renal)
sum(scr_out$neit$PCm_other)
sum(scr_out$neit$PCm_msk)
sum(scr_out$neit$PCm_cardio) +
  sum(scr_out$neit$PCm_resp) +
  sum(scr_out$neit$PCm_GI) +
  sum(scr_out$neit$PCm_neuro) +
  sum(scr_out$neit$PCm_sepsis) +
  sum(scr_out$neit$PCm_trauma) +
  sum(scr_out$neit$PCm_metabolic) +
  sum(scr_out$neit$PCm_Haem) +
  sum(scr_out$neit$PCm_renal) +
  sum(scr_out$neit$PCm_other) +
  sum(scr_out$neit$PCm_msk)
nrow(scr_out$neit) - sum(scr_out$neit$Surgadmission)
sum(scr_out$neit$PCs_cardio)
sum(scr_out$neit$PCs_resp)
sum(scr_out$neit$PCs_GI)
sum(scr_out$neit$PCs_neuro)
sum(scr_out$neit$PCs_trauma)
sum(scr_out$neit$PCs_renal)
sum(scr_out$neit$PCs_gynae)
sum(scr_out$neit$PCs_msk)
sum(scr_out$neit$PCs_haem)
sum(scr_out$neit$PCs_metabolic)
sum(scr_out$neit$PCs_cardio) +
  sum(scr_out$neit$PCs_resp) +
  sum(scr_out$neit$PCs_GI) +
  sum(scr_out$neit$PCs_neuro) +
  sum(scr_out$neit$PCs_trauma) +
  sum(scr_out$neit$PCs_renal) +
  sum(scr_out$neit$PCs_gynae) +
  sum(scr_out$neit$PCs_msk) +
  sum(scr_out$neit$PCs_haem) +
  sum(scr_out$neit$PCs_metabolic)
sum(scr_out$neit$Surgadmission)
PCtable <- as.table(rbind(c(sum(scr_out$neit$PCm_cardio),
                            sum(cr_first$PCm_cardio)),
                          c(nrow(scr_out$neit) - sum(scr_out$neit$PCm_cardio),
                            nrow(cr_first) - sum(cr_first$PCm_cardio))))
dimnames(PCtable) <- list(c("mCardio", "other"), c("neit", "cr"))
PCtable
chisq.test(PCtable, correct = FALSE)
PCtable <- as.table(rbind(c(sum(scr_out$neit$PCm_resp),
                            sum(cr_first$PCm_resp)),
                          c(nrow(scr_out$neit) - sum(scr_out$neit$PCm_resp),
                            nrow(cr_first) - sum(cr_first$PCm_resp))))
dimnames(PCtable) <- list(c("mResp", "other"), c("neit", "cr"))
PCtable
chisq.test(PCtable, correct = FALSE)
PCtable <- as.table(rbind(c(sum(scr_out$neit$PCm_GI),
                            sum(cr_first$PCm_GI)),
                          c(nrow(scr_out$neit) - sum(scr_out$neit$PCm_GI),
                            nrow(cr_first) - sum(cr_first$PCm_GI))))
dimnames(PCtable) <- list(c("mGI", "other"), c("neit", "cr"))
PCtable
fisher.test(PCtable)
PCtable <- as.table(rbind(c(sum(scr_out$neit$PCm_neuro),
                            sum(cr_first$PCm_neuro)),
                          c(nrow(scr_out$neit) - sum(scr_out$neit$PCm_neuro),
                            nrow(cr_first) - sum(cr_first$PCm_neuro))))
dimnames(PCtable) <- list(c("mNeuro", "other"), c("neit", "cr"))
PCtable
fisher.test(PCtable)
PCtable <- as.table(rbind(c(sum(scr_out$neit$PCm_sepsis),
                            sum(cr_first$PCm_sepsis)),
                          c(nrow(scr_out$neit) - sum(scr_out$neit$PCm_sepsis),
                            nrow(cr_first) - sum(cr_first$PCm_sepsis))))
dimnames(PCtable) <- list(c("mSepsis", "other"), c("neit", "cr"))
PCtable
chisq.test(PCtable, correct = FALSE)
PCtable <- as.table(rbind(c(sum(scr_out$neit$PCm_trauma),
                            sum(cr_first$PCm_trauma)),
                          c(nrow(scr_out$neit) - sum(scr_out$neit$PCm_trauma),
                            nrow(cr_first) - sum(cr_first$PCm_trauma))))
dimnames(PCtable) <- list(c("mTrauma", "other"), c("neit", "cr"))
PCtable
fisher.test(PCtable)
PCtable <- as.table(rbind(c(sum(scr_out$neit$PCm_metabolic),
                            sum(cr_first$PCm_metabolic)),
                          c(nrow(scr_out$neit) - sum(scr_out$neit$PCm_metabolic),
                            nrow(cr_first) - sum(cr_first$PCm_metabolic))))
dimnames(PCtable) <- list(c("mMetabolic", "other"), c("neit", "cr"))
PCtable
chisq.test(PCtable, correct = FALSE)
PCtable <- as.table(rbind(c(sum(scr_out$neit$PCm_renal),
                            sum(cr_first$PCm_renal)),
                          c(nrow(scr_out$neit) - sum(scr_out$neit$PCm_renal),
                            nrow(cr_first) - sum(cr_first$PCm_renal))))
dimnames(PCtable) <- list(c("mRenal", "other"), c("neit", "cr"))
PCtable
fisher.test(PCtable)
PCtable <- as.table(rbind(c(sum(scr_out$neit$PCs_cardio),
                            sum(cr_first$PCs_cardio)),
                          c(nrow(scr_out$neit) - sum(scr_out$neit$PCs_cardio),
                            nrow(cr_first) - sum(cr_first$PCs_cardio))))
dimnames(PCtable) <- list(c("sCardio", "other"), c("neit", "cr"))
PCtable
chisq.test(PCtable, correct = FALSE)
PCtable <- as.table(rbind(c(sum(scr_out$neit$PCs_resp),
                            sum(cr_first$PCs_resp)),
                          c(nrow(scr_out$neit) - sum(scr_out$neit$PCs_resp),
                            nrow(cr_first) - sum(cr_first$PCs_resp))))
dimnames(PCtable) <- list(c("sResp", "other"), c("neit", "cr"))
PCtable
fisher.test(PCtable)
PCtable <- as.table(rbind(c(sum(scr_out$neit$PCs_GI),
                            sum(cr_first$PCs_GI)),
                          c(nrow(scr_out$neit) - sum(scr_out$neit$PCs_GI),
                            nrow(cr_first) - sum(cr_first$PCs_GI))))
dimnames(PCtable) <- list(c("sGI", "other"), c("neit", "cr"))
PCtable
chisq.test(PCtable, correct = FALSE)
PCtable <- as.table(rbind(c(sum(scr_out$neit$PCs_neuro),
                            sum(cr_first$PCs_neuro)),
                          c(nrow(scr_out$neit) - sum(scr_out$neit$PCs_neuro),
                            nrow(cr_first) - sum(cr_first$PCs_neuro))))
dimnames(PCtable) <- list(c("sNeuro", "other"), c("neit", "cr"))
PCtable
fisher.test(PCtable)
PCtable <- as.table(rbind(c(sum(scr_out$neit$PCs_renal),
                            sum(cr_first$PCs_renal)),
                          c(nrow(scr_out$neit) - sum(scr_out$neit$PCs_renal),
                            nrow(cr_first) - sum(cr_first$PCs_renal))))
dimnames(PCtable) <- list(c("sRenal", "other"), c("neit", "cr"))
PCtable
fisher.test(PCtable)
PCtable <- as.table(rbind(c(sum(scr_out$neit$PCs_msk),
                            sum(cr_first$PCs_msk)),
                          c(nrow(scr_out$neit) - sum(scr_out$neit$PCs_msk),
                            nrow(cr_first) - sum(cr_first$PCs_msk))))
dimnames(PCtable) <- list(c("sMSK", "other"), c("neit", "cr"))
PCtable
chisq.test(PCtable, correct = FALSE)
PCtable <- as.table(rbind(c(sum(scr_out$neit$PCs_metabolic),
                            sum(cr_first$PCs_metabolic)),
                          c(nrow(scr_out$neit) - sum(scr_out$neit$PCs_metabolic),
                            nrow(cr_first) - sum(cr_first$PCs_metabolic))))
dimnames(PCtable) <- list(c("sMetabolic", "other"), c("neit", "cr"))
PCtable
fisher.test(PCtable)
#demographics - Compare those with oliguria to those with neither change 165 vs 74----
#Age
wilcox.test(oli_first$Age, scr_out$neit$Age)
#Male
gendertable <- as.table(rbind(c(sum(scr_out$neit$Male),
                                sum(oli_first$Male)),
                              c(nrow(scr_out$neit)-sum(scr_out$neit$Male),
                                nrow(oli_first)-sum(oli_first$Male))))
dimnames(gendertable) <- list(c("M", "F"), c("neither", "oli"))
gendertable
chisq.test(gendertable, correct = FALSE)
#Weight
wilcox.test(oli_first$Wt, scr_out$neit$Wt)
#weight measured
wtmeasuredtable <- as.table(rbind(c(sum(na.omit(scr_out$neit$Wtmeasured)),
                                    sum(oli_first$Wtmeasured)),
                                  c(nrow(scr_out$neit)-sum(is.na(scr_out$neit$Wtmeasured))-
                                      sum(na.omit(scr_out$neit$Wtmeasured)),
                                    nrow(oli_first)-sum(oli_first$Wtmeasured))))
dimnames(wtmeasuredtable) <- list(c("Measured", "Estimated"), c("neither", "Oli"))
wtmeasuredtable
chisq.test(wtmeasuredtable, correct = FALSE)
#mech vent
mvtable <- as.table(rbind(c(sum(scr_out$neit$Mecvenadm),
                            sum(oli_first$Mecvenadm)),
                          c(nrow(scr_out$neit)-sum(scr_out$neit$Mecvenadm),
                            nrow(oli_first)-sum(oli_first$Mecvenadm))))
dimnames(mvtable) <- list(c("MV", "Not MV"), c("neither", "oli"))
mvtable
chisq.test(mvtable, correct = FALSE)
#surg vs med adm
surgadmtable <- as.table(rbind(c(sum(scr_out$neit$Surgadmission),
                                 sum(oli_first$Surgadmission)),
                               c(nrow(scr_out$neit)-sum(scr_out$neit$Surgadmission),
                                 nrow(oli_first)-sum(oli_first$Surgadmission))))
dimnames(surgadmtable) <- list(c("Surgical", "Medical"), c("neither", "oli"))
surgadmtable
chisq.test(surgadmtable, correct = FALSE)
#APACHE II
wilcox.test(oli_first$APACHE_II, scr_out$neit$APACHE_II)
#APACHE III
wilcox.test(oli_first$APACHE_III, scr_out$neit$APACHE_III)
#HOPC ICU admission
PCtable <- as.table(rbind(c(sum(scr_out$neit$PCm_cardio),
                            sum(oli_first$PCm_cardio)),
                          c(nrow(scr_out$neit) - sum(scr_out$neit$PCm_cardio),
                            nrow(oli_first) - sum(oli_first$PCm_cardio))))
dimnames(PCtable) <- list(c("mCardio", "other"), c("neit", "oli"))
PCtable
chisq.test(PCtable, correct = FALSE)
PCtable <- as.table(rbind(c(sum(scr_out$neit$PCm_resp),
                            sum(oli_first$PCm_resp)),
                          c(nrow(scr_out$neit) - sum(scr_out$neit$PCm_resp),
                            nrow(oli_first) - sum(oli_first$PCm_resp))))
dimnames(PCtable) <- list(c("mResp", "other"), c("neit", "oli"))
PCtable
chisq.test(PCtable, correct = FALSE)
PCtable <- as.table(rbind(c(sum(scr_out$neit$PCm_GI),
                            sum(oli_first$PCm_GI)),
                          c(nrow(scr_out$neit) - sum(scr_out$neit$PCm_GI),
                            nrow(oli_first) - sum(oli_first$PCm_GI))))
dimnames(PCtable) <- list(c("mGI", "other"), c("neit", "oli"))
PCtable
fisher.test(PCtable)
PCtable <- as.table(rbind(c(sum(scr_out$neit$PCm_neuro),
                            sum(oli_first$PCm_neuro)),
                          c(nrow(scr_out$neit) - sum(scr_out$neit$PCm_neuro),
                            nrow(oli_first) - sum(oli_first$PCm_neuro))))
dimnames(PCtable) <- list(c("mNeuro", "other"), c("neit", "oli"))
PCtable
fisher.test(PCtable)
PCtable <- as.table(rbind(c(sum(scr_out$neit$PCm_sepsis),
                            sum(oli_first$PCm_sepsis)),
                          c(nrow(scr_out$neit) - sum(scr_out$neit$PCm_sepsis),
                            nrow(oli_first) - sum(oli_first$PCm_sepsis))))
dimnames(PCtable) <- list(c("mSepsis", "other"), c("neit", "oli"))
PCtable
chisq.test(PCtable, correct = FALSE)
PCtable <- as.table(rbind(c(sum(scr_out$neit$PCm_trauma),
                            sum(oli_first$PCm_trauma)),
                          c(nrow(scr_out$neit) - sum(scr_out$neit$PCm_trauma),
                            nrow(oli_first) - sum(oli_first$PCm_trauma))))
dimnames(PCtable) <- list(c("mTrauma", "other"), c("neit", "oli"))
PCtable
fisher.test(PCtable)
PCtable <- as.table(rbind(c(sum(scr_out$neit$PCm_metabolic),
                            sum(oli_first$PCm_metabolic)),
                          c(nrow(scr_out$neit) - sum(scr_out$neit$PCm_metabolic),
                            nrow(oli_first) - sum(oli_first$PCm_metabolic))))
dimnames(PCtable) <- list(c("mMetabolic", "other"), c("neit", "oli"))
PCtable
fisher.test(PCtable)
PCtable <- as.table(rbind(c(sum(scr_out$neit$PCm_renal),
                            sum(oli_first$PCm_renal)),
                          c(nrow(scr_out$neit) - sum(scr_out$neit$PCm_renal),
                            nrow(oli_first) - sum(oli_first$PCm_renal))))
dimnames(PCtable) <- list(c("mRenal", "other"), c("neit", "oli"))
PCtable
fisher.test(PCtable)
PCtable <- as.table(rbind(c(sum(scr_out$neit$PCs_cardio),
                            sum(oli_first$PCs_cardio)),
                          c(nrow(scr_out$neit) - sum(scr_out$neit$PCs_cardio),
                            nrow(oli_first) - sum(oli_first$PCs_cardio))))
dimnames(PCtable) <- list(c("sCardio", "other"), c("neit", "oli"))
PCtable
chisq.test(PCtable, correct = FALSE)
PCtable <- as.table(rbind(c(sum(scr_out$neit$PCs_resp),
                            sum(oli_first$PCs_resp)),
                          c(nrow(scr_out$neit) - sum(scr_out$neit$PCs_resp),
                            nrow(oli_first) - sum(oli_first$PCs_resp))))
dimnames(PCtable) <- list(c("sResp", "other"), c("neit", "oli"))
PCtable
fisher.test(PCtable)
PCtable <- as.table(rbind(c(sum(scr_out$neit$PCs_GI),
                            sum(oli_first$PCs_GI)),
                          c(nrow(scr_out$neit) - sum(scr_out$neit$PCs_GI),
                            nrow(oli_first) - sum(oli_first$PCs_GI))))
dimnames(PCtable) <- list(c("sGI", "other"), c("neit", "oli"))
PCtable
chisq.test(PCtable, correct = FALSE)
PCtable <- as.table(rbind(c(sum(scr_out$neit$PCs_neuro),
                            sum(oli_first$PCs_neuro)),
                          c(nrow(scr_out$neit) - sum(scr_out$neit$PCs_neuro),
                            nrow(oli_first) - sum(oli_first$PCs_neuro))))
dimnames(PCtable) <- list(c("sNeuro", "other"), c("neit", "oli"))
PCtable
fisher.test(PCtable)
PCtable <- as.table(rbind(c(sum(scr_out$neit$PCs_renal),
                            sum(oli_first$PCs_renal)),
                          c(nrow(scr_out$neit) - sum(scr_out$neit$PCs_renal),
                            nrow(oli_first) - sum(oli_first$PCs_renal))))
dimnames(PCtable) <- list(c("sRenal", "other"), c("neit", "oli"))
PCtable
fisher.test(PCtable)
PCtable <- as.table(rbind(c(sum(scr_out$neit$PCs_gynae),
                            sum(oli_first$PCs_gynae)),
                          c(nrow(scr_out$neit) - sum(scr_out$neit$PCs_gynae),
                            nrow(oli_first) - sum(oli_first$PCs_gynae))))
dimnames(PCtable) <- list(c("sRenal", "other"), c("neit", "oli"))
PCtable
fisher.test(PCtable)
PCtable <- as.table(rbind(c(sum(scr_out$neit$PCs_msk),
                            sum(oli_first$PCs_msk)),
                          c(nrow(scr_out$neit) - sum(scr_out$neit$PCs_msk),
                            nrow(oli_first) - sum(oli_first$PCs_msk))))
dimnames(PCtable) <- list(c("sMSK", "other"), c("neit", "oli"))
PCtable
chisq.test(PCtable, correct = FALSE)
PCtable <- as.table(rbind(c(sum(scr_out$neit$PCs_metabolic),
                            sum(oli_first$PCs_metabolic)),
                          c(nrow(scr_out$neit) - sum(scr_out$neit$PCs_metabolic),
                            nrow(oli_first) - sum(oli_first$PCs_metabolic))))
dimnames(PCtable) <- list(c("sMetabolic", "other"), c("neit", "oli"))
PCtable
fisher.test(PCtable)
#demographics - compare those with both cr and oli vs neither 131 vs 74 ----
#Age
summary(creoliboth1$Age)
shapiro.test(creoliboth1$Age)
wilcox.test(creoliboth1$Age, scr_out$neit$Age)
#Male
sum(creoliboth1$Male)
sum(is.na(creoliboth1$Male))
sum(creoliboth1$Male)/nrow(creoliboth1) *100
nrow(creoliboth1) - sum(creoliboth1$Male)
gendertable <- as.table(rbind(c(sum(scr_out$neit$Male),
                                sum(creoliboth1$Male)),
                              c(nrow(scr_out$neit)-sum(scr_out$neit$Male),
                                nrow(creoliboth1)-sum(creoliboth1$Male))))
dimnames(gendertable) <- list(c("M", "F"), c("neither", "both"))
gendertable
chisq.test(gendertable, correct = FALSE)
#weight
summary(creoliboth1$Wt)
shapiro.test(creoliboth1$Wt)
wilcox.test(creoliboth1$Age, scr_out$neit$Wt)
#weight measured
sum(creoliboth1$Wtmeasured)
sum(is.na(creoliboth1$Wtmeasured))
sum(creoliboth1$Wtmeasured)/nrow(creoliboth1) *100
wtmeasuredtable <- as.table(rbind(c(sum(na.omit(scr_out$neit$Wtmeasured)),
                                    sum(creoliboth1$Wtmeasured)),
                                  c(nrow(scr_out$neit)-sum(is.na(scr_out$neit$Wtmeasured))-
                                      sum(na.omit(scr_out$neit$Wtmeasured)),
                                    nrow(creoliboth1)-sum(creoliboth1$Wtmeasured))))
dimnames(wtmeasuredtable) <- list(c("Measured", "Estimated"), c("neither", "both"))
wtmeasuredtable
chisq.test(wtmeasuredtable, correct = FALSE)
#mec vent
sum(creoliboth1$Mecvenadm)
sum(is.na(creoliboth1$Mecvenadm))
sum(creoliboth1$Mecvenadm)/nrow(creoliboth1) *100
mvtable <- as.table(rbind(c(sum(scr_out$neit$Mecvenadm),
                            sum(creoliboth1$Mecvenadm)),
                          c(nrow(scr_out$neit)-sum(scr_out$neit$Mecvenadm),
                            nrow(creoliboth1)-sum(creoliboth1$Mecvenadm))))
dimnames(mvtable) <- list(c("MV", "Not MV"), c("neither", "both"))
mvtable
chisq.test(mvtable, correct = FALSE)
#APACHE II
summary(creoliboth1$APACHE_II)
shapiro.test(creoliboth1$APACHE_II)
wilcox.test(creoliboth1$APACHE_II, scr_out$neit$APACHE_II)
#APACHE III
summary(creoliboth1$APACHE_III)
shapiro.test(creoliboth1$APACHE_III)
wilcox.test(creoliboth1$APACHE_III, scr_out$neit$APACHE_III)
#surgadm
sum(creoliboth1$Surgadmission)
sum(is.na(creoliboth1$Surgadmission))
sum(creoliboth1$Surgadmission)/nrow(creoliboth1) *100
nrow(creoliboth1)- sum(creoliboth1$Surgadmission)
(nrow(creoliboth1)- sum(creoliboth1$Surgadmission)) / nrow(creoliboth1) *100
surgadmtable <- as.table(rbind(c(sum(scr_out$neit$Surgadmission),
                                 sum(creoliboth1$Surgadmission)),
                               c(nrow(scr_out$neit)-sum(scr_out$neit$Surgadmission),
                                 nrow(creoliboth1)-sum(creoliboth1$Surgadmission))))
dimnames(surgadmtable) <- list(c("Surgical", "Medical"), c("neither", "both"))
surgadmtable
chisq.test(surgadmtable, correct = FALSE)
#HOPC ICU admission
sum(creoliboth1$PCm_cardio)
sum(creoliboth1$PCm_resp)
sum(creoliboth1$PCm_GI)
sum(creoliboth1$PCm_neuro)
sum(creoliboth1$PCm_sepsis)
sum(creoliboth1$PCm_trauma)
sum(creoliboth1$PCm_metabolic)
sum(creoliboth1$PCm_Haem)
sum(creoliboth1$PCm_renal)
sum(creoliboth1$PCm_other)
sum(creoliboth1$PCm_msk)
sum(creoliboth1$PCm_cardio) +
  sum(creoliboth1$PCm_resp) +
  sum(creoliboth1$PCm_GI) +
  sum(creoliboth1$PCm_neuro) +
  sum(creoliboth1$PCm_sepsis) +
  sum(creoliboth1$PCm_trauma) +
  sum(creoliboth1$PCm_metabolic) +
  sum(creoliboth1$PCm_Haem) +
  sum(creoliboth1$PCm_renal) +
  sum(creoliboth1$PCm_other) +
  sum(creoliboth1$PCm_msk)
nrow(creoliboth1) - sum(creoliboth1$Surgadmission)
sum(creoliboth1$PCs_cardio)
sum(creoliboth1$PCs_resp)
sum(creoliboth1$PCs_GI)
sum(creoliboth1$PCs_neuro)
sum(creoliboth1$PCs_trauma)
sum(creoliboth1$PCs_renal)
sum(creoliboth1$PCs_gynae)
sum(creoliboth1$PCs_msk)
sum(creoliboth1$PCs_haem)
sum(creoliboth1$PCs_metabolic)
sum(creoliboth1$PCs_cardio) +
  sum(creoliboth1$PCs_resp) +
  sum(creoliboth1$PCs_GI) +
  sum(creoliboth1$PCs_neuro) +
  sum(creoliboth1$PCs_trauma) +
  sum(creoliboth1$PCs_renal) +
  sum(creoliboth1$PCs_gynae) +
  sum(creoliboth1$PCs_msk) +
  sum(creoliboth1$PCs_haem) +
  sum(creoliboth1$PCs_metabolic)
sum(creoliboth1$Surgadmission)
PCtable <- as.table(rbind(c(sum(scr_out$neit$PCm_cardio),
                            sum(creoliboth1$PCm_cardio)),
                          c(nrow(scr_out$neit) - sum(scr_out$neit$PCm_cardio),
                            nrow(creoliboth1) - sum(creoliboth1$PCm_cardio))))
dimnames(PCtable) <- list(c("mCardio", "other"), c("neit", "both"))
PCtable
chisq.test(PCtable, correct = FALSE)
PCtable <- as.table(rbind(c(sum(scr_out$neit$PCm_resp),
                            sum(creoliboth1$PCm_resp)),
                          c(nrow(scr_out$neit) - sum(scr_out$neit$PCm_resp),
                            nrow(creoliboth1) - sum(creoliboth1$PCm_resp))))
dimnames(PCtable) <- list(c("mResp", "other"), c("neit", "both"))
PCtable
chisq.test(PCtable, correct = FALSE)
PCtable <- as.table(rbind(c(sum(scr_out$neit$PCm_GI),
                            sum(creoliboth1$PCm_GI)),
                          c(nrow(scr_out$neit) - sum(scr_out$neit$PCm_GI),
                            nrow(creoliboth1) - sum(creoliboth1$PCm_GI))))
dimnames(PCtable) <- list(c("mGI", "other"), c("neit", "both"))
PCtable
fisher.test(PCtable)
PCtable <- as.table(rbind(c(sum(scr_out$neit$PCm_neuro),
                            sum(creoliboth1$PCm_neuro)),
                          c(nrow(scr_out$neit) - sum(scr_out$neit$PCm_neuro),
                            nrow(creoliboth1) - sum(creoliboth1$PCm_neuro))))
dimnames(PCtable) <- list(c("mNeuro", "other"), c("neit", "both"))
PCtable
fisher.test(PCtable)
PCtable <- as.table(rbind(c(sum(scr_out$neit$PCm_sepsis),
                            sum(creoliboth1$PCm_sepsis)),
                          c(nrow(scr_out$neit) - sum(scr_out$neit$PCm_sepsis),
                            nrow(creoliboth1) - sum(creoliboth1$PCm_sepsis))))
dimnames(PCtable) <- list(c("mSepsis", "other"), c("neit", "both"))
PCtable
fisher.test(PCtable)
PCtable <- as.table(rbind(c(sum(scr_out$neit$PCm_trauma),
                            sum(creoliboth1$PCm_trauma)),
                          c(nrow(scr_out$neit) - sum(scr_out$neit$PCm_trauma),
                            nrow(creoliboth1) - sum(creoliboth1$PCm_trauma))))
dimnames(PCtable) <- list(c("mTrauma", "other"), c("neit", "both"))
PCtable
fisher.test(PCtable)
PCtable <- as.table(rbind(c(sum(scr_out$neit$PCm_metabolic),
                            sum(creoliboth1$PCm_metabolic)),
                          c(nrow(scr_out$neit) - sum(scr_out$neit$PCm_metabolic),
                            nrow(creoliboth1) - sum(creoliboth1$PCm_metabolic))))
dimnames(PCtable) <- list(c("mMetabolic", "other"), c("neit", "both"))
PCtable
fisher.test(PCtable)
PCtable <- as.table(rbind(c(sum(scr_out$neit$PCm_renal),
                            sum(creoliboth1$PCm_renal)),
                          c(nrow(scr_out$neit) - sum(scr_out$neit$PCm_renal),
                            nrow(creoliboth1) - sum(creoliboth1$PCm_renal))))
dimnames(PCtable) <- list(c("mRenal", "other"), c("neit", "both"))
PCtable
fisher.test(PCtable)
PCtable <- as.table(rbind(c(sum(scr_out$neit$PCs_cardio),
                            sum(creoliboth1$PCs_cardio)),
                          c(nrow(scr_out$neit) - sum(scr_out$neit$PCs_cardio),
                            nrow(creoliboth1) - sum(creoliboth1$PCs_cardio))))
dimnames(PCtable) <- list(c("sCardio", "other"), c("neit", "both"))
PCtable
chisq.test(PCtable, correct = FALSE)
PCtable <- as.table(rbind(c(sum(scr_out$neit$PCs_resp),
                            sum(creoliboth1$PCs_resp)),
                          c(nrow(scr_out$neit) - sum(scr_out$neit$PCs_resp),
                            nrow(creoliboth1) - sum(creoliboth1$PCs_resp))))
dimnames(PCtable) <- list(c("sResp", "other"), c("neit", "both"))
PCtable
fisher.test(PCtable)
PCtable <- as.table(rbind(c(sum(scr_out$neit$PCs_GI),
                            sum(creoliboth1$PCs_GI)),
                          c(nrow(scr_out$neit) - sum(scr_out$neit$PCs_GI),
                            nrow(creoliboth1) - sum(creoliboth1$PCs_GI))))
dimnames(PCtable) <- list(c("sGI", "other"), c("neit", "both"))
PCtable
chisq.test(PCtable, correct = FALSE)
PCtable <- as.table(rbind(c(sum(scr_out$neit$PCs_neuro),
                            sum(creoliboth1$PCs_neuro)),
                          c(nrow(scr_out$neit) - sum(scr_out$neit$PCs_neuro),
                            nrow(creoliboth1) - sum(creoliboth1$PCs_neuro))))
dimnames(PCtable) <- list(c("sNeuro", "other"), c("neit", "both"))
PCtable
fisher.test(PCtable)
PCtable <- as.table(rbind(c(sum(scr_out$neit$PCs_renal),
                            sum(creoliboth1$PCs_renal)),
                          c(nrow(scr_out$neit) - sum(scr_out$neit$PCs_renal),
                            nrow(creoliboth1) - sum(creoliboth1$PCs_renal))))
dimnames(PCtable) <- list(c("sRenal", "other"), c("neit", "both"))
PCtable
fisher.test(PCtable)
PCtable <- as.table(rbind(c(sum(scr_out$neit$PCs_msk),
                            sum(creoliboth1$PCs_msk)),
                          c(nrow(scr_out$neit) - sum(scr_out$neit$PCs_msk),
                            nrow(creoliboth1) - sum(creoliboth1$PCs_msk))))
dimnames(PCtable) <- list(c("sMSK", "other"), c("neit", "both"))
PCtable
fisher.test(PCtable)
PCtable <- as.table(rbind(c(sum(scr_out$neit$PCs_metabolic),
                            sum(creoliboth1$PCs_metabolic)),
                          c(nrow(scr_out$neit) - sum(scr_out$neit$PCs_metabolic),
                            nrow(creoliboth1) - sum(creoliboth1$PCs_metabolic))))
dimnames(PCtable) <- list(c("sMetabolic", "other"), c("neit", "both"))
PCtable
fisher.test(PCtable)

#--- OUTCOMES - AKI, RRT, LOS, mortality ----
#outcomes - cr 279 vs no cr 108 ----
#AKI
sum(cr_first$AKI_ICU)
sum(is.na(cr_first$AKI_ICU))
sum(cr_first$AKI_ICU)/nrow(cr_first) *100
sum(olionly_first$AKI_ICU)
sum(is.na(olionly_first$AKI_ICU))
sum(olionly_first$AKI_ICU == "0")
sum(olionly_first$AKI_ICU)/ 108 *100
AKItable <- as.table(rbind(c(sum(cr_first$AKI_ICU),
                             sum(olionly_first$AKI_ICU)),
                           c(nrow(cr_first) - sum(cr_first$AKI_ICU),
                             108 - sum(olionly_first$AKI_ICU))))
dimnames(AKItable) <- list(c("AKI", "no AKI"), c("cr", "no cr"))
AKItable
chisq.test(AKItable, correct = FALSE)
#AKI stages
sum(is.na(cr_first$AKI_stage))
sum(na.omit(cr_first$AKI_stage == "1"))
sum(na.omit(cr_first$AKI_stage == "2"))
sum(na.omit(cr_first$AKI_stage == "3"))
nrow(cr_first) == sum(na.omit(cr_first$AKI_stage == "1")) +sum(na.omit(cr_first$AKI_stage == "2")) +
  sum(na.omit(cr_first$AKI_stage == "3")) + sum(is.na(cr_first$AKI_stage))
sum(na.omit(cr_first$AKI_stage == "1"))/sum(cr_first$AKI_ICU) *100
sum(na.omit(cr_first$AKI_stage == "2"))/sum(cr_first$AKI_ICU) *100
sum(na.omit(cr_first$AKI_stage == "3"))/sum(cr_first$AKI_ICU) *100
sum(is.na(olionly_first$AKI_stage))
sum(na.omit(olionly_first$AKI_stage == "1"))
sum(na.omit(olionly_first$AKI_stage == "2"))
sum(na.omit(olionly_first$AKI_stage == "3"))
AKIs1table <- as.table(rbind(c(sum(na.omit(cr_first$AKI_stage == "1")),
                               sum(na.omit(olionly_first$AKI_stage == "1"))),
                             c(sum(cr_first$AKI_ICU) - sum(na.omit(cr_first$AKI_stage == "1")),
                               sum(olionly_first$AKI_ICU) - sum(na.omit(olionly_first$AKI_stage == "1")))))
dimnames(AKIs1table) <- list(c("AKIs1", "not AKIs1"), c("cr", "no cr"))
AKIs1table
chisq.test(AKIs1table, correct = FALSE)
AKIs2table <- as.table(rbind(c(sum(na.omit(cr_first$AKI_stage == "2")),
                               sum(na.omit(olionly_first$AKI_stage == "2"))),
                             c(sum(cr_first$AKI_ICU) - sum(na.omit(cr_first$AKI_stage == "2")),
                               sum(olionly_first$AKI_ICU) - sum(na.omit(olionly_first$AKI_stage == "2")))))
dimnames(AKIs2table) <- list(c("AKIs2", "not AKIs2"), c("cr", "no cr"))
AKIs2table
chisq.test(AKIs2table, correct = FALSE)
AKIs3table <- as.table(rbind(c(sum(na.omit(cr_first$AKI_stage == "3")),
                               sum(na.omit(olionly_first$AKI_stage == "3"))),
                             c(sum(cr_first$AKI_ICU) - sum(na.omit(cr_first$AKI_stage == "3")),
                               sum(olionly_first$AKI_ICU) - sum(na.omit(olionly_first$AKI_stage == "3")))))
dimnames(AKIs3table) <- list(c("AKIs3", "not AKIs3"), c("cr", "no cr"))
AKIs3table
fisher.test(AKIs3table)
#hours from ICU adm to AKI
summary(cr_first$ICUadmtoAKIDx)
shapiro.test(cr_first$ICUadmtoAKIDx)
summary(olionly_first$ICUadmtoAKIDx)
shapiro.test(olionly_first$ICUadmtoAKIDx)
wilcox.test(olionly_first$ICUadmtoAKIDx, cr_first$ICUadmtoAKIDx)
#RRT
sum(na.omit(cr_first$RRT))
sum(is.na(cr_first$RRT))
sum(na.omit(cr_first$RRT == "0"))
sum(na.omit(cr_first$RRT))/sum(cr_first$AKI_ICU) *100
sum(is.na(olionly_first$RRT))
sum(na.omit(olionly_first$RRT))
RRTtable <- as.table(rbind(c(sum(na.omit(cr_first$RRT)),
                             sum(na.omit(olionly_first$RRT))),
                           c(sum(cr_first$AKI_ICU) - sum(na.omit(cr_first$RRT)),
                             sum(olionly_first$AKI_ICU) - sum(na.omit(olionly_first$RRT)))))
dimnames(RRTtable) <- list(c("RRT", "not RRT"), c("cr", "no cr"))
RRTtable
fisher.test(RRTtable)
#ICU LOS
summary(cr_first$ICU_LOS)
cr_first %>% filter(ICU_LOS <=0) %>%
  select(Pt_Study_no, DateTime_ICU_admit, DateTime_ICU_dc, ICU_LOS) %>% View()
shapiro.test(cr_first$ICU_LOS)
shapiro.test(scr_out$nocr$ICU_LOS)
summary(scr_out$nocr$ICU_LOS)
wilcox.test(cr_first$ICU_LOS, scr_out$nocr$ICU_LOS)
#Hosp LOS
summary(cr_first$Hosp_LOS)
summary(scr_out$nocr$Hosp_LOS)
shapiro.test(cr_first$Hosp_LOS)
shapiro.test(scr_out$nocr$Hosp_LOS)
wilcox.test(cr_first$Hosp_LOS, scr_out$nocr$Hosp_LOS)
#ICU mortality
sum(cr_first$Dc_ICU_Alive)
sum(is.na(cr_first$Dc_ICU_Alive))
nrow(cr_first) - sum(cr_first$Dc_ICU_Alive)
(nrow(cr_first) - sum(cr_first$Dc_ICU_Alive)) / nrow(cr_first) *100
table(scr_out$nocr$dc_ICU_alive)
sum(scr_out$nocr$dc_ICU_alive ==0)/nrow(scr_out$nocr)
ICUmortalitytable <- as.table(rbind(c(sum(na.omit(scr_out$nocr$dc_ICU_alive)),
                                      sum(cr_first$Dc_ICU_Alive)),
                                    c(sum(scr_out$nocr$dc_ICU_alive == "0"),
                                      sum(cr_first$Dc_ICU_Alive == "0")
                                    )))
dimnames(ICUmortalitytable) <- list(c("Alive", "Deceased"), c("nocr", "cr"))
ICUmortalitytable
fisher.test(ICUmortalitytable)
#hosp mortality
table(cr_first$Dc_Hosp_Alive)
sum(is.na(cr_first$Dc_Hosp_Alive))
table(scr_out$nocr$dc_hosp_alive)
sum(scr_out$nocr$dc_hosp_alive ==0)/nrow(scr_out$nocr)
Hospmortalitytable <- as.table(rbind(c(sum(scr_out$nocr$dc_hosp_alive),
                                       sum(na.omit(cr_first$Dc_Hosp_Alive))+sum(is.na(cr_first$Dc_Hosp_Alive))),
                                     c(sum(scr_out$nocr$dc_hosp_alive == "0"),
                                       sum(na.omit(cr_first$Dc_Hosp_Alive == "0")))
))
dimnames(Hospmortalitytable) <- list(c("Alive", "Deceased"), c("nocr", "cr"))
Hospmortalitytable
fisher.test(Hospmortalitytable)
#outcomes - oli 165 vs no oli 222 ----
#AKI
sum(oli_first$AKI_ICU)
sum(is.na(oli_first$AKI_ICU))
sum(oli_first$AKI_ICU)/nrow(oli_first) *100
sum(cronly_first$AKI_ICU)
sum(is.na(cronly_first$AKI_ICU))
sum(cronly_first$AKI_ICU == "0")
sum(cronly_first$AKI_ICU)/ 222 *100
AKItable <- as.table(rbind(c(sum(oli_first$AKI_ICU),
                             sum(cronly_first$AKI_ICU)),
                           c(nrow(oli_first) - sum(oli_first$AKI_ICU),
                             222 - sum(cronly_first$AKI_ICU))))
dimnames(AKItable) <- list(c("AKI", "no AKI"), c("oli", "no oli"))
AKItable
chisq.test(AKItable, correct = FALSE)
#aki stages
sum(is.na(oli_first$AKI_stage))
sum(na.omit(oli_first$AKI_stage == "1"))
sum(na.omit(oli_first$AKI_stage == "2"))
sum(na.omit(oli_first$AKI_stage == "3"))
nrow(oli_first) == sum(na.omit(oli_first$AKI_stage == "1")) +sum(na.omit(oli_first$AKI_stage == "2")) +
  sum(na.omit(oli_first$AKI_stage == "3")) + sum(is.na(oli_first$AKI_stage))
sum(na.omit(oli_first$AKI_stage == "1"))/sum(oli_first$AKI_ICU) *100
sum(na.omit(oli_first$AKI_stage == "2"))/sum(oli_first$AKI_ICU) *100
sum(na.omit(oli_first$AKI_stage == "3"))/sum(oli_first$AKI_ICU) *100
sum(is.na(cronly_first$AKI_stage))
sum(na.omit(cronly_first$AKI_stage == "1"))
sum(na.omit(cronly_first$AKI_stage == "2"))
sum(na.omit(cronly_first$AKI_stage == "3"))
AKIs1table <- as.table(rbind(c(sum(na.omit(oli_first$AKI_stage == "1")),
                               sum(na.omit(cronly_first$AKI_stage == "1"))),
                             c(sum(oli_first$AKI_ICU) - sum(na.omit(oli_first$AKI_stage == "1")),
                               sum(cronly_first$AKI_ICU) - sum(na.omit(cronly_first$AKI_stage == "1")))))
dimnames(AKIs1table) <- list(c("AKIs1", "not AKIs1"), c("oli", "no oli"))
AKIs1table
chisq.test(AKIs1table, correct = FALSE)
AKIs2table <- as.table(rbind(c(sum(na.omit(oli_first$AKI_stage == "2")),
                               sum(na.omit(cronly_first$AKI_stage == "2"))),
                             c(sum(oli_first$AKI_ICU) - sum(na.omit(oli_first$AKI_stage == "2")),
                               sum(cronly_first$AKI_ICU) - sum(na.omit(cronly_first$AKI_stage == "2")))))
dimnames(AKIs2table) <- list(c("AKIs2", "not AKIs2"), c("oli", "no oli"))
AKIs2table
chisq.test(AKIs2table, correct = FALSE)
AKIs3table <- as.table(rbind(c(sum(na.omit(oli_first$AKI_stage == "3")),
                               sum(na.omit(cronly_first$AKI_stage == "3"))),
                             c(sum(oli_first$AKI_ICU) - sum(na.omit(oli_first$AKI_stage == "3")),
                               sum(cronly_first$AKI_ICU) - sum(na.omit(cronly_first$AKI_stage == "3")))))
dimnames(AKIs3table) <- list(c("AKIs3", "not AKIs3"), c("oli", "no oli"))
AKIs3table
chisq.test(AKIs3table, correct = FALSE)
#hours from ICU adm to AKI
summary(oli_first$ICUadmtoAKIDx)
shapiro.test(oli_first$ICUadmtoAKIDx)
summary(cronly_first$ICUadmtoAKIDx)
shapiro.test(cronly_first$ICUadmtoAKIDx)
wilcox.test(cronly_first$ICUadmtoAKIDx, cr_first$ICUadmtoAKIDx)
#RRT
sum(na.omit(oli_first$RRT))
sum(is.na(oli_first$RRT))
sum(na.omit(oli_first$RRT == "0"))
sum(na.omit(oli_first$RRT))/sum(oli_first$AKI_ICU) *100
sum(is.na(cronly_first$RRT))
sum(na.omit(cronly_first$RRT))
RRTtable <- as.table(rbind(c(sum(na.omit(oli_first$RRT)),
                             sum(na.omit(cronly_first$RRT))),
                           c(sum(oli_first$AKI_ICU) - sum(na.omit(oli_first$RRT)),
                             sum(cronly_first$AKI_ICU) - sum(na.omit(cronly_first$RRT)))))
dimnames(RRTtable) <- list(c("RRT", "not RRT"), c("oli", "no oli"))
RRTtable
fisher.test(RRTtable)
#ICU LOS
summary(oli_first$ICU_LOS)
oli_first %>% filter(ICU_LOS <=0) %>%
  select(Pt_Study_no, DateTime_ICU_admit, DateTime_ICU_dc, ICU_LOS) %>% View()
shapiro.test(oli_first$ICU_LOS)
shapiro.test(scr_out$nool$ICU_LOS)
summary(scr_out$nool$ICU_LOS)
wilcox.test(oli_first$ICU_LOS, scr_out$nool$ICU_LOS)
#Hosp LOS
summary(oli_first$Hosp_LOS)
summary(scr_out$nool$Hosp_LOS)
shapiro.test(oli_first$Hosp_LOS)
shapiro.test(scr_out$nool$Hosp_LOS)
wilcox.test(oli_first$Hosp_LOS, scr_out$nool$Hosp_LOS)
#ICU mortality
sum(oli_first$Dc_ICU_Alive)
sum(is.na(oli_first$Dc_ICU_Alive))
nrow(oli_first) - sum(oli_first$Dc_ICU_Alive)
(nrow(oli_first) - sum(oli_first$Dc_ICU_Alive)) / nrow(oli_first) *100
table(scr_out$nool$dc_ICU_alive)
sum(scr_out$nool$dc_ICU_alive ==0)/nrow(scr_out$nool)
ICUmortalitytable <- as.table(rbind(c(sum(na.omit(scr_out$nool$dc_ICU_alive)),
                                      sum(oli_first$Dc_ICU_Alive)),
                                    c(sum(scr_out$nool$dc_ICU_alive == "0"),
                                      sum(oli_first$Dc_ICU_Alive == "0")
                                    )))
dimnames(ICUmortalitytable) <- list(c("Alive", "Deceased"), c("nooli", "oli"))
ICUmortalitytable
chisq.test(ICUmortalitytable, correct = FALSE)
#hosp mortality
table(oli_first$Dc_Hosp_Alive)
sum(is.na(oli_first$Dc_Hosp_Alive))
table(scr_out$nool$dc_hosp_alive)
sum(scr_out$nool$dc_hosp_alive ==0)/nrow(scr_out$nool)
Hospmortalitytable <- as.table(rbind(c(sum(scr_out$nool$dc_hosp_alive)-2,
                                       sum(na.omit(oli_first$Dc_Hosp_Alive))),
                                     c(sum(scr_out$nool$dc_hosp_alive == "0"),
                                       sum(na.omit(oli_first$Dc_Hosp_Alive == "0")))))
dimnames(Hospmortalitytable) <- list(c("Alive", "Deceased"), c("nooli", "oli"))
Hospmortalitytable
chisq.test(Hospmortalitytable, correct = FALSE)
#outcomes cr with aki 185 vs cr no aki 94 ----
#ICU LOS
summary(cr_first_AKI$ICU_LOS)
shapiro.test(cr_first_AKI$ICU_LOS)
summary(cr_first_noAKI$ICU_LOS)
shapiro.test(cr_first_noAKI$ICU_LOS)
wilcox.test(cr_first_AKI$ICU_LOS, cr_first_noAKI$ICU_LOS)
#Hosp LOS
summary(cr_first_AKI$Hosp_LOS)
shapiro.test(cr_first_AKI$Hosp_LOS)
wilcox.test(cr_first_AKI$Hosp_LOS, cr_first_noAKI$Hosp_LOS)
#ICU mort
sum(cr_first_AKI$Dc_ICU_Alive)
sum(is.na(cr_first_AKI$Dc_ICU_Alive))
sum(cr_first_AKI$Dc_ICU_Alive == "0")
sum(cr_first_AKI$Dc_ICU_Alive == "0")/nrow(cr_first_AKI) *100
ICUmortalitytable <- as.table(rbind(c(sum(na.omit(cr_first_noAKI$Dc_ICU_Alive)),
                                      sum(cr_first_AKI$Dc_ICU_Alive)),
                                    c(sum(cr_first_noAKI$Dc_ICU_Alive == "0"),
                                      sum(cr_first_AKI$Dc_ICU_Alive == "0")
                                    )))
dimnames(ICUmortalitytable) <- list(c("Alive", "Deceased"), c("no AKI", "AKI"))
ICUmortalitytable
chisq.test(ICUmortalitytable, correct = FALSE)
#Hosp mort
sum(na.omit(cr_first_AKI$Dc_Hosp_Alive))
sum(is.na(cr_first_AKI$Dc_Hosp_Alive))
sum(na.omit(cr_first_AKI$Dc_Hosp_Alive == "0"))
sum(na.omit(cr_first_AKI$Dc_Hosp_Alive == "0"))/nrow(cr_first_AKI) *100
sum(na.omit(cr_first_noAKI$Dc_Hosp_Alive))
sum(is.na(cr_first_noAKI$Dc_Hosp_Alive))
sum(na.omit(cr_first_noAKI$Dc_Hosp_Alive == "0"))
sum(na.omit(cr_first_noAKI$Dc_Hosp_Alive == "0"))/nrow(cr_first_noAKI) *100
Hospmortalitytable <- as.table(rbind(c(sum(na.omit(cr_first_noAKI$Dc_Hosp_Alive)),
                                       sum(na.omit(cr_first_AKI$Dc_Hosp_Alive))),
                                     c(sum(na.omit(cr_first_noAKI$Dc_Hosp_Alive == "0")),
                                       sum(na.omit(cr_first_AKI$Dc_Hosp_Alive == "0"))
                                     )))
dimnames(Hospmortalitytable) <- list(c("Alive", "Deceased"), c("no AKI", "AKI"))
Hospmortalitytable
chisq.test(Hospmortalitytable, correct = FALSE)
#outcomes oli with aki 128 vs oli no aki 37 ----
#ICU LOS
summary(oli_first_AKI$ICU_LOS)
shapiro.test(oli_first_AKI$ICU_LOS)
summary(oli_first_noAKI$ICU_LOS)
shapiro.test(oli_first_noAKI$ICU_LOS)
wilcox.test(oli_first_AKI$ICU_LOS, oli_first_noAKI$ICU_LOS)
#Hosp LOS
summary(oli_first_AKI$Hosp_LOS)
shapiro.test(oli_first_AKI$Hosp_LOS)
summary(oli_first_noAKI$Hosp_LOS)
wilcox.test(oli_first_AKI$Hosp_LOS, oli_first_noAKI$Hosp_LOS)
#ICU mort
sum(oli_first_AKI$Dc_ICU_Alive)
sum(is.na(oli_first_AKI$Dc_ICU_Alive))
sum(oli_first_AKI$Dc_ICU_Alive == "0")
sum(oli_first_AKI$Dc_ICU_Alive == "0")/nrow(oli_first_AKI) *100
ICUmortalitytable <- as.table(rbind(c(sum(na.omit(oli_first_noAKI$Dc_ICU_Alive)),
                                      sum(oli_first_AKI$Dc_ICU_Alive)),
                                    c(sum(oli_first_noAKI$Dc_ICU_Alive == "0"),
                                      sum(oli_first_AKI$Dc_ICU_Alive == "0")
                                    )))
dimnames(ICUmortalitytable) <- list(c("Alive", "Deceased"), c("no AKI", "AKI"))
ICUmortalitytable
fisher.test(ICUmortalitytable)
#Hosp mort
sum(na.omit(oli_first_AKI$Dc_Hosp_Alive))
sum(is.na(oli_first_AKI$Dc_Hosp_Alive))
sum(na.omit(oli_first_AKI$Dc_Hosp_Alive == "0"))
sum(na.omit(oli_first_AKI$Dc_Hosp_Alive == "0"))/nrow(oli_first_AKI) *100
Hospmortalitytable <- as.table(rbind(c(sum(na.omit(oli_first_noAKI$Dc_Hosp_Alive)),
                                       sum(na.omit(oli_first_AKI$Dc_Hosp_Alive))+sum(is.na(oli_first_AKI$Dc_Hosp_Alive))),
                                     c(sum(na.omit(oli_first_noAKI$Dc_Hosp_Alive == "0")),
                                       sum(na.omit(oli_first_AKI$Dc_Hosp_Alive == "0"))
                                     )))
dimnames(Hospmortalitytable) <- list(c("Alive", "Deceased"), c("no AKI", "AKI"))
Hospmortalitytable
chisq.test(Hospmortalitytable, correct = FALSE)
#outcomes - cr pts 279 vs neither 74 ----
#ICU LOS
summary(scr_out$neit$ICU_LOS)
shapiro.test(scr_out$neit$ICU_LOS)
wilcox.test(cr_first$ICU_LOS, scr_out$neit$ICU_LOS)
#hosp LOS
summary(scr_out$neit$Hosp_LOS)
shapiro.test(scr_out$neit$Hosp_LOS)
wilcox.test(cr_first$Hosp_LOS, scr_out$neit$Hosp_LOS)
#ICU mortality
ICUmortalitytable <- as.table(rbind(c(sum(na.omit(scr_out$neit$dc_ICU_alive)),
                                      sum(cr_first$Dc_ICU_Alive)),
                                    c(sum(scr_out$neit$dc_ICU_alive == "0"),
                                      sum(cr_first$Dc_ICU_Alive == "0")
                                    )))
dimnames(ICUmortalitytable) <- list(c("Alive", "Deceased"), c("neit", "cr"))
ICUmortalitytable
fisher.test(ICUmortalitytable)
#hosp mort
Hospmortalitytable <- as.table(rbind(c(sum(scr_out$neit$dc_hosp_alive),
                                       sum(na.omit(cr_first$Dc_Hosp_Alive))+3),
                                     c(sum(scr_out$neit$dc_hosp_alive == "0"),
                                       sum(na.omit(cr_first$Dc_Hosp_Alive == "0")))
))
dimnames(Hospmortalitytable) <- list(c("Alive", "Deceased"), c("neit", "cr"))
Hospmortalitytable
fisher.test(Hospmortalitytable)
#outcomes - oli 165 vs neither 74 ----
#ICU LOS
wilcox.test(oli_first$ICU_LOS, scr_out$neit$ICU_LOS)
#hospital LOS
wilcox.test(oli_first$Hosp_LOS, scr_out$neit$Hosp_LOS)
#ICU mortality
ICUmortalitytable <- as.table(rbind(c(sum(na.omit(scr_out$neit$dc_ICU_alive)),
                                      sum(oli_first$Dc_ICU_Alive)),
                                    c(sum(scr_out$neit$dc_ICU_alive == "0"),
                                      sum(oli_first$Dc_ICU_Alive == "0")
                                    )))
dimnames(ICUmortalitytable) <- list(c("Alive", "Deceased"), c("neit", "oli"))
ICUmortalitytable
fisher.test(ICUmortalitytable)
#hosp mortality
Hospmortalitytable <- as.table(rbind(c(sum(scr_out$neit$dc_hosp_alive),
                                       sum(na.omit(oli_first$Dc_Hosp_Alive))),
                                     c(sum(scr_out$neit$dc_hosp_alive == "0"),
                                       sum(na.omit(oli_first$Dc_Hosp_Alive == "0")))
))
dimnames(Hospmortalitytable) <- list(c("Alive", "Deceased"), c("neit", "oli"))
Hospmortalitytable
fisher.test(Hospmortalitytable)
#outcomes - both 131 vs neither 74 ----
#ICU LOS
summary(creoliboth1$ICU_LOS)
shapiro.test(creoliboth1$ICU_LOS)
wilcox.test(creoliboth1$ICU_LOS, scr_out$neit$ICU_LOS)
#hosp LOS
summary(creoliboth1$Hosp_LOS)
shapiro.test(creoliboth1$Hosp_LOS)
wilcox.test(creoliboth1$Hosp_LOS, scr_out$neit$Hosp_LOS)
#ICU mortality
sum(creoliboth1$Dc_ICU_Alive)
sum(is.na(creoliboth1$Dc_ICU_Alive))
nrow(creoliboth1) - sum(creoliboth1$Dc_ICU_Alive)
(nrow(creoliboth1) - sum(creoliboth1$Dc_ICU_Alive))/nrow(creoliboth1) *100
ICUmortalitytable <- as.table(rbind(c(sum(na.omit(scr_out$neit$dc_ICU_alive)),
                                      sum(creoliboth1$Dc_ICU_Alive)),
                                    c(sum(scr_out$neit$dc_ICU_alive == "0"),
                                      sum(creoliboth1$Dc_ICU_Alive == "0")
                                    )))
dimnames(ICUmortalitytable) <- list(c("Alive", "Deceased"), c("neit", "both"))
ICUmortalitytable
fisher.test(ICUmortalitytable)
#hospital mortality
sum(na.omit(creoliboth1$Dc_Hosp_Alive))
sum(is.na(creoliboth1$Dc_Hosp_Alive))
creoliboth1 %>% filter(Dc_Hosp_Alive == 0) %>% nrow()
nrow(creoliboth1) - sum(na.omit(creoliboth1$Dc_Hosp_Alive)) - sum(is.na(creoliboth1$Dc_Hosp_Alive))
(nrow(creoliboth1) - sum(na.omit(creoliboth1$Dc_Hosp_Alive)) - sum(is.na(creoliboth1$Dc_Hosp_Alive)))/
  nrow(creoliboth1) *100
Hospmortalitytable <- as.table(rbind(c(sum(scr_out$neit$dc_hosp_alive),
                                       sum(na.omit(creoliboth1$Dc_Hosp_Alive))),
                                     c(sum(scr_out$neit$dc_hosp_alive == "0"),
                                       sum(na.omit(creoliboth1$Dc_Hosp_Alive == "0")))
))
dimnames(Hospmortalitytable) <- list(c("Alive", "Deceased"), c("neit", "both"))
Hospmortalitytable
fisher.test(Hospmortalitytable)
#--- Time from ICU adm to first cr/olig episode ----
#all cr change pts n=279 -> invalid data n=4 -> analysis n=275
cr_first1 <- cr_full %>% filter(Pt_Study_no != "LT1") %>%
  filter(Cr_ICU == 1) %>%
  group_by(Pt_Study_no) %>%
  top_n(-1, Cr_epis_no) %>%
  mutate(Time_First_Ep =
           as.duration(DateTime_ICU_admit %--% DateTime_Cr_epis) / dhours(1))
cr_first1$Time_First_Ep>0
nrow(cr_first1)
summary(cr_first1$Time_First_Ep)
#all oliguria pts n=165 -> invalid data n=1 -> analysis n=164
oli_first1 <- oli_full %>% filter(Pt_Study_no != "L1") %>%
  group_by(Pt_Study_no) %>%
  top_n(-1, Olig_epis_no) %>%
  mutate(Time_First_Ep =
           as.duration(DateTime_ICU_admit %--% DateTime_olig_epis) / dhours(1))
oli_first1$Time_First_Ep>0
nrow(oli_first1)
summary(oli_first1$Time_First_Ep)
wilcox.test(cr_first1$Time_First_Ep, oli_first1$Time_First_Ep)
#cr change only pts n=148 -> invalid data n=3 -> analysis n=145
cronly_first1 <- cronly %>% filter(Pt_Study_no != "LT1") %>%
  filter(Cr_ICU == 1) %>%
  group_by(Pt_Study_no) %>%
  top_n(-1, Cr_epis_no) %>%
  mutate(Time_First_Ep =
           as.duration(DateTime_ICU_admit %--% DateTime_Cr_epis) / dhours(1))
cronly_first1$Time_First_Ep>0
nrow(cronly_first1)
summary(cronly_first1$Time_First_Ep)
#oli only pts n=34
olionly_first <- olionly_first %>%
  mutate(Time_First_Ep =
           as.duration(DateTime_ICU_admit %--% DateTime_olig_epis) / dhours(1))
olionly_first$Time_First_Ep>0
summary(olionly_first$Time_First_Ep)
#compare cr only n=145 to oli only n=34
wilcox.test(cronly_first1$Time_First_Ep, olionly_first$Time_First_Ep)
#cr change pts with aki n=185 vs without aki n=94 -> invalid data -> 184 v 91
cr_first1aki <- cr_first1 %>% filter(AKI_ICU == 1)
cr_first1noaki <- cr_first1 %>% filter(AKI_ICU == 0)
nrow(cr_first1aki)
nrow(cr_first1noaki)
summary(cr_first1aki$Time_First_Ep)
summary(cr_first1noaki$Time_First_Ep)
wilcox.test(cr_first1aki$Time_First_Ep, cr_first1noaki$Time_First_Ep)
#olig pts with aki n=128 vs without aki n=37 -> invalid data -> 128 vs 36
olifirst1aki <- oli_first1 %>% filter(AKI_ICU == 1)
olifirst1noaki <- oli_first1 %>% filter(AKI_ICU == 0)
nrow(olifirst1aki)
nrow(olifirst1noaki)
summary(olifirst1aki$Time_First_Ep)
summary(olifirst1noaki$Time_First_Ep)
wilcox.test(olifirst1aki$Time_First_Ep, olifirst1noaki$Time_First_Ep)
#pts with both cr and oli n=131 vs cr change only n= 148 -> 131 vs 145
creboth1 <- creboth %>% filter(Pt_Study_no != "LT1") %>%
  filter(Cr_ICU == 1) %>%
  group_by(Pt_Study_no) %>%
  top_n(-1, Cr_epis_no) %>%
  mutate(Time_First_Ep =
           as.duration(DateTime_ICU_admit %--% DateTime_Cr_epis) / dhours(1))
creboth1$Time_First_Ep>0
nrow(creboth1)
summary(creboth1$Time_First_Ep)
wilcox.test(creboth1$Time_First_Ep, cronly_first1$Time_First_Ep)
#pts with both cr and oli n=131 vs oli only n=34 -> 130 vs 34
oliboth1 <- oliboth %>% filter(Pt_Study_no != "L1") %>%
  group_by(Pt_Study_no) %>%
  top_n(-1, Olig_epis_no) %>%
  mutate(Time_First_Ep =
           as.duration(DateTime_ICU_admit %--% DateTime_olig_epis) / dhours(1))
oliboth1$Time_First_Ep>0
nrow(oliboth1)
summary(oliboth1$Time_First_Ep)
wilcox.test(oliboth1$Time_First_Ep, olionly_first$Time_First_Ep)
#--- SE SP PPV NPV ----
sum(creoliboth1$AKI_ICU)
sum(is.na(creoliboth1$AKI_ICU))
sum(creoliboth1$AKI_ICU=="0")
crtable <- as.table(rbind(c(185, 94), c(27, 81)))
dimnames(crtable) <- list(c("cr ep", "no cr ep"), c("AKI", "no AKI"))
crtable
summary(epi.tests(crtable, conf.level=0.95))
olitable <- as.table(rbind(c(128, 37), c(84, 138)))
dimnames(olitable) <- list(c("olig ep", "no olig ep"), c("AKI", "no AKI"))
olitable
summary(epi.tests(olitable, conf.level=0.95))
bothtable <- as.table(rbind(c(101, 30), c(111, 145)))
dimnames(bothtable) <- list(c("olig & cr ep", "olig only or cr only or neither"), c("AKI", "no AKI"))
bothtable
summary(epi.tests(bothtable, conf.level=0.95))
craki23in24h <- as.table(rbind(c(54, 279-54), c(7, 108-7)))
dimnames(craki23in24h) <- list(c("cr ep", "no cr ep"), c("aki23in24h", "no aki23in24h"))
craki23in24h
summary(epi.tests(craki23in24h, conf.level=0.95))
craki23in12h <- as.table(rbind(c(45, 234), c(7, 101)))
dimnames(craki23in12h) <- list(c("cr ep", "no cr ep"), c("aki23in12h", "no aki23in12h"))
craki23in12h
summary(epi.tests(craki23in12h, conf.level=0.95))
craki23in48h <- as.table(rbind(c(58, 221), c(7, 101)))
dimnames(craki23in48h) <- list(c("cr ep", "no cr ep"), c("aki23in48h", "no aki23in48h"))
craki23in48h
summary(epi.tests(craki23in48h, conf.level=0.95))
craki23 <- as.table(rbind(c(61, 218), c(7, 101)))
dimnames(craki23) <- list(c("cr ep", "no cr ep"), c("aki23", "no aki23"))
craki23
summary(epi.tests(craki23, conf.level=0.95))
craki12h <- as.table(rbind(c(117, 162), c(23, 85)))
dimnames(craki12h) <- list(c("cr ep", "no cr ep"), c("aki12h", "no aki12h"))
craki12h
summary(epi.tests(craki12h, conf.level=0.95))
craki24h <- as.table(rbind(c(148, 131), c(24, 84)))
dimnames(craki24h) <- list(c("cr ep", "no cr ep"), c("aki24h", "no aki24h"))
craki24h
summary(epi.tests(craki24h, conf.level=0.95))
craki48h <- as.table(rbind(c(167, 112), c(26, 82)))
dimnames(craki48h) <- list(c("cr ep", "no cr ep"), c("aki48h", "no aki48h"))
craki48h
summary(epi.tests(craki48h, conf.level=0.95))
#--- NUMBER OF EPISODES ----
#creatinine----
table(cr_last$Cr_epis_no)
table(cr_last_aki$Cr_epis_no)
table(cr_last_no_aki$Cr_epis_no)
creptable <- as.table(rbind(c(sum(cr_last_aki$Cr_epis_no == "1"),
                              sum(cr_last_no_aki$Cr_epis_no == "1")),
                            c(nrow(cr_last_aki) - sum(cr_last_aki$Cr_epis_no == "1"),
                              nrow(cr_last_no_aki) - sum(cr_last_no_aki$Cr_epis_no == "1"))))
dimnames(creptable) <- list(c("1 ep", "not 1 ep"), c("AKI", "no AKI"))
creptable
chisq.test(creptable, correct = FALSE)
creptable <- as.table(rbind(c(sum(cr_last_aki$Cr_epis_no == "2"),
                              sum(cr_last_no_aki$Cr_epis_no == "2")),
                            c(nrow(cr_last_aki) - sum(cr_last_aki$Cr_epis_no == "2"),
                              nrow(cr_last_no_aki) - sum(cr_last_no_aki$Cr_epis_no == "2"))))
dimnames(creptable) <- list(c("2 ep", "not 2 ep"), c("AKI", "no AKI"))
creptable
chisq.test(creptable, correct = FALSE)
creptable <- as.table(rbind(c(sum(cr_last_aki$Cr_epis_no == "3"),
                              sum(cr_last_no_aki$Cr_epis_no == "3")),
                            c(nrow(cr_last_aki) - sum(cr_last_aki$Cr_epis_no == "3"),
                              nrow(cr_last_no_aki) - sum(cr_last_no_aki$Cr_epis_no == "3"))))
dimnames(creptable) <- list(c("3 ep", "not 3 ep"), c("AKI", "no AKI"))
creptable
chisq.test(creptable, correct = FALSE)
creptable <- as.table(rbind(c(sum(cr_last_aki$Cr_epis_no == "4"),
                              sum(cr_last_no_aki$Cr_epis_no == "4")),
                            c(nrow(cr_last_aki) - sum(cr_last_aki$Cr_epis_no == "4"),
                              nrow(cr_last_no_aki) - sum(cr_last_no_aki$Cr_epis_no == "4"))))
dimnames(creptable) <- list(c("4 ep", "not 4 ep"), c("AKI", "no AKI"))
creptable
chisq.test(creptable, correct = FALSE)
creptable <- as.table(rbind(c(sum(cr_last_aki$Cr_epis_no == "5"),
                              sum(cr_last_no_aki$Cr_epis_no == "5")),
                            c(nrow(cr_last_aki) - sum(cr_last_aki$Cr_epis_no == "5"),
                              nrow(cr_last_no_aki) - sum(cr_last_no_aki$Cr_epis_no == "5"))))
dimnames(creptable) <- list(c("5 ep", "not 5 ep"), c("AKI", "no AKI"))
creptable
fisher.test(creptable)
creptable <- as.table(rbind(c(sum(cr_last_aki$Cr_epis_no == "6"),
                              sum(cr_last_no_aki$Cr_epis_no == "6")),
                            c(nrow(cr_last_aki) - sum(cr_last_aki$Cr_epis_no == "6"),
                              nrow(cr_last_no_aki) - sum(cr_last_no_aki$Cr_epis_no == "6"))))
dimnames(creptable) <- list(c("6 ep", "not 6 ep"), c("AKI", "no AKI"))
creptable
fisher.test(creptable)
creptable <- as.table(rbind(c(sum(cr_last_aki$Cr_epis_no == "7"),
                              sum(cr_last_no_aki$Cr_epis_no == "7")),
                            c(nrow(cr_last_aki) - sum(cr_last_aki$Cr_epis_no == "7"),
                              nrow(cr_last_no_aki) - sum(cr_last_no_aki$Cr_epis_no == "7"))))
dimnames(creptable) <- list(c("7 ep", "not 7 ep"), c("AKI", "no AKI"))
creptable
fisher.test(creptable)
creptable <- as.table(rbind(c(sum(cr_last_aki$Cr_epis_no == "8"),
                              sum(cr_last_no_aki$Cr_epis_no == "8")),
                            c(nrow(cr_last_aki) - sum(cr_last_aki$Cr_epis_no == "8"),
                              nrow(cr_last_no_aki) - sum(cr_last_no_aki$Cr_epis_no == "8"))))
dimnames(creptable) <- list(c("8 ep", "not 8 ep"), c("AKI", "no AKI"))
creptable
fisher.test(creptable)
creptable <- as.table(rbind(c(sum(cr_last_aki$Cr_epis_no == "10"),
                              sum(cr_last_no_aki$Cr_epis_no == "10")),
                            c(nrow(cr_last_aki) - sum(cr_last_aki$Cr_epis_no == "10"),
                              nrow(cr_last_no_aki) - sum(cr_last_no_aki$Cr_epis_no == "10"))))
dimnames(creptable) <- list(c("10 ep", "not 10 ep"), c("AKI", "no AKI"))
creptable
fisher.test(creptable)
#oliguria ----
table(oli_last$Olig_epis_no)
table(oli_last_aki$Olig_epis_no)
table(oli_last_no_aki$Olig_epis_no)
olieptable <- as.table(rbind(c(sum(oli_last_aki$Olig_epis_no == "1"),
                               sum(oli_last_no_aki$Olig_epis_no == "1")),
                             c(nrow(oli_last_aki) - sum(oli_last_aki$Olig_epis_no == "1"),
                               nrow(oli_last_no_aki) - sum(oli_last_no_aki$Olig_epis_no == "1"))))
dimnames(olieptable) <- list(c("1 ep", "not 1 ep"), c("AKI", "no AKI"))
olieptable
chisq.test(olieptable, correct = FALSE)
olieptable <- as.table(rbind(c(sum(oli_last_aki$Olig_epis_no == "2"),
                               sum(oli_last_no_aki$Olig_epis_no == "2")),
                             c(nrow(oli_last_aki) - sum(oli_last_aki$Olig_epis_no == "2"),
                               nrow(oli_last_no_aki) - sum(oli_last_no_aki$Olig_epis_no == "2"))))
dimnames(olieptable) <- list(c("2 ep", "not 2 ep"), c("AKI", "no AKI"))
olieptable
chisq.test(olieptable, correct = FALSE)
olieptable <- as.table(rbind(c(sum(oli_last_aki$Olig_epis_no == "3"),
                               sum(oli_last_no_aki$Olig_epis_no == "3")),
                             c(nrow(oli_last_aki) - sum(oli_last_aki$Olig_epis_no == "3"),
                               nrow(oli_last_no_aki) - sum(oli_last_no_aki$Olig_epis_no == "3"))))
dimnames(olieptable) <- list(c("3 ep", "not 3 ep"), c("AKI", "no AKI"))
olieptable
fisher.test(olieptable)
olieptable <- as.table(rbind(c(sum(oli_last_aki$Olig_epis_no == "4"),
                               sum(oli_last_no_aki$Olig_epis_no == "4")),
                             c(nrow(oli_last_aki) - sum(oli_last_aki$Olig_epis_no == "4"),
                               nrow(oli_last_no_aki) - sum(oli_last_no_aki$Olig_epis_no == "4"))))
dimnames(olieptable) <- list(c("4 ep", "not 4 ep"), c("AKI", "no AKI"))
olieptable
fisher.test(olieptable)
#AUCROC
#cr
roc(cr_last$AKI_ICU, cr_last$Cr_epis_no)
wilcox.test(Cr_epis_no ~ AKI_ICU, cr_last)
#oli
roc(oli_last$AKI_ICU, oli_last$Olig_epis_no)
wilcox.test(Olig_epis_no ~ AKI_ICU, oli_last)
#--- AUROCs ----
#making function for streamlining auc analysis ----
rocfun <- function(df, col_pre, col_out) {
  predictor <- df[[col_pre]]
  outcome   <- df[[col_out]]
  # ROC
  roc_out   <- roc(outcome, predictor)
  cat(paste("Predictor:", col_pre, "| Outcome:", col_out, "\n",
             "Controls:", length(roc_out$controls), "\n",
             "Cases   :", length(roc_out$cases), "\n",
             "AUC     :", sprintf("%0.4f", roc_out$auc), "\n"))
  # Wilcox
  wilc_out <- wilcox.test(predictor, outcome)
  wilc_out <- wilcox.test(as.formula(paste0(col_pre, "~", col_out)), df)
  cat(paste(" Wilcox p:", sprintf("%0.4f", wilc_out$p.value), "\n"))
  # Output
  return(data.frame(
    predictor = col_pre,
    outcome   = col_out,
    controls  = length(roc_out$controls),
    cases     = length(roc_out$cases),
    auc       = roc_out$auc,
    wilcoxp   = wilc_out$p.value))
}

# df <- cr_first; col_pre <- "delta_cr"; col_out <- "AKI_ICU"
# rocfun(cr_first, "delta_cr", "AKI_ICU")

predictors <- c("delta_cr")
outcomes   <- c("AKI_ICU",  "aki2or3")

rocoutdf <- data.frame()
for (i in 1:length(predictors)) {
  for (j in 1:length(outcomes)) {
    rocout <- rocfun(cr_first, predictors[i], outcomes[j])
    rocoutdf <- rbind(rocoutdf, rocout)
  }
}
#first cr ep ----
#delta cr
roc(cr_first$AKI_ICU, cr_first$delta_cr) # roc(AKI_ICU~delta_cr, cr_first)
wilcox.test(delta_cr~AKI_ICU, cr_first)  # wilcox.test(cr_first$AKI_ICU, cr_first$delta_cr)
roc(cr_first$aki2or3, cr_first$delta_cr)
wilcox.test(delta_cr~aki2or3, cr_first)
roc(cr_first$aki12h, cr_first$delta_cr)
wilcox.test(delta_cr~aki12h, cr_first)
roc(cr_first$aki24h, cr_first$delta_cr)
wilcox.test(delta_cr~aki24h, cr_first)
roc(cr_first$aki48h, cr_first$delta_cr)
wilcox.test(delta_cr~aki48h, cr_first)
roc(cr_first$aki2or3in12h, cr_first$delta_cr)
wilcox.test(delta_cr~aki2or3in12h, cr_first)
roc(cr_first$aki2or3in24h, cr_first$delta_cr)
wilcox.test(delta_cr~aki2or3in24h, cr_first)
roc(cr_first$aki2or3in48h, cr_first$delta_cr)
wilcox.test(delta_cr~aki2or3in48h, cr_first)
#percent delta cr
roc(cr_first$AKI_ICU, cr_first$percent_delta_cr)
wilcox.test(percent_delta_cr~AKI_ICU, cr_first)
roc(cr_first$aki2or3, cr_first$percent_delta_cr)
wilcox.test(percent_delta_cr~aki2or3, cr_first)
roc(cr_first$aki12h, cr_first$percent_delta_cr)
wilcox.test(percent_delta_cr~aki12h, cr_first)
roc(cr_first$aki24h, cr_first$percent_delta_cr)
wilcox.test(percent_delta_cr~aki24h, cr_first)
roc(cr_first$aki48h, cr_first$percent_delta_cr)
wilcox.test(percent_delta_cr~aki48h, cr_first)
roc(cr_first$aki2or3in12h, cr_first$percent_delta_cr)
wilcox.test(percent_delta_cr~aki2or3in12h, cr_first)
roc(cr_first$aki2or3in24h, cr_first$percent_delta_cr)
wilcox.test(percent_delta_cr~aki2or3in24h, cr_first)
roc(cr_first$aki2or3in48h, cr_first$percent_delta_cr)
wilcox.test(percent_delta_cr~aki2or3in48h, cr_first)
#last cr ep ----
#delta cr
roc(cr_last$AKI_ICU, cr_last$delta_cr)
wilcox.test(delta_cr~AKI_ICU, cr_last)
roc(cr_last$aki2or3, cr_last$delta_cr)
wilcox.test(delta_cr~aki2or3, cr_last)
roc(cr_last$aki12h, cr_last$delta_cr)
wilcox.test(delta_cr~aki12h, cr_last)
roc(cr_last$aki24h, cr_last$delta_cr)
wilcox.test(delta_cr~aki24h, cr_last)
roc(cr_last$aki48h, cr_last$delta_cr)
wilcox.test(delta_cr~aki48h, cr_last)
roc(cr_last$aki2or3in12h, cr_last$delta_cr)
wilcox.test(delta_cr~aki2or3in12h, cr_last)
roc(cr_last$aki2or3in24h, cr_last$delta_cr)
wilcox.test(delta_cr~aki2or3in24h, cr_last)
roc(cr_last$aki2or3in48h, cr_last$delta_cr)
wilcox.test(delta_cr~aki2or3in48h, cr_last)
#percent delta cr
roc(cr_last$AKI_ICU, cr_last$percent_delta_cr)
wilcox.test(percent_delta_cr~AKI_ICU, cr_last)
roc(cr_last$aki2or3, cr_last$percent_delta_cr)
wilcox.test(percent_delta_cr~aki2or3, cr_last)
roc(cr_last$aki12h, cr_last$percent_delta_cr)
wilcox.test(percent_delta_cr~aki12h, cr_last)
roc(cr_last$aki24h, cr_last$percent_delta_cr)
wilcox.test(percent_delta_cr~aki24h, cr_last)
roc(cr_last$aki48h, cr_last$percent_delta_cr)
wilcox.test(percent_delta_cr~aki48h, cr_last)
roc(cr_last$aki2or3in12h, cr_last$percent_delta_cr)
wilcox.test(percent_delta_cr~aki2or3in12h, cr_last)
roc(cr_last$aki2or3in24h, cr_last$percent_delta_cr)
wilcox.test(percent_delta_cr~aki2or3in24h, cr_last)
roc(cr_last$aki2or3in48h, cr_last$percent_delta_cr)
wilcox.test(percent_delta_cr~aki2or3in48h, cr_last)
#all cr eps ----
#delta cr
roc(cr_full$AKI_ICU, cr_full$delta_cr)
wilcox.test(delta_cr~AKI_ICU, cr_full)
roc(cr_full$aki2or3, cr_full$delta_cr)
wilcox.test(delta_cr~aki2or3, cr_full)
roc(cr_full$aki12h, cr_full$delta_cr)
wilcox.test(delta_cr~aki12h, cr_full)
roc(cr_full$aki24h, cr_full$delta_cr)
wilcox.test(delta_cr~aki24h, cr_full)
roc(cr_full$aki48h, cr_full$delta_cr)
wilcox.test(delta_cr~aki48h, cr_full)
roc(cr_full$aki2or3in12h, cr_full$delta_cr)
wilcox.test(delta_cr~aki2or3in12h, cr_full)
roc(cr_full$aki2or3in24h, cr_full$delta_cr)
wilcox.test(delta_cr~aki2or3in24h, cr_full)
roc(cr_full$aki2or3in48h, cr_full$delta_cr)
wilcox.test(delta_cr~aki2or3in48h, cr_full)
#percent delta cr
roc(cr_full$AKI_ICU, cr_full$percent_delta_cr)
wilcox.test(percent_delta_cr~AKI_ICU, cr_full)
roc(cr_full$aki2or3, cr_full$percent_delta_cr)
wilcox.test(percent_delta_cr~aki2or3, cr_full)
roc(cr_full$aki12h, cr_full$percent_delta_cr)
wilcox.test(percent_delta_cr~aki12h, cr_full)
roc(cr_full$aki24h, cr_full$percent_delta_cr)
wilcox.test(percent_delta_cr~aki24h, cr_full)
roc(cr_full$aki48h, cr_full$percent_delta_cr)
wilcox.test(percent_delta_cr~aki48h, cr_full)
roc(cr_full$aki2or3in12h, cr_full$percent_delta_cr)
wilcox.test(percent_delta_cr~aki2or3in12h, cr_full)
roc(cr_full$aki2or3in24h, cr_full$percent_delta_cr)
wilcox.test(percent_delta_cr~aki2or3in24h, cr_full)
roc(cr_full$aki2or3in48h, cr_full$percent_delta_cr)
wilcox.test(percent_delta_cr~aki2or3in48h, cr_full)
#cr eps with oli at time T0 ----
#delta cr
roc(oliattimeofcrep$AKI_ICU, oliattimeofcrep$delta_cr)
wilcox.test(delta_cr~AKI_ICU, oliattimeofcrep)
roc(oliattimeofcrep$aki2or3, oliattimeofcrep$delta_cr)
wilcox.test(delta_cr~aki2or3, oliattimeofcrep)
roc(oliattimeofcrep$aki12h, oliattimeofcrep$delta_cr)
wilcox.test(delta_cr~aki12h, oliattimeofcrep)
roc(oliattimeofcrep$aki24h, oliattimeofcrep$delta_cr)
wilcox.test(delta_cr~aki24h, oliattimeofcrep)
roc(oliattimeofcrep$aki48h, oliattimeofcrep$delta_cr)
wilcox.test(delta_cr~aki48h, oliattimeofcrep)
roc(oliattimeofcrep$aki2or3in12h, oliattimeofcrep$delta_cr)
wilcox.test(delta_cr~aki2or3in12h, oliattimeofcrep)
roc(oliattimeofcrep$aki2or3in24h, oliattimeofcrep$delta_cr)
wilcox.test(delta_cr~aki2or3in24h, oliattimeofcrep)
roc(oliattimeofcrep$aki2or3in48h, oliattimeofcrep$delta_cr)
wilcox.test(delta_cr~aki2or3in48h, oliattimeofcrep)
#percent delta cr
roc(oliattimeofcrep$AKI_ICU, oliattimeofcrep$percent_delta_cr)
wilcox.test(percent_delta_cr~AKI_ICU, oliattimeofcrep)
roc(oliattimeofcrep$aki2or3, oliattimeofcrep$percent_delta_cr)
wilcox.test(percent_delta_cr~aki2or3, oliattimeofcrep)
roc(oliattimeofcrep$aki12h, oliattimeofcrep$percent_delta_cr)
wilcox.test(percent_delta_cr~aki12h, oliattimeofcrep)
roc(oliattimeofcrep$aki24h, oliattimeofcrep$percent_delta_cr)
wilcox.test(percent_delta_cr~aki24h, oliattimeofcrep)
roc(oliattimeofcrep$aki48h, oliattimeofcrep$percent_delta_cr)
wilcox.test(percent_delta_cr~aki48h, oliattimeofcrep)
roc(oliattimeofcrep$aki2or3in12h, oliattimeofcrep$percent_delta_cr)
wilcox.test(percent_delta_cr~aki2or3in12h, oliattimeofcrep)
roc(oliattimeofcrep$aki2or3in24h, oliattimeofcrep$percent_delta_cr)
wilcox.test(percent_delta_cr~aki2or3in24h, oliattimeofcrep)
roc(oliattimeofcrep$aki2or3in48h, oliattimeofcrep$percent_delta_cr)
wilcox.test(percent_delta_cr~aki2or3in48h, oliattimeofcrep)
#cr eps with oli at T-4 and T0 ----
#delta cr
roc(oliaroundcrep$AKI_ICU, oliaroundcrep$delta_cr)
wilcox.test(delta_cr~AKI_ICU, oliaroundcrep)
roc(oliaroundcrep$aki2or3, oliaroundcrep$delta_cr)
wilcox.test(delta_cr~aki2or3, oliaroundcrep)
roc(oliaroundcrep$AKI_ICU, oliaroundcrep$percent_delta_cr)
wilcox.test(percent_delta_cr~AKI_ICU, oliaroundcrep)
roc(oliaroundcrep$aki2or3, oliaroundcrep$percent_delta_cr)
wilcox.test(percent_delta_cr~aki2or3, oliaroundcrep)
#all oli eps ----
#UO
roc(oli_full1$AKI_ICU, oli_full1$T0_UO)
wilcox.test(T0_UO~AKI_ICU, oli_full1)
roc(oli_full1$aki2or3, oli_full1$T0_UO)
wilcox.test(T0_UO~aki2or3, oli_full1)
roc(oli_full1$aki12h, oli_full1$T0_UO)
wilcox.test(T0_UO~aki12h, oli_full1)
roc(oli_full1$aki24h, oli_full1$T0_UO)
wilcox.test(T0_UO~aki24h, oli_full1)
roc(oli_full1$aki48h, oli_full1$T0_UO)
wilcox.test(T0_UO~aki48h, oli_full1)
roc(oli_full1$aki2or3in12h, oli_full1$T0_UO)
wilcox.test(T0_UO~aki2or3in12h, oli_full1)
roc(oli_full1$aki2or3in24h, oli_full1$T0_UO)
wilcox.test(T0_UO~aki2or3in24h, oli_full1)
roc(oli_full1$aki2or3in48h, oli_full1$T0_UO)
wilcox.test(T0_UO~aki2or3in48h, oli_full1)
#wt adjusted UO
roc(oli_full1$AKI_ICU, oli_full1$T0_UO_wtadjusted)
wilcox.test(T0_UO_wtadjusted~AKI_ICU, oli_full1)
roc(oli_full1$aki2or3, oli_full1$T0_UO_wtadjusted)
wilcox.test(T0_UO_wtadjusted~aki2or3, oli_full1)
roc(oli_full1$aki12h, oli_full1$T0_UO_wtadjusted)
wilcox.test(T0_UO_wtadjusted~aki12h, oli_full1)
roc(oli_full1$aki24h, oli_full1$T0_UO_wtadjusted)
wilcox.test(T0_UO_wtadjusted~aki24h, oli_full1)
roc(oli_full1$aki48h, oli_full1$T0_UO_wtadjusted)
wilcox.test(T0_UO_wtadjusted~aki48h, oli_full1)
roc(oli_full1$aki2or3in12h, oli_full1$T0_UO_wtadjusted)
wilcox.test(T0_UO_wtadjusted~aki2or3in12h, oli_full1)
roc(oli_full1$aki2or3in24h, oli_full1$T0_UO_wtadjusted)
wilcox.test(T0_UO_wtadjusted~aki2or3in24h, oli_full1)
roc(oli_full1$aki2or3in48h, oli_full1$T0_UO_wtadjusted)
wilcox.test(T0_UO_wtadjusted~aki2or3in48h, oli_full1)
#first oli ep ----
#UO
roc(oli_first1$AKI_ICU, oli_first1$T0_UO)
wilcox.test(T0_UO~AKI_ICU, oli_first1)
roc(oli_first1$aki2or3, oli_first1$T0_UO)
wilcox.test(T0_UO~aki2or3, oli_first1)
roc(oli_first1$aki12h, oli_first1$T0_UO)
wilcox.test(T0_UO~aki12h, oli_first1)
roc(oli_first1$aki24h, oli_first1$T0_UO)
wilcox.test(T0_UO~aki24h, oli_first1)
roc(oli_first1$aki48h, oli_first1$T0_UO)
wilcox.test(T0_UO~aki48h, oli_first1)
roc(oli_first1$aki2or3in12h, oli_first1$T0_UO)
wilcox.test(T0_UO~aki2or3in12h, oli_first1)
roc(oli_first1$aki2or3in24h, oli_first1$T0_UO)
wilcox.test(T0_UO~aki2or3in24h, oli_first1)
roc(oli_first1$aki2or3in48h, oli_first1$T0_UO)
wilcox.test(T0_UO~aki2or3in48h, oli_first1)
#wt adjusted UO
roc(oli_first1$AKI_ICU, oli_first1$T0_UO_wtadjusted)
wilcox.test(T0_UO_wtadjusted~AKI_ICU, oli_first1)
roc(oli_first1$aki2or3, oli_first1$T0_UO_wtadjusted)
wilcox.test(T0_UO_wtadjusted~aki2or3, oli_first1)
roc(oli_first1$aki12h, oli_first1$T0_UO_wtadjusted)
wilcox.test(T0_UO_wtadjusted~aki12h, oli_first1)
roc(oli_first1$aki24h, oli_first1$T0_UO_wtadjusted)
wilcox.test(T0_UO_wtadjusted~aki24h, oli_first1)
roc(oli_first1$aki48h, oli_first1$T0_UO_wtadjusted)
wilcox.test(T0_UO_wtadjusted~aki48h, oli_first1)
roc(oli_first1$aki2or3in12h, oli_first1$T0_UO_wtadjusted)
wilcox.test(T0_UO_wtadjusted~aki2or3in12h, oli_first1)
roc(oli_first1$aki2or3in24h, oli_first1$T0_UO_wtadjusted)
wilcox.test(T0_UO_wtadjusted~aki2or3in24h, oli_first1)
roc(oli_first1$aki2or3in48h, oli_first1$T0_UO_wtadjusted)
wilcox.test(T0_UO_wtadjusted~aki2or3in48h, oli_first1)
#cr change during all oli eps ----
#delta cr
roc(oli_full1$AKI_ICU, oli_full1$delta_cr)
wilcox.test(delta_cr~AKI_ICU, oli_full1)
roc(oli_full1$aki2or3, oli_full1$delta_cr)
wilcox.test(delta_cr~aki2or3, oli_full1)
roc(oli_full1$aki12h, oli_full1$delta_cr)
wilcox.test(delta_cr~aki12h, oli_full1)
roc(oli_full1$aki24h, oli_full1$delta_cr)
wilcox.test(delta_cr~aki24h, oli_full1)
roc(oli_full1$aki48h, oli_full1$delta_cr)
wilcox.test(delta_cr~aki48h, oli_full1)
roc(oli_full1$aki2or3in12h, oli_full1$delta_cr)
wilcox.test(delta_cr~aki2or3in12h, oli_full1)
roc(oli_full1$aki2or3in24h, oli_full1$delta_cr)
wilcox.test(delta_cr~aki2or3in24h, oli_full1)
roc(oli_full1$aki2or3in48h, oli_full1$delta_cr)
wilcox.test(delta_cr~aki2or3in48h, oli_full1)
#percentage delta cr
roc(oli_full1$AKI_ICU, oli_full1$percent_delta_cr)
wilcox.test(percent_delta_cr~AKI_ICU, oli_full1)
roc(oli_full1$aki2or3, oli_full1$percent_delta_cr)
wilcox.test(percent_delta_cr~aki2or3, oli_full1)
roc(oli_full1$aki12h, oli_full1$percent_delta_cr)
wilcox.test(percent_delta_cr~aki12h, oli_full1)
roc(oli_full1$aki24h, oli_full1$percent_delta_cr)
wilcox.test(percent_delta_cr~aki24h, oli_full1)
roc(oli_full1$aki48h, oli_full1$percent_delta_cr)
wilcox.test(percent_delta_cr~aki48h, oli_full1)
roc(oli_full1$aki2or3in12h, oli_full1$percent_delta_cr)
wilcox.test(percent_delta_cr~aki2or3in12h, oli_full1)
roc(oli_full1$aki2or3in24h, oli_full1$percent_delta_cr)
wilcox.test(percent_delta_cr~aki2or3in24h, oli_full1)
roc(oli_full1$aki2or3in48h, oli_full1$percent_delta_cr)
wilcox.test(percent_delta_cr~aki2or3in48h, oli_full1)
#change in UO ml/kg/h from T-4 to T0 for all oliguria eps ----
roc(oli_deltaUO$AKI_ICU, oli_deltaUO$deltaUO)
wilcox.test(deltaUO~AKI_ICU, oli_deltaUO)
roc(oli_deltaUO$aki2or3, oli_deltaUO$deltaUO)
wilcox.test(deltaUO~aki2or3, oli_deltaUO)
roc(oli_deltaUO$aki12h, oli_deltaUO$deltaUO)
wilcox.test(deltaUO~aki12h, oli_deltaUO)
roc(oli_deltaUO$aki24h, oli_deltaUO$deltaUO)
wilcox.test(deltaUO~aki24h, oli_deltaUO)
roc(oli_deltaUO$aki48h, oli_deltaUO$deltaUO)
wilcox.test(deltaUO~aki48h, oli_deltaUO)
roc(oli_deltaUO$aki2or3in12h, oli_deltaUO$deltaUO)
wilcox.test(deltaUO~aki2or3in12h, oli_deltaUO)
roc(oli_deltaUO$aki2or3in24h, oli_deltaUO$deltaUO)
wilcox.test(deltaUO~aki2or3in24h, oli_deltaUO)
roc(oli_deltaUO$aki2or3in48h, oli_deltaUO$deltaUO)
wilcox.test(deltaUO~aki2or3in48h, oli_deltaUO)
#first cr ep where time_betw_ABG>2----
cr_first2h <- cr_full %>% filter(Time_betw_ABG>=2) %>% group_by(Pt_Study_no) %>% top_n(-1, Cr_epis_no)
#delta cr
roc(cr_first2h$AKI_ICU, cr_first2h$delta_cr)
wilcox.test(delta_cr~AKI_ICU, cr_first2h)
roc(cr_first2h$aki2or3, cr_first2h$delta_cr)
wilcox.test(delta_cr~aki2or3, cr_first2h)
roc(cr_first2h$aki12h, cr_first2h$delta_cr)
wilcox.test(delta_cr~aki12h, cr_first2h)
roc(cr_first2h$aki24h, cr_first2h$delta_cr)
wilcox.test(delta_cr~aki24h, cr_first2h)
roc(cr_first2h$aki48h, cr_first2h$delta_cr)
wilcox.test(delta_cr~aki48h, cr_first2h)
roc(cr_first2h$aki2or3in12h, cr_first2h$delta_cr)
wilcox.test(delta_cr~aki2or3in12h, cr_first2h)
roc(cr_first2h$aki2or3in24h, cr_first2h$delta_cr)
wilcox.test(delta_cr~aki2or3in24h, cr_first2h)
roc(cr_first2h$aki2or3in48h, cr_first2h$delta_cr)
wilcox.test(delta_cr~aki2or3in48h, cr_first2h)
#percent delta cr
roc(cr_first2h$AKI_ICU, cr_first2h$percent_delta_cr)
wilcox.test(percent_delta_cr~AKI_ICU, cr_first2h)
roc(cr_first2h$aki2or3, cr_first2h$percent_delta_cr)
wilcox.test(percent_delta_cr~aki2or3, cr_first2h)
roc(cr_first2h$aki12h, cr_first2h$percent_delta_cr)
wilcox.test(percent_delta_cr~aki12h, cr_first2h)
roc(cr_first2h$aki24h, cr_first2h$percent_delta_cr)
wilcox.test(percent_delta_cr~aki24h, cr_first2h)
roc(cr_first2h$aki48h, cr_first2h$percent_delta_cr)
wilcox.test(percent_delta_cr~aki48h, cr_first2h)
roc(cr_first2h$aki2or3in12h, cr_first2h$percent_delta_cr)
wilcox.test(percent_delta_cr~aki2or3in12h, cr_first2h)
roc(cr_first2h$aki2or3in24h, cr_first2h$percent_delta_cr)
wilcox.test(percent_delta_cr~aki2or3in24h, cr_first2h)
roc(cr_first2h$aki2or3in48h, cr_first2h$percent_delta_cr)
wilcox.test(percent_delta_cr~aki2or3in48h, cr_first2h)
#optimal cutpoints on aucs ----
roc(cr_first$aki2or3in24h, cr_first$delta_cr)
cr_first <- as.data.frame(cr_first)
optimal.cutpoint.Youden <- optimal.cutpoints(X = "delta_cr", status = "aki2or3in24h", tag.healthy = 0,
                                             methods = "Youden", data = cr_first, conf.level = 0.95,
                                             ci.fit=TRUE)
summary(optimal.cutpoint.Youden)
roc(cr_last$aki48h, cr_last$delta_cr)
cr_last <- as.data.frame(cr_last)
optimal.cutpoint.Youden <- optimal.cutpoints(X = "delta_cr", status = "aki48h", tag.healthy = 0,
                                             methods = "Youden", data = cr_last, conf.level = 0.95)
summary(optimal.cutpoint.Youden)
roc(oli_full1$aki2or3in12h, oli_full1$T0_UO_wtadjusted)
oli_full1 <- as.data.frame(oli_full1)
optimal.cutpoint.Youden <- optimal.cutpoints(X = "T0_UO_wtadjusted", status = "aki2or3in12h", tag.healthy = 1,
                                             methods = "Youden", data = oli_full1, conf.level = 0.95)
summary(optimal.cutpoint.Youden)
roc(oli_first1$aki2or3in12h, oli_first1$T0_UO_wtadjusted)
oli_first1 <- as.data.frame(oli_first1)
optimal.cutpoint.Youden <- optimal.cutpoints(X = "T0_UO_wtadjusted", status = "aki2or3in12h", tag.healthy = 1,
                                             methods = "Youden", data = oli_first1, conf.level = 0.95)
summary(optimal.cutpoint.Youden)
roc(oli_first1$aki2or3in24h, oli_first1$T0_UO_wtadjusted)
optimal.cutpoint.Youden <- optimal.cutpoints(X = "T0_UO_wtadjusted", status = "aki2or3in24h", tag.healthy = 1,
                                             methods = "Youden", data = oli_first1, conf.level = 0.95)
summary(optimal.cutpoint.Youden)
roc(oli_first1$aki2or3in48h, oli_first1$T0_UO_wtadjusted)
optimal.cutpoint.Youden <- optimal.cutpoints(X = "T0_UO_wtadjusted", status = "aki2or3in48h", tag.healthy = 1,
                                             methods = "Youden", data = oli_first1, conf.level = 0.95)
summary(optimal.cutpoint.Youden)
roc(oli_full1$aki12h, oli_full1$delta_cr)
optimal.cutpoint.Youden <- optimal.cutpoints(X = "delta_cr", status = "aki12h", tag.healthy = 0,
                                             methods = "Youden", data = oli_full1, conf.level = 0.95)
summary(optimal.cutpoint.Youden)
#--- NEW ANALYSES ----
#creating new dataframe for Laurent n=313 all pts first ep----
#get all cr pts n=279
data<-data.frame()
colnames(cr_first)
data <- cr_first[,c(1,29:55,57,108:111,69,124,70,74,115,91,116,117,93,100,121,119,24,107,126:128)]
colnames(data)[colnames(data)=="DateTime_Cr_epis"] <- "DateTime_epis"
data %>% View()
#get oli only pts n=34
colnames(olionly_first)
colnames(cr_first) == colnames(olionly_first)
data2 <- data.frame()
data2 <- olionly_first[,c(1,29:55,57,108:111,69,125,70,74,115,91,116,117,93,100,121,119,18,107,127:129)]
colnames(data2)[colnames(data2)=="DateTime_olig_epis"] <- "DateTime_epis"
data2 %>% View()
#bind two dataframes n=313
ncol(data) == ncol(data2)
colnames(data) == colnames(data2)
data3 <- bind_rows(data, data2)
data3 %>% View()
nrow(data3)
ncol(data3)
#add final column with category of episode - cr or oli
data3$data_cr_ep <- c(rep(1, 279), rep(0,34))
data3 %>% View()
colnames(data3)
#override data for n=131
creoliboth1$DateTime_olig_epis <- NA
creoliboth1$DateTime_olig_epis <- creoliboth2$DateTime_olig_epis
creoliboth1$DateTime_olig_epis == creoliboth2$DateTime_olig_epis
creoliboth1 <- creoliboth1 %>% mutate(
  Time_betw_cr_oli = as.duration(DateTime_Cr_epis %--% DateTime_olig_epis) / dhours(1)
)
creoliboth1 %>%  filter(Time_betw_cr_oli>0) %>% nrow() #take data from cr_first n=113
creoliboth1 %>%  filter(Time_betw_cr_oli<0) %>% nrow() #take data from oli_first n=18
crpts <- creoliboth1 %>%  filter(Time_betw_cr_oli<0) %>% select(Pt_Study_no)
colnames(creoliboth1)

creoliboth2$DateTime_Cr_epis <- NA
creoliboth2$DateTime_Cr_epis <- creoliboth1$DateTime_Cr_epis
creoliboth2$DateTime_Cr_epis == creoliboth1$DateTime_Cr_epis
creoliboth2 <- creoliboth2 %>% mutate(
  Time_betw_cr_oli = as.duration(DateTime_Cr_epis %--% DateTime_olig_epis) / dhours(1)
)
creoliboth2 %>%  filter(Time_betw_cr_oli>0) %>% nrow() #take data from cr_first n=113
creoliboth2 %>%  filter(Time_betw_cr_oli<0) %>% nrow() #take data from oli_first n=18
olipts <- creoliboth2 %>%  filter(Time_betw_cr_oli<0) %>% select(Pt_Study_no)

t1 <- mer$scre %>% filter(Pt_Study_no_cre %in% crpts$Pt_Study_no) %>%
  select(Pt_Study_no_cre, Pt_Study_no_oli) %>% arrange(Pt_Study_no_cre)
t2 <- mer$scre %>% filter(Pt_Study_no_oli %in% olipts$Pt_Study_no) %>%
  select(Pt_Study_no_cre, Pt_Study_no_oli) %>% arrange(Pt_Study_no_cre)
View(t1)
View(t2)
t1$Pt_Study_no_cre == t2$Pt_Study_no_cre
t1$Pt_Study_no_oli == t2$Pt_Study_no_oli

data3 <- data3 %>% filter(! Pt_Study_no %in% t1$Pt_Study_no_cre)
nrow(data3)
colnames(oli_first)
t3 <- data.frame()
t3 <- oli_first[,c(1,29:55,57,108:111,69,125,70,74,115,91,116,117,93,100,121,119,18,107,127:129)]
colnames(t3)[colnames(t3)=="DateTime_olig_epis"] <- "DateTime_epis"
t3 <- t3 %>% filter(Pt_Study_no %in% t1$Pt_Study_no_oli)
t3$data_cr_ep <- 0
t3 %>% View()
nrow(t3)
ncol(t3)
colnames(data3) == colnames(t3)
data3 <- bind_rows(data3, t3)
nrow(data3)
ncol(data3)
View(data3)
length(unique(data3$Pt_Study_no))
sum(data3$data_cr_ep)
summary(data3$Time_betw_ABG)
#save dataframe
save(data3,file="EpocData313ptsfirstepinICU.RData")
#creating new dataset of first ep with specific conditions n=277 ----
#clean the full cr database with specific conditions
cr_first_clean <- cr_full %>% filter(Time_betw_ICU_cr>=2)
nrow(cr_first_clean) #521 rows
cr_first_clean <- cr_first_clean %>% filter(Time_betw_ABG>=2) %>% filter(Time_betw_ABG<=6)
nrow(cr_first_clean) #450 rows
cr_first_clean <- cr_first_clean %>% filter(Time_betw_ICU_cr<=48)
nrow(cr_first_clean) #390 rows
cr_first_clean <- cr_first_clean %>%
  group_by(Pt_Study_no) %>%
  top_n(-1, Cr_epis_no) %>% ungroup()
nrow(cr_first_clean) #227 rows = 227 patients
#clean full oli database with specific conditions
oli_first_clean <- oli_full %>% filter(Time_betw_ICU_oli<=48)
nrow(oli_first_clean) #202
oli_first_clean <- oli_first_clean %>%
  group_by(Pt_Study_no) %>%
  top_n(-1, Olig_epis_no) %>% ungroup()
nrow(oli_first_clean) #154
#oliguria only clean
mer$olionly <- filter(mer$scre, Incl_criteria_ok_cre == "N", Epis_olig == "Y")
nrow(mer$olionly)
olionlyclean <- filter(oli_first_clean, Pt_Study_no %in% mer$olionly$Pt_Study_no_oli)
nrow(olionlyclean) #32 = 32 patients
#cr pts excluded on specific conditions but also had oli ep
group4 <-  mer$both %>% filter(! Pt_Study_no_cre %in% cr_first_clean$Pt_Study_no)
nrow(group4) # 21 patients
group4 <- oli_first_clean %>% filter(Pt_Study_no %in% group4$Pt_Study_no_oli)
nrow(group4) #18 pts
# cr pts excluded on specific conditions who also had oli ep
# but 3 pts also excluded from oli with specific conditions
# 18 pts need to take data from oli_first_clean (group 4)

# group 3
# pts had first oli ep meeting conditions before first cr ep meeting conditions
group1n3 <- left_join(cr_first_clean,
                    mer$both %>% select(Pt_Study_no_oli, Pt_Study_no_cre) %>%
                      rename(Pt_Study_no = Pt_Study_no_cre),
                    by = "Pt_Study_no")
select(group1n3, Pt_Study_no, Pt_Study_no_oli)
group1n3 <- left_join(group1n3,
                    oli_first_clean %>% select(Pt_Study_no, DateTime_olig_epis) %>%
                      rename(Pt_Study_no_oli = Pt_Study_no),
                    by = "Pt_Study_no_oli")
select(group1n3, Pt_Study_no, Pt_Study_no_oli, DateTime_Cr_epis, DateTime_olig_epis)
group1n3 <- group1n3 %>% mutate(
  Time_betw_cr_oli = as.duration(DateTime_Cr_epis %--% DateTime_olig_epis) / dhours(1)
)
select(group1n3, Pt_Study_no, Pt_Study_no_oli,
       DateTime_Cr_epis, DateTime_olig_epis, Time_betw_cr_oli)
# if positive or NA, cr happened first
nrow(group1n3 %>% filter(is.na(Time_betw_cr_oli) | Time_betw_cr_oli > 0)) #213 Cr first
nrow(group1n3 %>% filter(Time_betw_cr_oli < 0)) # 14 oli first

group1 <- group1n3 %>% filter(is.na(Time_betw_cr_oli) | Time_betw_cr_oli > 0)
group3 <- group1n3 %>% filter(Time_betw_cr_oli < 0)
group3 <- oli_first_clean %>% filter(Pt_Study_no %in% group3$Pt_Study_no_oli)

group2 <- olionlyclean

group1$data_cr_ep = 1
group2$data_cr_ep = 0
group3$data_cr_ep = 0
group4$data_cr_ep = 0

nrow(group1) #213
colnames(group1)[1:5]
colnames(group1)[colnames(group1)=="DateTime_post_ABG"] <- "DateTime_epis"
nrow(group2) #32
colnames(group2)[1:5]
colnames(group2)[colnames(group2)=="DateTime_olig_epis"] <- "DateTime_epis"
nrow(group3) #14
colnames(group3)[1:5]
colnames(group3)[colnames(group3)=="DateTime_olig_epis"] <- "DateTime_epis"
nrow(group4) #18
colnames(group4)[1:5]
colnames(group4)[colnames(group4)=="DateTime_olig_epis"] <- "DateTime_epis"

nrow(group1) + nrow(group2) + nrow(group3) + nrow(group4) #277 pts

# Double check we have the right number of patients
mer$scre %>% filter(!is.na(Pt_Study_no_oli) | !is.na(Pt_Study_no_cre)) %>% nrow()
mer$scre %>% filter(!is.na(Pt_Study_no_oli) | !is.na(Pt_Study_no_cre)) %>%
  filter(!Pt_Study_no_oli %in% oli_first_clean$Pt_Study_no,
         !Pt_Study_no_cre %in% cr_first_clean$Pt_Study_no) %>%
  nrow()  # 36 have been excluded by special conditions
313 - 36 #277 pts

## Select columns/variables required
colnameswanted <- c(colnames(data3), "T0_Norad", "T0_Metaraminol")
group1 <- group1[,colnameswanted]
group2 <- group2[,colnameswanted]
group3 <- group3[,colnameswanted]
group4 <- group4[,colnameswanted]
dim(group1)
dim(group2)
dim(group3)
dim(group4)
#rbind 4 groups
data4 <- bind_rows(group1, group2, group3, group4)
data4 %>% View()
dim(data4)
length(unique(data4$Pt_Study_no))
sum(data4$data_cr_ep)
summary(data4$Time_betw_ABG)
#add binary variables for vasopressor
data4 <- data4 %>% mutate(vaso_binary = ifelse(T0_Norad ==0 & T0_Metaraminol==0, 0, 1))
#identify pts with aki2or3 in 12h
data4 %>% filter(aki12h == 1) %>%
  filter(akistagesv2 == 2 | akistagesv2==3) %>% select(Pt_Study_no) %>% View()
#save dataframe
save(data4,file="EpocData277ptsfirstepinICU.RData")
#full cohort demographics n=387 ----
#make full dataframe n=387
nrow(cr_first) #279
nrow(scr_out$nocr) #108
279 + 108 == 387
colnames(cr_first)
colnames(scr_out$nocr)
colnames(scr_out$nocr)[colnames(scr_out$nocr)=="dc_ICU_alive"] <- "Dc_ICU_Alive"
colnames(scr_out$nocr)[colnames(scr_out$nocr)=="dc_hosp_alive"] <- "Dc_Hosp_Alive"
cr_first <- as.data.frame(cr_first)
scr_out$nocr <-  as.data.frame(scr_out$nocr)
fulldemo <- rbind( cr_first[ ,c("Age", "Male", "Wt", "Wtmeasured", "Surgadmission",
                                 "Mecvenadm", "APACHE_II", "APACHE_III", "ICU_LOS", "Hosp_LOS",
                                 "Dc_ICU_Alive", "Dc_Hosp_Alive", "PCm_cardio", "PCm_resp",
                                "PCm_GI","PCm_neuro","PCm_sepsis","PCm_trauma","PCm_metabolic","PCm_Haem",
                                "PCm_renal","PCm_other","PCm_msk",
                                "PCs_cardio","PCs_resp","PCs_GI",
                                "PCs_neuro","PCs_trauma","PCs_renal",
                                "PCs_gynae","PCs_msk","PCs_haem",
                                "PCs_metabolic")],
                       scr_out$nocr[ ,c("Age", "Male", "Wt", "Wtmeasured", "Surgadmission",
                                     "Mecvenadm", "APACHE_II", "APACHE_III", "ICU_LOS", "Hosp_LOS",
                                     "Dc_ICU_Alive", "Dc_Hosp_Alive","PCm_cardio", "PCm_resp",
                                     "PCm_GI","PCm_neuro","PCm_sepsis","PCm_trauma","PCm_metabolic","PCm_Haem",
                                     "PCm_renal","PCm_other","PCm_msk",
                                     "PCs_cardio","PCs_resp","PCs_GI",
                                     "PCs_neuro","PCs_trauma","PCs_renal",
                                     "PCs_gynae","PCs_msk","PCs_haem",
                                     "PCs_metabolic")])
dim(fulldemo)
colnames(fulldemo)
fulldemo$cr_ep <- c(rep(1, 279), rep(0,108))
#age
summary(fulldemo$Age)
#male
sum(fulldemo$Male)
sum(fulldemo$Male == 0)
sum(fulldemo$Male)/nrow(fulldemo) *100
#wt
summary(fulldemo$Wt)
#wtmeasured
sum(is.na(fulldemo$Wtmeasured))
sum(na.omit(fulldemo$Wtmeasured))
sum(na.omit(fulldemo$Wtmeasured)==0)
sum(na.omit(fulldemo$Wtmeasured))/nrow(fulldemo)*100
#surg
sum(fulldemo$Surgadmission)
sum(fulldemo$Surgadmission ==0)
sum(fulldemo$Surgadmission)/nrow(fulldemo) *100
#mv
sum(fulldemo$Mecvenadm)
sum(fulldemo$Mecvenadm==0)
sum(fulldemo$Mecvenadm)/nrow(fulldemo) *100
#APACHE II
summary(fulldemo$APACHE_II)
#APACHE III
summary(fulldemo$APACHE_III)
#PC
sum(fulldemo$PCm_cardio)
sum(fulldemo$PCm_cardio)/nrow(fulldemo) *100
sum(fulldemo$PCm_resp)
sum(fulldemo$PCm_resp)/nrow(fulldemo) *100
sum(fulldemo$PCm_GI)
sum(fulldemo$PCm_GI)/nrow(fulldemo) *100
sum(fulldemo$PCm_neuro)
sum(fulldemo$PCm_neuro)/nrow(fulldemo) *100
sum(fulldemo$PCm_sepsis)
sum(fulldemo$PCm_sepsis)/nrow(fulldemo) *100
sum(fulldemo$PCm_trauma)
sum(fulldemo$PCm_trauma)/nrow(fulldemo) *100
sum(fulldemo$PCm_metabolic)
sum(fulldemo$PCm_metabolic)/nrow(fulldemo) *100
sum(fulldemo$PCm_renal)
sum(fulldemo$PCm_renal)/nrow(fulldemo) *100
sum(fulldemo$PCs_cardio)
sum(fulldemo$PCs_cardio)/nrow(fulldemo) *100
sum(fulldemo$PCs_resp)
sum(fulldemo$PCs_resp)/nrow(fulldemo) *100
sum(fulldemo$PCs_GI)
sum(fulldemo$PCs_GI)/nrow(fulldemo) *100
sum(fulldemo$PCs_neuro)
sum(fulldemo$PCs_neuro)/nrow(fulldemo) *100
sum(fulldemo$PCs_renal)
sum(fulldemo$PCs_renal)/nrow(fulldemo) *100
sum(fulldemo$PCs_gynae)
sum(fulldemo$PCs_gynae)/nrow(fulldemo) *100
sum(fulldemo$PCs_msk)
sum(fulldemo$PCs_msk)/nrow(fulldemo) *100
sum(fulldemo$PCs_metabolic)
sum(fulldemo$PCs_metabolic)/nrow(fulldemo) *100
#ICU LOS
summary(fulldemo$ICU_LOS)
#hosp LOS
summary(fulldemo$Hosp_LOS)
#ICU mortality
sum(is.na(fulldemo$Dc_ICU_Alive))
sum(fulldemo$Dc_ICU_Alive)
sum(fulldemo$Dc_ICU_Alive==0)
sum(fulldemo$Dc_ICU_Alive==0)/nrow(fulldemo) *100
#hosp mortality
sum(is.na(fulldemo$Dc_Hosp_Alive))
sum(na.omit(fulldemo$Dc_Hosp_Alive))
sum(na.omit(fulldemo$Dc_Hosp_Alive)==0)
sum(na.omit(fulldemo$Dc_Hosp_Alive)==0)/nrow(fulldemo) *100
#ICU adm to AKI Dx
#merge cr_first and olionly_first
icutoaki <- rbind(cr_first[, c("Age", "ICUadmtoAKIDx")], olionly_first[,c("Age", "ICUadmtoAKIDx")])
shapiro.test(icutoaki$ICUadmtoAKIDx)
summary(icutoaki$ICUadmtoAKIDx)
#cr only demographics n=148----
nrow(cronly_first)
colnames(cronly_first)
#age
summary(cronly_first$Age)
#male
sum(cronly_first$Male)
sum(cronly_first$Male == 0)
sum(cronly_first$Male)/nrow(cronly_first) *100
#wt
summary(cronly_first$Wt)
#wtmeasured
sum(is.na(cronly_first$Wtmeasured))
sum(na.omit(cronly_first$Wtmeasured))
sum(na.omit(cronly_first$Wtmeasured)==0)
sum(na.omit(cronly_first$Wtmeasured))/nrow(cronly_first)*100
#surg
sum(cronly_first$Surgadmission)
sum(cronly_first$Surgadmission ==0)
sum(cronly_first$Surgadmission)/nrow(cronly_first) *100
#mv
sum(cronly_first$Mecvenadm)
sum(cronly_first$Mecvenadm==0)
sum(cronly_first$Mecvenadm)/nrow(cronly_first) *100
#APACHE II
summary(cronly_first$APACHE_II)
#APACHE III
summary(cronly_first$APACHE_III)
#ICU LOS
summary(cronly_first$ICU_LOS)
#hosp LOS
summary(cronly_first$Hosp_LOS)
#ICU mortality
sum(is.na(cronly_first$Dc_ICU_Alive))
sum(cronly_first$Dc_ICU_Alive)
sum(cronly_first$Dc_ICU_Alive==0)
sum(cronly_first$Dc_ICU_Alive==0)/nrow(cronly_first) *100
#hosp mortality
sum(is.na(cronly_first$Dc_Hosp_Alive))
sum(na.omit(cronly_first$Dc_Hosp_Alive))
sum(na.omit(cronly_first$Dc_Hosp_Alive)==0)
sum(na.omit(cronly_first$Dc_Hosp_Alive)==0)/nrow(cronly_first) *100
#import APACHEs from APD extract ----
# Load APD extract
print(list.files(pattern = ".xlsx"))
apdextract <- read_excel("APD231018.xlsx", "Admissions")
# colnames(apdextract)
apdextract <- apdextract[
  c("HRN/NIH", "ICU_ADM_DTM",
    "AP2score", "AP3score")]
apdextract <- apdextract%>%
  mutate(AP2score = as.numeric(AP2score),
         AP3score = as.numeric(AP3score))
# type = "cre"; df = cr_first; apddf = apdextract; scre = mer$scre
# rm(type, df, apddf, scre, outdf, oldnrow, oldPt)
mergeapscore <- function(type, df, apddf, scre) {
  # Add Pt_Study_no into the scre dataframe
  if (type == "cre") {scre <- mutate(scre, Pt_Study_no = Pt_Study_no_cre)
  } else {            scre <- mutate(scre, Pt_Study_no = Pt_Study_no_oli)}
  scre <- select(scre, Pt_Study_no, `UR number`)
  # Merge the UR number into the dataframe
  oldnrow = nrow(df)
  df <- left_join(df, scre, c("Pt_Study_no"))
  if (sum(is.na(df$`UR number`))) warning("Missing UR numbers")
  if (nrow(df) != oldnrow) warning("More rows introduced")
  # Merge all the columns from apddf into df
  oldPt <- df$Pt_Study_no
  apddf <- rename(apddf, `UR number` = `HRN/NIH`)
  df <- left_join(df, apddf, c("UR number")) %>% ungroup()
  # Check difference between DateTime_ICU_admit and ICU_ADM_DTM
  df <- df %>%
    mutate(ICU_admit_diff =
             as.duration(DateTime_ICU_admit %--% force_tz(ICU_ADM_DTM, "Australia/Melbourne")) / dhours(1)) %>%
    mutate(ICU_admit_diff = ifelse(is.na(ICU_admit_diff), 0, ICU_admit_diff))
  # check <- df %>% filter(Pt_Study_no == "LT22") %>% select(`UR number`, "DateTime_ICU_admit", "ICU_ADM_DTM", "ICU_admit_diff")
  # View(df %>% select(`UR number`, "DateTime_ICU_admit", "ICU_ADM_DTM", "ICU_admit_diff", "AP2score", "AP3score"))
  # Filter out incorrect date times
  outdf <- df %>%
    group_by(Pt_Study_no) %>%
    top_n(-1, abs(ICU_admit_diff)) %>%
    mutate(remove = (abs(ICU_admit_diff) > 24)) %>%
    mutate(ICU_ADM_DTM = ifelse(remove, NA, ICU_ADM_DTM),
           AP2score    = ifelse(remove, NA, AP2score),
           AP3score    = ifelse(remove, NA, AP3score)) %>%
    select(-`UR number`, -remove) %>%
    ungroup()
  # errdf <- df %>%
  #   filter(abs(ICU_admit_diff) > 24) %>%
  #   select("Pt_Study_no", `UR number`, "DateTime_ICU_admit", "ICU_ADM_DTM", "ICU_admit_diff", "AP2score", "AP3score")
  # # View(errdf)
  # nrow(outdf)

  if(nrow(outdf) != oldnrow) warning("Rows missing or rows added")
  if(sum(oldPt != outdf$Pt_Study_no) != 0) warning("Patients missing or added...")
  return(outdf)
}

#cr_first
cr_first <- mergeapscore("cre", cr_first, apdextract, mer$scre) %>%
  mutate(APACHE_II  = AP2score,
         APACHE_III = AP3score) %>%
  select(-AP2score, -AP3score)
dim(cr_first)
sum(is.na(cr_first$APACHE_III))

#scr_out$nocr
df = scr_out$nocr %>% mutate(unique = row_number())
df <- left_join(df, rename(apdextract, UR=`HRN/NIH`), c("UR")) %>% ungroup()
# Check difference between DateTime_ICU_admit and ICU_ADM_DTM
df <- df %>%
  mutate(ICU_admit_diff =
           as.duration(DateTime_ICU_admit %--% force_tz(ICU_ADM_DTM, "Australia/Melbourne")) / dhours(1)) %>%
  mutate(ICU_admit_diff = ifelse(is.na(ICU_admit_diff), 0, ICU_admit_diff))
df <- df %>%
  group_by(unique) %>%
  top_n(-1, abs(ICU_admit_diff)) %>%
  mutate(remove = (abs(ICU_admit_diff) > 24)) %>%
  mutate(ICU_ADM_DTM = ifelse(remove, NA, ICU_ADM_DTM),
         AP2score    = ifelse(remove, NA, AP2score),
         AP3score    = ifelse(remove, NA, AP3score)) %>%
  ungroup() %>%
  select(-unique, -remove) %>%
  mutate(APACHE_II  = AP2score,
         APACHE_III = AP3score) %>%
  select(-AP2score, -AP3score)

scr_out$nocr <-  df
rm(df)
#Table 2 cr change characteristics ----
#cr increase (umol/L)
cr_first$crdiff = cr_first$T0_ABG_Cr - cr_first$`T-4_ABG_Cr`
shapiro.test(cr_first$crdiff)
summary(cr_first$crdiff)
#delta cr
shapiro.test(cr_first$delta_cr)
summary(cr_first$delta_cr)
#percent increase
cr_first$percentcrdiff = ((cr_first$T0_ABG_Cr - cr_first$`T-4_ABG_Cr`)/cr_first$`T-4_ABG_Cr`) *100
shapiro.test(cr_first$percentcrdiff)
summary(cr_first$percentcrdiff)
#percent delta cr
shapiro.test(cr_first$percent_delta_cr)
summary(cr_first$percent_delta_cr)
#time betw ABG
shapiro.test(cr_first$Time_betw_ABG)
summary(cr_first$Time_betw_ABG)
#UO
shapiro.test(cr_first$T0_UO)
summary(cr_first$T0_UO)
#UO wt adjusted
cr_first$T0_UO_wtadjusted = (cr_first$T0_UO/4)/cr_first$Wt
shapiro.test(cr_first$T0_UO_wtadjusted)
summary(cr_first$T0_UO_wtadjusted)
#fluid bal
shapiro.test(cr_first$T0_fluid_bal)
summary(cr_first$T0_fluid_bal)
#number with negative fluid bal
cr_first %>% filter(T0_fluid_bal<0) %>% nrow()
#number with vasopressor
cr_first$vaso = ifelse(cr_first$T0_Norad > 0 | cr_first$T0_Metaraminol > 0, 1, 0)
sum(na.omit(cr_first$vaso))

#GLM cr episode for AKI ----
#make dataframe fromm cr_first, olionly_first, scr_out$neit
scr_out$neit$AKI_ICU <- 0
scr_out$neit$crep <- 0
cr_first$crep <- 1
olionly_first$crep <- 0
nrow(cr_first) + nrow(olionly_first) + nrow(scr_out$neit)
crunivar <- rbind(cr_first[ ,c("Age", "AKI_ICU", "crep")],
      olionly_first[ ,c("Age", "AKI_ICU", "crep")],
      scr_out$neit[ ,c("Age", "AKI_ICU", "crep")])
glm(AKI_ICU~crep, data = crunivar, family = "binomial")
summary(glm(AKI_ICU~crep, data = crunivar, family = "binomial"))
round(exp(summary(glm(AKI_ICU~crep, data = crunivar, family = "binomial"))$coefficients[-1,1]),2)
round(exp(confint(glm(AKI_ICU~crep, data = crunivar, family = "binomial")))[-1,],3)

#try to make a loop
outcomecols <- c("AKI_ICU", "aki2or3", "aki12h", "aki24h", "aki48h", "aki2or3in12h", "aki2or3in24h",
                 "aki2or3in48h")
scr_out$neit$aki2or3 <- 0
scr_out$neit$aki12h <- 0
scr_out$neit$aki24h <- 0
scr_out$neit$aki48h <- 0
scr_out$neit$aki2or3in12h <- 0
scr_out$neit$aki2or3in24h <- 0
scr_out$neit$aki2or3in48h <- 0
crunivar <- rbind(cr_first[ ,c("Age", "crep", outcomecols)],
                  olionly_first[ ,c("Age", "crep", outcomecols)],
                  scr_out$neit[ ,c("Age", "crep", outcomecols)])
table <- data.frame()
for (o in outcomecols){
  mod<-glm(paste0(o, "~crep"),data=crunivar,family="binomial")
  coef<-round(exp(summary(mod)$coefficients[-1,1]),2)
  ci<-round(exp(confint(mod))[-1,],3)
  col1=paste0(coef," [",ci[1],"; ",ci[2],"]")
  r <-data.frame(
    o = o, p= "crep",
    outcome = o,
    n.events=sum(crunivar[o]),
    or = coef, ci.low=ci[1], ci.up=ci[2], col1)
  # Combine into the table dataframe
  table=rbind(table,r)
}
rownames(table) <- NULL
table
table$o <- c("AKI in ICU", "AKI stages 2 & 3 in ICU", "AKI in 12h", "AKI in 24h",
             "AKI in 48h", "AKI stages 2 & 3 in 12h", "AKI stages 2 & 3 in 24h",
             "AKI stages 2 & 3 in 48h")
table$o <- factor(table$o, levels=table[order(table$o,decreasing=T),]$o)

#run code to make table2 from section called "cr ep univariate for mortality & LOS"
#combine table and table2
table <- rbind(table, table2)
table
#get rid of ICU mortality b/c CI takes it to 100+
#even log transforming the scale doesn't make the graph presentable
table <- table[-c(9),]
table
table$o <- c("AKI in ICU", "AKI stages 2 & 3 in ICU", "AKI in 12h", "AKI in 24h",
             "AKI in 48h", "AKI stages 2 & 3 in 12h", "AKI stages 2 & 3 in 24h",
             "AKI stages 2 & 3 in 48h", "Hospital mortality")
table
#plot the output for figure with aki outcomes only
#my version 1
fig2plot <- ggplot(table, aes(x = o, y = or)) +
  theme_bw() +
  geom_hline(yintercept=1, linetype=2, color="grey40") +
  geom_errorbar(aes(ymin=ci.low, ymax=ci.up), width=0.2, size=0.6) +
  geom_point(shape=15, size=4) +
  coord_flip() +
  labs(y="Odds ratio [95% confidence interval]") +
  theme(axis.title.y=element_blank(),axis.ticks.y=element_blank(),
        axis.line.y=element_blank()) +
  scale_y_continuous(breaks = 1:10) +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank()) +
  geom_text(data = table,
            aes(y=11, x=o, label= paste0(round(or,1)," [",round(ci.low,1),"-",round(ci.up,1),"]" )),
            size=3.2)
#alwin's final version - landscape
fig2plot <- ggplot(table, aes(x = o, y = or)) +
  theme_bw() +
  geom_hline(yintercept=1, linetype=2, color="grey40") +
  geom_errorbar(aes(ymin=ci.low, ymax=ci.up), width=0.2, size=0.6) +
  geom_point(shape=15, size=4) +
  coord_flip() +
  labs(y="Odds ratio [95% confidence interval]") +
  theme(axis.title.y=element_blank(),axis.ticks.y=element_blank(),
        axis.line.y=element_blank()) +
  scale_y_continuous(breaks = 1:10) +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        text = element_text(size=14),
        axis.title.x = element_text(size=12),
        axis.text = element_text(colour = "black")) +
  geom_text(data = table,
            aes(y=10.6, x=o, label= sprintf("%3.1f [%3.1f-%3.1f]", or, ci.low, ci.up)),
            size=3.5)
# %3.1f means use at least 3 characters and round to 1 decimal places:
#   sprintf("%3.1f", pi) --> "3.1"
# %8.4f means use at least 8 characters and round to 4 decimal places:
#   sprintf("%8.4f", pi) --> "  3.1416"
fig2plot
#alwin's version smaller size + Times New Roman
windowsFonts(TimesNewRoman = windowsFont("Times New Roman"))

fig2plot <- ggplot(table, aes(x = o, y = or)) +
  theme_bw() +
  geom_hline(yintercept=1, linetype=2, color="grey40") +
  geom_errorbar(aes(ymin=ci.low, ymax=ci.up), width=0.2, size=0.6) +
  geom_point(shape=15, size=4) +
  coord_flip(ylim = c(1, 12)) +
  labs(y="Odds ratio [95% confidence interval]") +
  theme(axis.title.y=element_blank(),axis.ticks.y=element_blank(),
        axis.line.y=element_blank()) +
  scale_y_continuous(breaks = 1:10) +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        text = element_text(size=14, family = "TimesNewRoman"),
        axis.title.x = element_text(size=12, family = "TimesNewRoman"),
        axis.text = element_text(colour = "black")) +
  geom_text(data = table,
            aes(y=11.3, x=o, label= sprintf("%3.1f [%3.1f-%3.1f]", or, ci.low, ci.up)),
            size=3.7,
            family = "TimesNewRoman")
# %3.1f means use at least 3 characters and round to 1 decimal places:
#   sprintf("%3.1f", pi) --> "3.1"
# %8.4f means use at least 8 characters and round to 4 decimal places:
#   sprintf("%8.4f", pi) --> "  3.1416"
fig2plot

ggsave("fig2plot.png", plot = fig2plot, width = 15, height = 9, units = "cm",
       scale = 1.2, dpi = 300)
save(table,file="LisaTohfigure2data.RData") #this makes it flatter looking
#plot the output for figure with hosp mortality added
fig2plot <- ggplot(table, aes(x = o, y = or)) +
  theme_bw() +
  geom_hline(yintercept=1, linetype=2, color="grey40") +
  geom_errorbar(aes(ymin=ci.low, ymax=ci.up), width=0.2, size=0.6) +
  geom_point(shape=15, size=4) +
  coord_flip() +
  labs(y="Odds ratio [95% confidence interval]") +
  theme(axis.title.y=element_blank(),axis.ticks.y=element_blank(),
        axis.line.y=element_blank()) +
  scale_y_continuous(breaks = 1:11) +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank()) +
  geom_text(data = table,
            aes(y=11.5, x=o, label= paste0(round(or,1)," [",round(ci.low,1),"-",round(ci.up,1),"]" )),
            size=3.2)
fig2plot
#GLM Unipredictor cr_first ----
#change mortality columns
cr_first <- cr_first %>% mutate(ICU_mortality = ifelse(Dc_ICU_Alive == 1, 0, 1),
                                Hosp_mortality = ifelse(Dc_Hosp_Alive == 1 , 0, 1))
temp <- cr_first
outcomecols <- c("AKI_ICU", "aki2or3", "aki12h", "aki24h", "aki48h", "aki2or3in12h", "aki2or3in24h",
                 "aki2or3in48h", "ICU_mortality", "Hosp_mortality")
predictorcols <- c("crchange", "delta_cr", "percent_delta_cr")
table <- data.frame()
# Loop across each of the outcomes
for (o in c(1:length(outcomecols))) {
  # Loop across each of the predictors
  for (p in c(1:length(predictorcols))) {
    # Create a dataframe of outcome and predictor
    df<-temp[,c(predictorcols[p], outcomecols[o])]
    colnames(df)[1]<-"pred"
    df$pred=df$pred/10
    colnames(df)[2]<-"outcome"
    df$outcome <- factor(df$outcome)
    # Perform glm
    mod<-glm(outcome~pred,data=df,family="binomial")
    # Summarise the output
    coef<-round(exp(summary(mod)$coefficients[-1,1]),2)
    ci<-round(exp(confint(mod))[-1,],3)
    col1=paste0(coef," [",ci[1],"; ",ci[2],"]")
    # Return the output
    r <-data.frame(
      o = o, p= p,
      outcome = outcomecols[o], predictor = predictorcols[p],
      n.events=length(which(df$outcome %in% levels(df$outcome)[-1])),
      or = coef, ci.low=ci[1], ci.up=ci[2], col1)
    # Combine into the table dataframe
    table=rbind(table,r)
  }
}
# Clean up the output and save as csv
rownames(table) <- NULL
write.csv(table, "cr_first_univariate.csv")
# Plot the output
ggplot(table, aes(x = outcome, y = or)) +
  facet_wrap(~predictor, scales = "free_x") +
  theme_bw() +
  geom_hline(yintercept=1, linetype=2, color="grey40") +
  geom_errorbar(aes(ymin=ci.low, ymax=ci.up), width=0.2, size=0.6) +
  geom_point(shape=15, size=4) +
  coord_flip() +
  labs(y="Odds ratio (95% confidence interval)") +
  theme(axis.title.y=element_blank(),axis.ticks.y=element_blank(),
        axis.line.y=element_blank()) +
  ggtitle("cr_first")

# GLM unipredictor Cr more than 2 hrs v1 ----
cr_first2h <- cr_full %>% filter(Time_betw_ABG>=2) %>% group_by(Pt_Study_no) %>% top_n(-1, Cr_epis_no)
temp <- cr_first2h
outcomecols <- c("AKI_ICU", "aki2or3", "aki12h", "aki24h", "aki48h", "aki2or3in12h", "aki2or3in24h",
                 "aki2or3in48h")
predictorcols <- c("crchange", "delta_cr", "percent_delta_cr")
table <- data.frame()
# Loop across each of the outcomes
for (o in c(1:length(outcomecols))) {
  # Loop across each of the predictors
  for (p in c(1:length(predictorcols))) {
    # Create a dataframe of outcome and predictor
    df<-temp[,c(predictorcols[p], outcomecols[o])]
    colnames(df)[1]<-"pred"
    df$pred=df$pred/10
    colnames(df)[2]<-"outcome"
    df$outcome <- factor(df$outcome)
    # Perform glm
    mod<-glm(outcome~pred,data=df,family="binomial")
    # Summarise the output
    coef<-round(exp(summary(mod)$coefficients[-1,1]),2)
    ci<-round(exp(confint(mod))[-1,],3)
    col1=paste0(coef," [",ci[1],"; ",ci[2],"]")
    # Return the output
    r <-data.frame(
      o = o, p= p,
      outcome = outcomecols[o], predictor = predictorcols[p],
      n.events=length(which(df$outcome %in% levels(df$outcome)[-1])),
      or = coef, ci.low=ci[1], ci.up=ci[2], col1)
    # Combine into the table dataframe
    table=rbind(table,r)
  }
}
# Clean up the output and save as csv
rownames(table) <- NULL
write.csv(table, "cr_first2h_univariate.csv")
# Plot the output
ggplot(table, aes(x = outcome, y = or)) +
  facet_wrap(~predictor, scales = "free_x") +
  theme_bw() +
  geom_hline(yintercept=1, linetype=2, color="grey40") +
  geom_errorbar(aes(ymin=ci.low, ymax=ci.up), width=0.2, size=0.6) +
  geom_point(shape=15, size=4) +
  coord_flip() +
  labs(y="Odds ratio (95% confidence interval)") +
  theme(axis.title.y=element_blank(),axis.ticks.y=element_blank(),
        axis.line.y=element_blank()) +
  ggtitle("cr_first2h")

# GLM unipredictor Cr more than 2 hrs v2 ----
cr_first2hv2 <- cr_first %>% filter(Time_betw_ABG>=2)
temp <- cr_first2hv2
outcomecols <- c("AKI_ICU", "aki2or3", "aki12h", "aki24h", "aki48h", "aki2or3in12h", "aki2or3in24h",
                 "aki2or3in48h")
predictorcols <- c("crchange", "delta_cr", "percent_delta_cr")
table <- data.frame()
# Loop across each of the outcomes
for (o in c(1:length(outcomecols))) {
  # Loop across each of the predictors
  for (p in c(1:length(predictorcols))) {
    # Create a dataframe of outcome and predictor
    df<-temp[,c(predictorcols[p], outcomecols[o])]
    colnames(df)[1]<-"pred"
    df$pred=df$pred/10
    colnames(df)[2]<-"outcome"
    df$outcome <- factor(df$outcome)
    # Perform glm
    mod<-glm(outcome~pred,data=df,family="binomial")
    # Summarise the output
    coef<-round(exp(summary(mod)$coefficients[-1,1]),2)
    ci<-round(exp(confint(mod))[-1,],3)
    col1=paste0(coef," [",ci[1],"; ",ci[2],"]")
    # Return the output
    r <-data.frame(
      o = o, p= p,
      outcome = outcomecols[o], predictor = predictorcols[p],
      n.events=length(which(df$outcome %in% levels(df$outcome)[-1])),
      or = coef, ci.low=ci[1], ci.up=ci[2], col1)
    # Combine into the table dataframe
    table=rbind(table,r)
  }
}
# Clean up the output and save as csv
rownames(table) <- NULL
write.csv(table, "cr_first2hv2_univariate.csv")
# Plot the output
ggplot(table, aes(x = outcome, y = or)) +
  facet_wrap(~predictor, scales = "free_x") +
  theme_bw() +
  geom_hline(yintercept=1, linetype=2, color="grey40") +
  geom_errorbar(aes(ymin=ci.low, ymax=ci.up), width=0.2, size=0.6) +
  geom_point(shape=15, size=4) +
  coord_flip() +
  labs(y="Odds ratio (95% confidence interval)") +
  theme(axis.title.y=element_blank(),axis.ticks.y=element_blank(),
        axis.line.y=element_blank()) +
  ggtitle("cr_first2hv2")
#GLM unipredictor Cr more than 4 hrs ----
cr_first4h <- cr_full %>% filter(Time_betw_ABG>=4) %>% group_by(Pt_Study_no) %>% top_n(-1, Cr_epis_no)
temp <- cr_first4h
outcomecols <- c("AKI_ICU", "aki2or3", "aki12h", "aki24h", "aki48h", "aki2or3in12h", "aki2or3in24h",
                 "aki2or3in48h")
predictorcols <- c("crchange", "delta_cr", "percent_delta_cr")
table <- data.frame()
# Loop across each of the outcomes
for (o in c(1:length(outcomecols))) {
  # Loop across each of the predictors
  for (p in c(1:length(predictorcols))) {
    # Create a dataframe of outcome and predictor
    df<-temp[,c(predictorcols[p], outcomecols[o])]
    colnames(df)[1]<-"pred"
    df$pred=df$pred/10
    colnames(df)[2]<-"outcome"
    df$outcome <- factor(df$outcome)
    # Perform glm
    mod<-glm(outcome~pred,data=df,family="binomial")
    # Summarise the output
    coef<-round(exp(summary(mod)$coefficients[-1,1]),2)
    ci<-round(exp(confint(mod))[-1,],3)
    col1=paste0(coef," [",ci[1],"; ",ci[2],"]")
    # Return the output
    r <-data.frame(
      o = o, p= p,
      outcome = outcomecols[o], predictor = predictorcols[p],
      n.events=length(which(df$outcome %in% levels(df$outcome)[-1])),
      or = coef, ci.low=ci[1], ci.up=ci[2], col1)
    # Combine into the table dataframe
    table=rbind(table,r)
  }
}
# Clean up the output and save as csv
rownames(table) <- NULL
write.csv(table, "cr_first4h_univariate.csv")
# Plot the output
ggplot(table, aes(x = outcome, y = or)) +
  facet_wrap(~predictor, scales = "free_x") +
  theme_bw() +
  geom_hline(yintercept=1, linetype=2, color="grey40") +
  geom_errorbar(aes(ymin=ci.low, ymax=ci.up), width=0.2, size=0.6) +
  geom_point(shape=15, size=4) +
  coord_flip() +
  labs(y="Odds ratio (95% confidence interval)") +
  theme(axis.title.y=element_blank(),axis.ticks.y=element_blank(),
        axis.line.y=element_blank()) +
  ggtitle("cr_first4h")
# Multivariate Analysis cr_first ----
temp <- cr_first
outcomecols <- c("AKI_ICU", "aki2or3", "aki12h", "aki24h", "aki48h", "aki2or3in12h", "aki2or3in24h",
                 "aki2or3in48h")
predictorcols <- c("crchange", "delta_cr", "percent_delta_cr")
variablecols <- c("Age", "APACHE_III", "Baseline_Cr", "PCs_cardio")
table <- data.frame()
# Loop across each of the outcomes
for (o in c(1:length(outcomecols))) {
  # Loop across each of the predictors
  for (p in c(1:length(predictorcols))) {
    # Create a dataframe of outcome and predictor
    df<-temp[,c(predictorcols[p], outcomecols[o], variablecols)]
    colnames(df)[1]<-"pred"
    df$pred=df$pred/10
    colnames(df)[2]<-"outcome"
    colnames(df)[3:ncol(df)] <- paste0("var", 1:length(variablecols))
    # Perform glm with and without pred
    mod.null <- glm(outcome~var1+var2+var3+var4,      data = df, family = "binomial")
    mod.full <- glm(outcome~var1+var2+var3+var4+pred, data = df, family = "binomial")

    glm.pval=round(anova(mod.null,mod.full,test="LRT")[2,5],3)

    # Summarise the output
    coef.null <- round(exp(summary(mod.null)$coefficients[-1,1]),2)
    coef.null.df <- as.data.frame(t(coef.null))
    colnames(coef.null.df) <- paste0("null_", colnames(coef.null.df))
    ci.null   <- round(exp(confint(mod.null))[-1,],3)
    ci.null.df <- as.data.frame(ci.null) %>% cbind(., var = rownames(ci.null)) %>% gather(key, value, -var) %>%
      mutate(var = ifelse(key == "2.5 %", paste0("null_", var,".ci.low"), paste0("null_", var, ".ci.up"))) %>%
      select(-key) %>% arrange(var) %>% t(.)
    colnames(ci.null.df) <- ci.null.df[1,]
    ci.null.df <- as.data.frame(t(as.matrix(ci.null.df[2,])))

    coef.full <- round(exp(summary(mod.full)$coefficients[-1,1]),2)
    coef.full.df <- as.data.frame(t(coef.full))
    colnames(coef.full.df) <- paste0("full_", colnames(coef.full.df))
    ci.full   <- round(exp(confint(mod.full))[-1,],3)
    ci.full.df <- as.data.frame(ci.full) %>% cbind(., var = rownames(ci.full)) %>% gather(key, value, -var) %>%
      mutate(var = ifelse(key == "2.5 %", paste0("full_", var,".ci.low"), paste0("full_", var, ".ci.up"))) %>%
      select(-key) %>% arrange(var) %>% t(.)
    colnames(ci.full.df) <- ci.full.df[1,]
    ci.full.df <- as.data.frame(t(as.matrix(ci.full.df[2,])))

    # Return the output
    r <-data.frame(
      o = o, p= p,
      outcome = outcomecols[o], predictor = predictorcols[p],
      n.events=sum(df$outcome),
      glm.pval = glm.pval)
    r <- cbind(r, coef.null.df, ci.null.df, coef.full.df, ci.full.df)
    # Combine into the table dataframe
    table=rbind(table,r)
  }
}
# Clean up the output and save as csv
rownames(table) <- NULL
write.csv(table, "cr_first_multivariate.csv")

#NRI cr first ----
df<-cr_first

variables.index=c("Age", "APACHE_III", "Baseline_Cr", "PCs_cardio")
test.pred.index=c("delta_cr")


table<-data.frame()
j= "aki2or3in24h" # AKI 23 at 24h
for(i in c(1:length(test.pred.index))){
  index<-c(variables.index,test.pred.index[i])
  temp<-df[,c(j,index)]
  temp<-temp[complete.cases(temp),]

  pred.test.name=colnames(temp)[length(index)+1]
  colnames(temp)[length(index)+1]="pred.test"

  mod.null<-glm(aki2or3in24h~.-pred.test,data=temp,family="binomial",x=T)
  mod.1<-glm(aki2or3in24h~.,data=temp,family="binomial",x=T)
  glm.pval=round(anova(mod.null,mod.1,test="LRT")[2,5],2)

  auc=round(roc(aki2or3in24h~predict(mod.1),data=temp)$auc,2)

  res=nribin(mdl.std=mod.null,mdl.new=mod.1,cut=0.1,niter=0,updown='diff',msg=F)
  nri=round(res$nri[c(1:3),1],2)

  r=data.frame(pred=pred.test.name,glm.pval,auc,nri.all=nri[1],nri.pos=nri[2],nri.neg=nri[3])
  table=rbind(table,r)
}
print(paste("AUC of null model =",round(roc(aki2or3in24h~predict(mod.null),data=temp)$auc,2)))
write.csv(table, "cr_first_nri.csv")

#get confidence interval
nribin(mdl.std=mod.null,mdl.new=mod.1,cut=0.1,niter=1000,updown='diff', alpha = 0.05,msg=F)

#multivariate analysis cr more than 2 hrs v1 ----
cr_first2h <- cr_full %>% filter(Time_betw_ABG>=2) %>% group_by(Pt_Study_no) %>% top_n(-1, Cr_epis_no)
temp <- cr_first2h
outcomecols <- c("AKI_ICU", "aki2or3", "aki12h", "aki24h", "aki48h", "aki2or3in12h", "aki2or3in24h",
                 "aki2or3in48h")
predictorcols <- c("crchange", "delta_cr", "percent_delta_cr")
variablecols <- c("Age", "APACHE_II", "APACHE_III", "Baseline_Cr", "PCs_cardio")
table <- data.frame()
# Loop across each of the outcomes
for (o in c(1:length(outcomecols))) {
  # Loop across each of the predictors
  for (p in c(1:length(predictorcols))) {
    # Create a dataframe of outcome and predictor
    df<-temp[,c(predictorcols[p], outcomecols[o], variablecols)]
    colnames(df)[1]<-"pred"
    df$pred=df$pred/10
    colnames(df)[2]<-"outcome"
    colnames(df)[3:ncol(df)] <- paste0("var", 1:length(variablecols))
    # Perform glm with and without pred
    mod.null <- glm(outcome~var1+var2+var3+var4+var5,      data = df, family = "binomial")
    mod.full <- glm(outcome~var1+var2+var3+var4+var5+pred, data = df, family = "binomial")

    glm.pval=round(anova(mod.null,mod.full,test="LRT")[2,5],2)

    # Summarise the output
    coef.null <- round(exp(summary(mod.null)$coefficients[-1,1]),2)
    coef.null.df <- as.data.frame(t(coef.null))
    colnames(coef.null.df) <- paste0("null_", colnames(coef.null.df))
    ci.null   <- round(exp(confint(mod.null))[-1,],3)
    ci.null.df <- as.data.frame(ci.null) %>% cbind(., var = rownames(ci.null)) %>% gather(key, value, -var) %>%
      mutate(var = ifelse(key == "2.5 %", paste0("null_", var,".ci.low"), paste0("null_", var, ".ci.up"))) %>%
      select(-key) %>% arrange(var) %>% t(.)
    colnames(ci.null.df) <- ci.null.df[1,]
    ci.null.df <- as.data.frame(t(as.matrix(ci.null.df[2,])))

    coef.full <- round(exp(summary(mod.full)$coefficients[-1,1]),2)
    coef.full.df <- as.data.frame(t(coef.full))
    colnames(coef.full.df) <- paste0("full_", colnames(coef.full.df))
    ci.full   <- round(exp(confint(mod.full))[-1,],3)
    ci.full.df <- as.data.frame(ci.full) %>% cbind(., var = rownames(ci.full)) %>% gather(key, value, -var) %>%
      mutate(var = ifelse(key == "2.5 %", paste0("full_", var,".ci.low"), paste0("full_", var, ".ci.up"))) %>%
      select(-key) %>% arrange(var) %>% t(.)
    colnames(ci.full.df) <- ci.full.df[1,]
    ci.full.df <- as.data.frame(t(as.matrix(ci.full.df[2,])))

    # Return the output
    r <-data.frame(
      o = o, p= p,
      outcome = outcomecols[o], predictor = predictorcols[p],
      n.events=sum(df$outcome),
      glm.pval = glm.pval)
    r <- cbind(r, coef.null.df, ci.null.df, coef.full.df, ci.full.df)
    # Combine into the table dataframe
    table=rbind(table,r)
  }
}
# Clean up the output and save as csv
rownames(table) <- NULL
write.csv(table, "cr_first2h_multivariate.csv")

#cr ep univariate for mortality & LOS ----
#use dataframe called fulldemo
#mortality
dim(fulldemo)
colnames(fulldemo)
fulldemo <- fulldemo %>% mutate(ICU_mortality = ifelse(Dc_ICU_Alive == 1, 0, 1),
                                Hosp_mortality = ifelse(Dc_Hosp_Alive == 1 , 0, 1))
outcomecols <- c("ICU_mortality", "Hosp_mortality")
table2 <- data.frame()
for (o in outcomecols){
  mod<-glm(paste0(o, "~cr_ep"),data=fulldemo,family="binomial")
  coef<-round(exp(summary(mod)$coefficients[-1,1]),2)
  ci<-round(exp(confint(mod))[-1,],3)
  col1=paste0(coef," [",ci[1],"; ",ci[2],"]")
  r <-data.frame(
    o = o, p= "cr_ep",
    outcome = o,
    n.events=sum(na.omit(fulldemo[o])),
    or = coef, ci.low=ci[1], ci.up=ci[2], col1)
  # Combine into the table dataframe
  table2=rbind(table2,r)
}
rownames(table2) <- NULL
table2
#mortality for crchange,deltacr and % delta cr done in cr_first unipredictor glm
#LOS
#dichotomous predictor
cor(fulldemo$cr_ep, fulldemo$ICU_LOS)
asdf <- fulldemo %>% filter(!is.na(Hosp_LOS))
cor(asdf$cr_ep, asdf$Hosp_LOS)
plot(fulldemo$cr_ep, fulldemo$ICU_LOS)
plot(fulldemo$cr_ep, fulldemo$Hosp_LOS)
summary(lm(ICU_LOS~cr_ep, data=fulldemo))
summary(lm(Hosp_LOS~cr_ep, data=fulldemo))
publish(lm(ICU_LOS~cr_ep, data=fulldemo))
publish(lm(Hosp_LOS~cr_ep, data=fulldemo))
#log transform data before doing dichotomous predictor
publish(lm(log(ICU_LOS)~cr_ep, data=fulldemo))
publish(lm(log(Hosp_LOS)~cr_ep, data=fulldemo))
#continuous predictors ICU LOS
cor(cr_first$crchange, cr_first$ICU_LOS)
cor(cr_first$delta_cr, cr_first$ICU_LOS)
cor(cr_first$percent_delta_cr, cr_first$ICU_LOS)
plot(cr_first$delta_cr, cr_first$ICU_LOS)
plot(cr_first$crchange, cr_first$ICU_LOS)
plot(cr_first$percent_delta_cr, cr_first$ICU_LOS)
summary(lm(ICU_LOS~crchange, data = cr_first))
summary(lm(ICU_LOS~delta_cr, data = cr_first))
summary(lm(ICU_LOS~percent_delta_cr, data = cr_first))
publish(lm(ICU_LOS~crchange, data = cr_first))
publish(lm(ICU_LOS~delta_cr, data = cr_first))
publish(lm(ICU_LOS~percent_delta_cr, data = cr_first))
#continuous predictors ICU LOS with log transformation
publish(lm(log(ICU_LOS)~crchange, data = cr_first))
publish(lm(log(ICU_LOS)~delta_cr, data = cr_first))
publish(lm(log(ICU_LOS)~percent_delta_cr, data = cr_first))
#continuous predictors Hosp LOS
asdf <- cr_first %>% filter(!is.na(Hosp_LOS))
cor(asdf$crchange, asdf$Hosp_LOS)
cor(asdf$delta_cr, asdf$Hosp_LOS)
cor(asdf$percent_delta_cr, asdf$Hosp_LOS)
plot(cr_first$delta_cr, cr_first$Hosp_LOS)
plot(cr_first$crchange, cr_first$Hosp_LOS)
plot(cr_first$percent_delta_cr, cr_first$Hosp_LOS)
summary(lm(Hosp_LOS~crchange, data = cr_first))
summary(lm(Hosp_LOS~delta_cr, data = cr_first))
summary(lm(Hosp_LOS~percent_delta_cr, data = cr_first))
publish(lm(Hosp_LOS~crchange, data = cr_first))
publish(lm(Hosp_LOS~delta_cr, data = cr_first))
publish(lm(Hosp_LOS~percent_delta_cr, data = cr_first))
rm(asdf)
#continuous predictors Hosp LOS with log transformation
publish(lm(log(Hosp_LOS)~crchange, data = cr_first))
publish(lm(log(Hosp_LOS)~delta_cr, data = cr_first))
publish(lm(log(Hosp_LOS)~percent_delta_cr, data = cr_first))

#cr defined AKI analyses ----
sum(cr_first$Cr_defined_AKI)
sum(olionly_first$Cr_defined_AKI)
sum(cr_first$craki2or3in24h)
sum(olionly_first$craki2or3in24h)
asdf <- cr_first %>% filter(Cr_defined_AKI == 1)
summary(asdf$ICUadmtoAKIDx)
summary(cr_first$ICUadmtoAKIDx)
qwer <- olionly_first %>% filter(Cr_defined_AKI == 1)
summary(qwer$ICUadmtoAKIDx)
icutoaki <- rbind(asdf[, c("Age", "ICUadmtoAKIDx")], qwer[,c("Age", "ICUadmtoAKIDx")])
summary(icutoaki$ICUadmtoAKIDx)
wilcox.test(asdf$ICUadmtoAKIDx, qwer$ICUadmtoAKIDx)
AKItable <- as.table(rbind(c(sum(cr_first$Cr_defined_AKI),
                             sum(olionly_first$Cr_defined_AKI)),
                           c(nrow(cr_first) - sum(cr_first$Cr_defined_AKI),
                             108 - sum(olionly_first$Cr_defined_AKI))))
dimnames(AKItable) <- list(c("AKI", "no AKI"), c("cr", "no cr"))
AKItable
chisq.test(AKItable, correct = FALSE)
AKIs1table <- as.table(rbind(c(sum(na.omit(cr_first$Cr_defined_AKI_stage == "1")),
                               sum(na.omit(olionly_first$Cr_defined_AKI_stage == "1"))),
                             c(sum(cr_first$Cr_defined_AKI) - sum(na.omit(cr_first$Cr_defined_AKI_stage == "1")),
                               sum(olionly_first$Cr_defined_AKI) - sum(na.omit(olionly_first$Cr_defined_AKI_stage == "1")))))
dimnames(AKIs1table) <- list(c("AKIs1", "not AKIs1"), c("cr", "no cr"))
AKIs1table
fisher.test(AKIs1table)
AKIs2table <- as.table(rbind(c(sum(na.omit(cr_first$Cr_defined_AKI_stage == "2")),
                               sum(na.omit(olionly_first$Cr_defined_AKI_stage == "2"))),
                             c(sum(cr_first$Cr_defined_AKI) - sum(na.omit(cr_first$Cr_defined_AKI_stage == "2")),
                               sum(olionly_first$Cr_defined_AKI) - sum(na.omit(olionly_first$Cr_defined_AKI_stage == "2")))))
dimnames(AKIs2table) <- list(c("AKIs2", "not AKIs2"), c("cr", "no cr"))
AKIs2table
fisher.test(AKIs2table)
AKIs3table <- as.table(rbind(c(sum(na.omit(cr_first$Cr_defined_AKI_stage == "3")),
                               sum(na.omit(olionly_first$Cr_defined_AKI_stage == "3"))),
                             c(sum(cr_first$Cr_defined_AKI) - sum(na.omit(cr_first$Cr_defined_AKI_stage == "3")),
                               sum(olionly_first$Cr_defined_AKI) - sum(na.omit(olionly_first$Cr_defined_AKI_stage == "3")))))
dimnames(AKIs3table) <- list(c("AKIs3", "not AKIs3"), c("cr", "no cr"))
AKIs3table
fisher.test(AKIs3table)
#GLM cr episode for cr defined AKI ----
#make dataframe from cr_first, olionly_first, scr_out$neit
scr_out$neit$Cr_defined_AKI <- 0
scr_out$neit$crep <- 0
cr_first$crep <- 1
olionly_first$crep <- 0
nrow(cr_first) + nrow(olionly_first) + nrow(scr_out$neit)
outcomecols <- c("Cr_defined_AKI", "craki2or3", "craki12h", "craki24h", "craki48h", "craki2or3in12h",
                 "craki2or3in24h", "craki2or3in48h")
scr_out$neit$craki2or3 <- 0
scr_out$neit$craki12h <- 0
scr_out$neit$craki24h <- 0
scr_out$neit$craki48h <- 0
scr_out$neit$craki2or3in12h <- 0
scr_out$neit$craki2or3in24h <- 0
scr_out$neit$craki2or3in48h <- 0
crunivar <- rbind(cr_first[ ,c("Age", "crep", outcomecols)],
                  olionly_first[ ,c("Age", "crep", outcomecols)],
                  scr_out$neit[ ,c("Age", "crep", outcomecols)])
table <- data.frame()
for (o in outcomecols){
  mod<-glm(paste0(o, "~crep"),data=crunivar,family="binomial")
  pval<-coef(summary(mod))[2,4]
  coef<-round(exp(summary(mod)$coefficients[-1,1]),2)
  ci<-round(exp(confint(mod))[-1,],3)
  col1=paste0(coef," [",ci[1],"; ",ci[2],"]")
  r <-data.frame(
    o = o, p= "crep",
    outcome = o,
    n.events=sum(crunivar[o]),
    pval = pval,
    or = coef, ci.low=ci[1], ci.up=ci[2], col1)
  # Combine into the table dataframe
  table=rbind(table,r)
}
rownames(table) <- NULL
table
write.csv(table, "cr_definedAKI_univariate.csv")
#se sp ppv npv
crtable <- as.table(rbind(c(36, 243), c(1, 107)))
dimnames(crtable) <- list(c("cr ep", "no cr ep"), c("AKI", "no AKI"))
crtable
summary(epi.tests(crtable, conf.level=0.95))
sum(cr_first$craki2or3in48h)
#glm cr change for cr defined AKI ----
temp <- cr_first
outcomecols <- c("Cr_defined_AKI", "craki2or3", "craki12h", "craki24h", "craki48h", "craki2or3in12h",
                 "craki2or3in24h", "craki2or3in48h")
predictorcols <- c("crchange", "delta_cr", "percent_delta_cr")
table <- data.frame()
# Loop across each of the outcomes
for (o in c(1:length(outcomecols))) {
  # Loop across each of the predictors
  for (p in c(1:length(predictorcols))) {
    # Create a dataframe of outcome and predictor
    df<-temp[,c(predictorcols[p], outcomecols[o])]
    colnames(df)[1]<-"pred"
    df$pred=df$pred/10
    colnames(df)[2]<-"outcome"
    df$outcome <- factor(df$outcome)
    # Perform glm
    mod<-glm(outcome~pred,data=df,family="binomial")
    # Summarise the output
    coef<-round(exp(summary(mod)$coefficients[-1,1]),2)
    ci<-round(exp(confint(mod))[-1,],3)
    col1=paste0(coef," [",ci[1],"; ",ci[2],"]")
    # Return the output
    r <-data.frame(
      o = o, p= p,
      outcome = outcomecols[o], predictor = predictorcols[p],
      n.events=length(which(df$outcome %in% levels(df$outcome)[-1])),
      or = coef, ci.low=ci[1], ci.up=ci[2], col1)
    # Combine into the table dataframe
    table=rbind(table,r)
  }
}
# Clean up the output and save as csv
rownames(table) <- NULL
table
write.csv(table, "crdefinedaki_first_univariate.csv")

#AUROC cr change for cr defined AKI
#delta cr
roc(cr_first$Cr_defined_AKI, cr_first$delta_cr)
wilcox.test(delta_cr~Cr_defined_AKI, cr_first)
roc(cr_first$craki2or3, cr_first$delta_cr)
wilcox.test(delta_cr~craki2or3, cr_first)
roc(cr_first$craki12h, cr_first$delta_cr)
wilcox.test(delta_cr~craki12h, cr_first)
roc(cr_first$craki24h, cr_first$delta_cr)
wilcox.test(delta_cr~craki24h, cr_first)
roc(cr_first$craki48h, cr_first$delta_cr)
wilcox.test(delta_cr~craki48h, cr_first)
roc(cr_first$craki2or3in12h, cr_first$delta_cr)
wilcox.test(delta_cr~craki2or3in12h, cr_first)
roc(cr_first$craki2or3in24h, cr_first$delta_cr)
wilcox.test(delta_cr~craki2or3in24h, cr_first)
roc(cr_first$craki2or3in48h, cr_first$delta_cr)
wilcox.test(delta_cr~craki2or3in48h, cr_first)
#percent delta cr
roc(cr_first$Cr_defined_AKI, cr_first$percent_delta_cr)
wilcox.test(percent_delta_cr~Cr_defined_AKI, cr_first)
roc(cr_first$craki2or3, cr_first$percent_delta_cr)
wilcox.test(percent_delta_cr~craki2or3, cr_first)
roc(cr_first$craki12h, cr_first$percent_delta_cr)
wilcox.test(percent_delta_cr~craki12h, cr_first)
roc(cr_first$craki24h, cr_first$percent_delta_cr)
wilcox.test(percent_delta_cr~craki24h, cr_first)
roc(cr_first$craki48h, cr_first$percent_delta_cr)
wilcox.test(percent_delta_cr~craki48h, cr_first)
roc(cr_first$craki2or3in12h, cr_first$percent_delta_cr)
wilcox.test(percent_delta_cr~craki2or3in12h, cr_first)
roc(cr_first$craki2or3in24h, cr_first$percent_delta_cr)
wilcox.test(percent_delta_cr~craki2or3in24h, cr_first)
roc(cr_first$craki2or3in48h, cr_first$percent_delta_cr)
wilcox.test(percent_delta_cr~craki2or3in48h, cr_first)
#optimal cutpoint
roc(cr_first$craki2or3in24h, cr_first$delta_cr)
cr_first <- as.data.frame(cr_first)
optimal.cutpoint.Youden <- optimal.cutpoints(X = "delta_cr", status = "craki2or3in24h", tag.healthy = 0,
                                             methods = "Youden", data = cr_first, conf.level = 0.95,
                                             ci.fit=TRUE)
summary(optimal.cutpoint.Youden)
#NRI for cr defined AKI ----
df<-cr_first

variables.index=c("Age", "APACHE_III", "Baseline_Cr", "PCs_cardio")
test.pred.index=c("delta_cr")


table<-data.frame()
j= "craki2or3in24h" # AKI 23 at 24h
for(i in c(1:length(test.pred.index))){
  index<-c(variables.index,test.pred.index[i])
  temp<-df[,c(j,index)]
  temp<-temp[complete.cases(temp),]

  pred.test.name=colnames(temp)[length(index)+1]
  colnames(temp)[length(index)+1]="pred.test"

  mod.null<-glm(craki2or3in24h~.-pred.test,data=temp,family="binomial",x=T)
  mod.1<-glm(craki2or3in24h~.,data=temp,family="binomial",x=T)
  glm.pval=round(anova(mod.null,mod.1,test="LRT")[2,5],2)

  auc=round(roc(craki2or3in24h~predict(mod.1),data=temp)$auc,2)

  res=nribin(mdl.std=mod.null,mdl.new=mod.1,cut=0.1,niter=0,updown='diff',msg=F)
  nri=round(res$nri[c(1:3),1],2)

  r=data.frame(pred=pred.test.name,glm.pval,auc,nri.all=nri[1],nri.pos=nri[2],nri.neg=nri[3])
  table=rbind(table,r)
}
print(paste("AUC of null model =",round(roc(craki2or3in24h~predict(mod.null),data=temp)$auc,2)))
write.csv(table, "cr_definedAKI_nri.csv")

#get confidence interval
nribin(mdl.std=mod.null,mdl.new=mod.1,cut=0.1,niter=1000,updown='diff', alpha = 0.05,msg=F)
