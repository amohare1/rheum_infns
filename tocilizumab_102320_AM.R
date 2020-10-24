# This code imports and analyzes RPDR data for the RHEUM-INFNS project
# July 2019; October 2019; January 2020; June 2020; October 2020
rm(list = ls())
getwd()
#Import libraries and set working directory
library(tidyverse)
library(lubridate)
library(stringr)
## Contents
## 1. Medications
## 2. Encounters
## 2.5. Diagnosis
## 3. Lab data
## 4. Date comparisons
## 5. Demographics
## 6. A_B List
##
##### IMPORT
dem_in<-read.table("EPH10_20191208_142820_Dem.txt", sep = "|", fill = TRUE, header = TRUE, quote = "")
med_in<-read.table("EPH10_20191208_142820_Med.txt", sep = "|", fill = TRUE, header = TRUE, quote = "")
enc_in<-read.table("EPH10_20191208_142820_Enc.txt", sep = "|", fill = TRUE, header = TRUE, quote = "")
lab_in <-read.table("EPH10_20191208_142820_Lab.txt", sep = "|", fill = TRUE, header = TRUE, quote = "")
######
# Biologic and DMARD variables
######
biol_b <- grepl(paste("Certolizumab|Cimzia|Etanercept|Enbrel|Adalimumab|Humira|Anakinra|Kineret|",
                    "Canakinumab|Ilaris|Abatacept|Orencia|Infliximab|Remicade|Rituximab|Rituxan|",
                    "Golimumab|Simponi|Ustekinumab|Stelara|Secukinumab|Cosentyx|Ixekizumab|Taltz|",
                    "Baricitinib|Olumiant|Apremilast|Sarilumab|Kevzara|Belimumab|Benlysta|tocilizumab|Actemra",sep=""),
              med_in$Medication,ignore.case=T)
biol_a <- grepl(paste("Certolizumab|Cimzia|Etanercept|Enbrel|Adalimumab|Humira|Anakinra|Kineret|",
                              "Canakinumab|Ilaris|Abatacept|Orencia|Infliximab|Remicade|Rituximab|Rituxan|",
                              "Golimumab|Simponi|Ustekinumab|Stelara|Secukinumab|Cosentyx|Ixekizumab|Taltz|",
                              "Baricitinib|Olumiant|Apremilast|Sarilumab|Kevzara|Belimumab|Benlysta|tofacitinib|xeljanz",sep=""),
                        med_in$Medication,ignore.case=T)
biol_ab <- grepl(paste("Certolizumab|Cimzia|Etanercept|Enbrel|Adalimumab|Humira|Anakinra|Kineret|",
                    "Canakinumab|Ilaris|Abatacept|Orencia|Infliximab|Remicade|Rituximab|Rituxan|",
                    "Golimumab|Simponi|Ustekinumab|Stelara|Secukinumab|Cosentyx|Ixekizumab|Taltz|",
                    "Baricitinib|Olumiant|Apremilast|Sarilumab|Kevzara|Belimumab|Benlysta",sep=""),
              med_in$Medication,ignore.case=T)
non_biol <- grepl(paste("Hydroxychloroquine|Plaquenil|Leflunomide|Arava|Methotrexate|Otrexup|Xatmep|Trexall|Rasuvo|",
                        "Sulfasalazine|Azulfidine|Azathioprine|Imuran|Azasan|Mycophenolate|Myfortic|Cellcept|", 
                        "Cyclosporine|NeoralGengraf|Sandimmune",sep=""),med_in$Medication,ignore.case=T) 
sum(non_biol) #22004
# Extract tocilizumab (drug_a) from the medication file: 
drug_a <- grepl("tocilizumab|actemra",med_in$Medication,ignore.case=T)
# Extract tofacitinib (drug_b) from the medication file
drug_b <- grepl("tofacitinib|xeljanz",med_in$Medication,ignore.case=T)
##
##
##
# 1. Medications
med_a <- select(med_in[drug_a,], EMPI, MRN_Type, MRN, Med = Medication, Med_Date = Medication_Date, Med_Clinic = Clinic, Encounter_number)
length(unique(med_a$EMPI)) #913 unique EMPIs
med_b <- select(med_in[drug_b,], EMPI, MRN_Type, MRN, Med = Medication, Med_Date = Medication_Date, Med_Clinic = Clinic, Encounter_number)
length(unique(med_b$EMPI)) #630 unique EMPIs
# Exclude <18yo; keep only adults
med_a <- med_a[med_a$EMPI %in% dem_in$EMPI[dem_in$Age > 17],]
length(unique(med_a$EMPI)) #911
med_b <- med_b[med_b$EMPI %in% dem_in$EMPI[dem_in$Age > 17],]
length(unique(med_b$EMPI)) #627

# Dataframe with the first medication date
a_first <- med_a[order(med_a$EMPI, mdy(med_a$Med_Date), decreasing = FALSE),]
a_first <- a_first[-which(duplicated(a_first$EMPI)),]
b_first <- med_b[order(med_b$EMPI, mdy(med_b$Med_Date), decreasing = FALSE),]
b_first <- b_first[-which(duplicated(b_first$EMPI)),]
# Confirm the same EMPIs are kept
sum(a_first$EMPI %in% med_a$EMPI) #911
sum(b_first$EMPI %in% med_b$EMPI) #627

# Combine toci/tofa dataframes; for EMPIs with both tofa/toci, keep only the drug administered first
ab_first <- rbind(a_first, b_first)
ab_first <- ab_first[order(ab_first$EMPI, mdy(ab_first$Med_Date), decreasing = FALSE),]
dim(ab_first) # 1538
ab_first <- ab_first[-which(duplicated(ab_first$EMPI)),]
dim(ab_first) #1391 x 7
1538 - 1391 # Number of patients receiving both = 147

# Extract Biologics from the medication file
biologics_a <- select(med_in[biol_a,], EMPI, MRN_Type, MRN, 
                      Med = Medication, Med_Date = Medication_Date, Med_Clinic = Clinic, Encounter_number)

# Dataframe with the first biologics date
biologicsa_first <- biologics_a[order(biologics_a$EMPI, mdy(biologics_a$Med_Date), decreasing = FALSE),]
biologicsa_first <- biologicsa_first[-which(duplicated(biologicsa_first$EMPI)),]

# confirm the same EMPIs are kept
sum(biologicsa_first$EMPI %in% biologics_a$EMPI) #1097 #### Serena 1094? 102499248 102799183 103891683####
### Amir: I am still obtaining 1097

# Extract non-biologic DMARDs
dmard <- select(med_in[non_biol,], EMPI, MRN_Type, MRN, Med = Medication, Med_Date = Medication_Date, Med_Clinic = Clinic, Encounter_number)
length(unique(dmard$EMPI)) #1053 unique EMPIs
# Keep only the first dmard medication date
dmard_first <- dmard[order(dmard$EMPI, mdy(dmard$Med_Date), decreasing = FALSE),]
dmard_first <- dmard_first[-which(duplicated(dmard_first$EMPI)),]
# confirm the same EMPIs are kept
sum(dmard_first$EMPI %in% dmard$EMPI) #1053

# Keep only the first biologics with EMPIs associated with drug_a
biologics_first_a <- filter(biologicsa_first, EMPI %in% a_first$EMPI)
# confirm the same EMPIs are kept
sum(biologics_first_a$EMPI %in% a_first$EMPI) #592
# This implies that 592 patients who received toci also received another biologic

# Keep only the first dmards with EMPIs associated with drug_a
dmard_first_a <- filter(dmard_first, EMPI %in% a_first$EMPI)
# confirm the same EMPIs are kept
sum(dmard_first_a$EMPI %in% a_first$EMPI) #637
#This means that 637 of the drug_a patients also received a non-biologic DMARD

#Repeat the same for drug_b: Extract biologics/dmards
biologics_b <- select(med_in[biol_b,], EMPI, MRN_Type, MRN, 
                      Med = Medication, Med_Date = Medication_Date, Med_Clinic = Clinic, Encounter_number)
# Keep only the first biologics medication date
biologicsb_first <- biologics_b[order(biologics_b$EMPI, mdy(biologics_b$Med_Date), decreasing = FALSE),]
biologicsb_first <- biologicsb_first[-which(duplicated(biologicsb_first$EMPI)),]
biologics_first_b <- filter(biologicsb_first, EMPI %in% b_first$EMPI)
# confirm the same EMPIs are kept
sum(biologics_first_b$EMPI %in% b_first$EMPI) #467
# This implies that 467 patients who received tofa also received a biologic
# Keep only the first dmards with EMPIs associated with drug_b
dmard_first_b <- filter(dmard_first, EMPI %in% b_first$EMPI)
sum(dmard_first_b$EMPI %in% b_first$EMPI) #530
#530/627 of the drug_b patients also received a non-biologic DMARD

#Repeat for ab
biologics_ab <- select(med_in[biol_ab,], EMPI, MRN_Type, MRN, Med = Medication, Med_Date = Medication_Date, Med_Clinic = Clinic, Encounter_number)
biologicsab_first <- biologics_ab[order(biologics_ab$EMPI, mdy(biologics_ab$Med_Date), decreasing = FALSE),]
biologicsab_first <- biologicsab_first[-which(duplicated(biologicsab_first$EMPI)),]
biologics_first_ab <- filter(biologicsab_first, EMPI %in% ab_first$EMPI)
sum(biologics_first_ab$EMPI %in% ab_first$EMPI) #900
dmard_first_ab <- filter(dmard_first, EMPI %in% ab_first$EMPI)
sum(dmard_first_ab$EMPI %in% ab_first$EMPI) #1026
##
##
##
# 2. Encounters
# Keep only outpatient encounters
outpt_enc <- select(enc_in[enc_in$Inpatient_Outpatient == "Outpatient",], 
                    EMPI, MRN_Type, MRN, Encounter_number, Enc_Clinic = Clinic_Name, 
                    Enc_Date = Admit_Date, Principle_Diagnosis)
# Find outpatient encounters associated with Rheum or Arthritis clinics
rheum_enc <- grepl("rheu|arth",outpt_enc$Enc_Clinic,ignore.case=T)
sum(rheum_enc) #77 445
outpt_enc_rheum <- outpt_enc[rheum_enc,]
nrow(outpt_enc_rheum) #77 445
length(unique(outpt_enc_rheum$EMPI)) #1151 unique EMPIs

#Keep only outpatient rheum encounters with drug a and b. Then determine the number of pts associated
#with rheum encounters within 90 days before or 30 days after first med date.
#New inclusion criteria (Oct 2020)
outpt_enc_rheum_ab <- filter(outpt_enc_rheum, EMPI %in% ab_first$EMPI)
outpt_enc_rheum_ab$Med_Date <- ab_first$Med_Date[match(outpt_enc_rheum_ab$EMPI, ab_first$EMPI)]

outpt_enc_rheum_ab$enc_med_date_diff <- 
  (0 <= (mdy(outpt_enc_rheum_ab$Med_Date) + 30 - mdy(outpt_enc_rheum_ab$Enc_Date))) &
  ((mdy(outpt_enc_rheum_ab$Med_Date) + 30 - mdy(outpt_enc_rheum_ab$Enc_Date) <= 120))

length(unique(outpt_enc_rheum_ab$EMPI[outpt_enc_rheum_ab$enc_med_date_diff == TRUE])) #936

outpt_enc_rheum_a <- filter(outpt_enc_rheum, EMPI %in% a_first$EMPI)
outpt_enc_rheum_a$Med_Date <- a_first$Med_Date[match(outpt_enc_rheum_a$EMPI, a_first$EMPI)]
outpt_enc_rheum_a$enc_med_date_diff <- 
  (0 <= (mdy(outpt_enc_rheum_a$Med_Date) + 30 - mdy(outpt_enc_rheum_a$Enc_Date))) &
  ((mdy(outpt_enc_rheum_a$Med_Date) + 30 - mdy(outpt_enc_rheum_a$Enc_Date) <= 120))

length(unique(outpt_enc_rheum_a$EMPI[outpt_enc_rheum_a$enc_med_date_diff == TRUE])) #678 -> drug_a

outpt_enc_rheum_b <- filter(outpt_enc_rheum, EMPI %in% b_first$EMPI)
outpt_enc_rheum_b$Med_Date <- b_first$Med_Date[match(outpt_enc_rheum_b$EMPI, b_first$EMPI)]
outpt_enc_rheum_b$enc_med_date_diff <- 
  (0 <= (mdy(outpt_enc_rheum_b$Med_Date) + 30 - mdy(outpt_enc_rheum_b$Enc_Date))) &
  ((mdy(outpt_enc_rheum_b$Med_Date) + 30 - mdy(outpt_enc_rheum_b$Enc_Date) <= 120))

length(unique(outpt_enc_rheum_b$EMPI[outpt_enc_rheum_b$enc_med_date_diff == TRUE])) #391 -> drug_b

# Make list of unique EMPIs to include in the analysis (enc_med_date_diff == TRUE)
ab_included <- outpt_enc_rheum_ab$EMPI[outpt_enc_rheum_ab$enc_med_date_diff == TRUE] %>%
  unique()
b_included <- outpt_enc_rheum_b$EMPI[outpt_enc_rheum_b$enc_med_date_diff == TRUE] %>%
  unique()

# Keep only the first outpatient encounter date
outpt_enc_first <- outpt_enc[order(outpt_enc$EMPI, mdy(outpt_enc$Enc_Date), decreasing = FALSE),]
outpt_enc_first <- outpt_enc_first[-which(duplicated(outpt_enc_first$EMPI)),]

# confirm the same EMPIs are kept
sum(outpt_enc_first$EMPI %in% outpt_enc$EMPI) #1464
# Keep only the first outpatient encounters with EMPIs among the included patients
ab_included <- filter(outpt_enc_first, EMPI %in% outpt_enc_rheum_ab$EMPI[outpt_enc_rheum_ab$enc_med_date_diff == TRUE])
sum(ab_included$EMPI %in% ab_first$EMPI) #936
ab_included$Med_date <- ab_first$Med_Date[match(ab_included$EMPI, ab_first$EMPI)]
a_included <- filter(outpt_enc_first, EMPI %in% outpt_enc_rheum_a$EMPI[outpt_enc_rheum_a$enc_med_date_diff == TRUE])
sum(a_included$EMPI %in% a_first$EMPI) #678
a_included$Med_date <- a_first$Med_Date[match(a_included$EMPI, a_first$EMPI)]
b_included <- filter(outpt_enc_first, EMPI %in% outpt_enc_rheum_b$EMPI[outpt_enc_rheum_b$enc_med_date_diff == TRUE])
sum(b_included$EMPI %in% b_first$EMPI) #391
b_included$Med_date <- b_first$Med_Date[match(b_included$EMPI, b_first$EMPI)]
##
##
##
# 2.5. Diagnosis
# Mode function
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
# Remove the missing values from Principle Diagnosis.
outpt_enc_rheum$Principle_Diagnosis[outpt_enc_rheum$Principle_Diagnosis == ""] <- NA 
outpt_rheum_enc_DX <- outpt_enc_rheum[!is.na(outpt_enc_rheum$Principle_Diagnosis),] 

# Pass  Mode function to the Principle_Diagnosis for each EMPI -- create a new array called diagnosis_rheum
diagnosis_rheum <- outpt_enc_rheum %>%
  group_by(EMPI) %>%
  mutate(dx_mode = Mode(Principle_Diagnosis))
### Serena: I guess that you want to grab the most freq diagnosis for each empi, and add it to dx_mode
### Amir: Yes, this section of code is trying to make a file with all the primary diagnosis for each rheumatology encounter

# Create a column which has the mode Principle Diagnosis from all the rheum encounters for each EMPI
a_included$dx_mode <- diagnosis_rheum$dx_mode[match(a_included$EMPI, diagnosis_rheum$EMPI)]
b_included$dx_mode <- diagnosis_rheum$dx_mode[match(b_included$EMPI, diagnosis_rheum$EMPI)]
ab_included$dx_mode <- diagnosis_rheum$dx_mode[match(ab_included$EMPI, diagnosis_rheum$EMPI)]
# Export Diagnosis File
# Create a dataframe that has each mode diagnosis with the number of subjects it is associated with
diagnosis_list_a <- a_included %>% 
  group_by(dx_mode) %>% 
  tally()
# Write to a csv file
write.csv(diagnosis_list_a, "toci_diagnosis_102420.csv")
#Same for B
diagnosis_list_b <- b_included %>% 
  group_by(dx_mode) %>% 
  tally()
# Write to a csv file
write.csv(diagnosis_list_b, "tofa_diagnosis_102420.csv")
##
##
##
# 3. Lab Data
labs <- select(lab_in, EMPI, MRN_Type, MRN, Lab_Date = Seq_Date_Time, Lab_test = Group_Id, Test_Id, Test_Description, Result)
# Keep only HBV SAg tests
lab_SAg <- labs[labs$Lab_test == "HBV Surface Ag",]
# Keep only the first HBV SAg test date
#lab_SAg_first <- lab_SAg[order(lab_SAg$EMPI, mdy_hm(lab_SAg$Lab_Date), decreasing = FALSE),] ### Serena: three date didn't convert to date time. row 170, 218, 1495, test code: which(is.na(mdy_hm(lab_SAg$Lab_Date)))
#lab_SAg_first <- lab_SAg_first[-which(duplicated(lab_SAg_first$EMPI)),] 
### Serena: Suggestion: like to convert it to string, easy to compaire. 
### code: lab_SAg$date <- paste(substr(lab_SAg$Lab_Date, 7,10), substr(lab_SAg$Lab_Date, 1,1), substr(lab_SAg$Lab_Date, 4,5), sep = "")
### Amir: I am not getting the correct months in this string; will use this instead: 
lab_SAg$date <- paste(substr(lab_SAg$Lab_Date, 7,10), substr(lab_SAg$Lab_Date, 1,2), substr(lab_SAg$Lab_Date, 4,5), sep = "")
lab_SAg_first <- lab_SAg[order(lab_SAg$EMPI, lab_SAg$date, decreasing = FALSE),]
lab_SAg_first <- lab_SAg_first[-which(duplicated(lab_SAg_first$EMPI)),] 

sum(lab_SAg_first$EMPI %in% lab_SAg$EMPI) #913
# Keep only HBV SAb tests
lab_SAb <- labs[labs$Lab_test == "HBV Surface Ab",]
# Keep only the first HBV SAb test date
lab_SAb$date <- paste(substr(lab_SAb$Lab_Date, 7,10), substr(lab_SAb$Lab_Date, 1,2), substr(lab_SAb$Lab_Date, 4,5), sep = "")
lab_SAb_first <- lab_SAb[order(lab_SAb$EMPI, lab_SAb$date, decreasing = FALSE),]
lab_SAb_first <- lab_SAb_first[-which(duplicated(lab_SAb_first$EMPI)),] 
sum(lab_SAb_first$EMPI %in% lab_SAb$EMPI) #680
# Keep only HBV CAb tests (note: HBV Core Ab(s)  = core Ab total; HBV Core Ab, IgM  = core Ab IgM)
lab_CAb <- labs[labs$Lab_test == "HBV Core Ab(s)",]
# Keep only the first HBV CAb test date
lab_CAb$date <- paste(substr(lab_CAb$Lab_Date, 7,10), substr(lab_CAb$Lab_Date, 1,2), substr(lab_CAb$Lab_Date, 4,5), sep = "")
lab_CAb_first <- lab_CAb[order(lab_CAb$EMPI, lab_CAb$date, decreasing = FALSE),]
lab_CAb_first <- lab_CAb_first[-which(duplicated(lab_CAb_first$EMPI)),] 
sum(lab_CAb_first$EMPI %in% lab_CAb$EMPI) #563
# Keep only HBV CAb IgM test (note: HBV Core Ab(s)  = core Ab total; HBV Core Ab, IgM  = core Ab IgM)
lab_CAbigm <- labs[labs$Lab_test == "HBV Core Ab, IgM",]
# Keep only the first HBV CAb test date
lab_CAbigm$date <- paste(substr(lab_CAbigm$Lab_Date, 7,10), substr(lab_CAbigm$Lab_Date, 1,2), substr(lab_CAbigm$Lab_Date, 4,5), sep = "")
lab_CAbigm_first <- lab_CAbigm[order(lab_CAbigm$EMPI, lab_CAbigm$date, decreasing = FALSE),]
lab_CAbigm_first <- lab_CAbigm_first[-which(duplicated(lab_CAbigm_first$EMPI)),] 
sum(lab_CAbigm_first$EMPI %in% lab_CAbigm$EMPI) #304
# Keep only HBV e-Ag HBV e-Ab tests
lab_HBVeag <- labs[labs$Lab_test == "HBV e Ag",]
lab_HBVeag$date <- paste(substr(lab_HBVeag$Lab_Date, 7,10), substr(lab_HBVeag$Lab_Date, 1,2), substr(lab_HBVeag$Lab_Date, 4,5), sep = "")
lab_HBVeag_first <- lab_HBVeag[order(lab_HBVeag$EMPI, lab_HBVeag$date, decreasing = FALSE),]
lab_HBVeag_first <- lab_HBVeag_first[-which(duplicated(lab_HBVeag_first$EMPI)),]
# Keep only HBV e-Ab
lab_HBVeab <- labs[labs$Lab_test == "HBV e Ab",]
lab_HBVeab$date <- paste(substr(lab_HBVeab$Lab_Date, 7,10), substr(lab_HBVeab$Lab_Date, 1,2), substr(lab_HBVeab$Lab_Date, 4,5), sep = "")
lab_HBVeab_first <- lab_HBVeab[order(lab_HBVeab$EMPI, lab_HBVeab$date, decreasing = FALSE),]
lab_HBVeab_first <- lab_HBVeab_first[-which(duplicated(lab_HBVeab_first$EMPI)),]
# Keep only HBV DNA
lab_HBVdna <- labs[labs$Lab_test == "HBV DNA (IU/mL)",]
lab_HBVdna$date <- paste(substr(lab_HBVdna$Lab_Date, 7,10), substr(lab_HBVdna$Lab_Date, 1,2), substr(lab_HBVdna$Lab_Date, 4,5), sep = "")
lab_HBVdna_first <- lab_HBVdna[order(lab_HBVdna$EMPI, lab_HBVdna$date, decreasing = FALSE),]
lab_HBVdna_first <- lab_HBVdna_first[-which(duplicated(lab_HBVdna_first$EMPI)),]
##
##
##
# 4. Date Comparisons
# Compare the first date of the different parameters above:
# Drug_a, Outpt Encounter, HBV SAg, etc.
a_included$DOB <- dem_in$Date_of_Birth[match(a_included$EMPI, dem_in$EMPI)]
a_included$First_Biologic_date <- biologics_first_a$Med_Date[match(a_included$EMPI, biologics_first_a$EMPI)]
sum(is.na(a_included$First_Biologic_date)) # 205 drug_a pts did not receive a biologic
a_included$First_dmard_date <- dmard_first_a$Med_Date[match(a_included$EMPI, dmard_first_a$EMPI)]
sum(is.na(a_included$First_dmard_date)) # 180 drug_a pts did not receive a dmard
a_included$First_HBVSAG_date <- lab_SAg_first$Lab_Date[match(a_included$EMPI, lab_SAg_first$EMPI)]
sum(is.na(a_included$First_HBVSAG_date)) # 157 pts receiving drug_a did not have a HBV SAG test
a_included$First_HBVSAB_date <- lab_SAb_first$Lab_Date[match(a_included$EMPI, lab_SAb_first$EMPI)]
sum(is.na(a_included$First_HBVSAB_date)) # 298 pts receiving drug_a did not have a HBV SAb test
a_included$First_HBVCABG_date <- lab_CAb_first$Lab_Date[match(a_included$EMPI, lab_CAb_first$EMPI)]
sum(is.na(a_included$First_HBVCABG_date)) # 341 pts receiving drug_a did not have a HBV Cab IGG test
a_included$First_HBeAg_date <- lab_HBVeag_first$Lab_Date[match(a_included$EMPI, lab_HBVeag_first$EMPI)]
sum(is.na(a_included$First_HBeAg_date)) #660 pts did not have HBVeAg test
a_included$First_HBeAb_date <- lab_HBVeab_first$Lab_Date[match(a_included$EMPI, lab_HBVeab_first$EMPI)]
sum(is.na(a_included$First_HBeAb_date)) #659 pts did not have HBVeAg test
a_included$First_HBVdna_date <- lab_HBVdna_first$Lab_Date[match(a_included$EMPI, lab_HBVdna_first$EMPI)]
sum(is.na(a_included$First_HBVdna_date)) #655 pts did not have HBVeAg test
a_included$First_HBVCABM_date <- lab_CAbigm_first$Lab_Date[match(a_included$EMPI, lab_CAbigm_first$EMPI)]
sum(is.na(a_included$First_HBVCABM_date)) # 496 pts receiving drug_a did not have a HBV CAb IgM test
# Determine number of Drug_A patients who received a biologic before receiving DrugA
a_included$date_diff_bio <- mdy(a_included$Med_date) >= mdy(a_included$First_Biologic_date)
sum(is.na(a_included$date_diff_bio)) # 205
sum(a_included$date_diff_bio, na.rm = T) # 438 with >= 0
a_included$date_diff_bio[is.na(a_included$date_diff_bio)] <- FALSE # change all the NAs to False
# Determine number of Drug_a patients who received a dmard before receiving Drug_a
a_included$date_diff_dmard <- mdy(a_included$Med_date) >= mdy(a_included$First_dmard_date)
sum(is.na(a_included$date_diff_dmard)) # 180
sum(a_included$date_diff_dmard, na.rm = T) # 468 with >= 0
a_included$date_diff_dmard[is.na(a_included$date_diff_dmard)] <- FALSE # change all the NAs to False
# Determine number of drug_a patients who underwent HBV SAg screening prior to (up to 30d after) receiving drug_a:
# Note that 278 tofa patients did not undergo HBV SAG screening during this time period
a_included$date_diff_sag <- mdy(a_included$Med_date) + 30 >= mdy_hm(a_included$First_HBVSAG_date) 
sum(is.na(a_included$date_diff_sag)) # 157
sum(a_included$date_diff_sag, na.rm = T) # 468
a_included$date_diff_sag[is.na(a_included$date_diff_sag)] <- FALSE # change all the NAs to False
# Determine number of drug_a patients who underwent HBV SAb screening prior to (up to 30d after) receiving drug_a:
a_included$date_diff_sab <- mdy(a_included$Med_date) + 30 >= mdy_hm(a_included$First_HBVSAB_date) 
sum(is.na(a_included$date_diff_sab)) # 298
sum(a_included$date_diff_sab, na.rm = T) # 328
a_included$date_diff_sab[is.na(a_included$date_diff_sab)] <- FALSE # change all the NAs to False
# Determine number of drug_a patients who underwent HBV CAb(all) screening prior to (up to 30d after) receiving drug_a:
a_included$date_diff_cabG <- mdy(a_included$Med_date) + 30 >= mdy_hm(a_included$First_HBVCABG_date) 
sum(is.na(a_included$date_diff_cabG)) # 341
sum(a_included$date_diff_cabG, na.rm = T) # 264
a_included$date_diff_cabG[is.na(a_included$date_diff_cabG)] <- FALSE # change all the NAs to False
# Determine number of drug_a patients who underwent HBV CAb(IgM) screening prior to (up to 30d after) receiving drug_A:
a_included$date_diff_cabM <- mdy(a_included$Med_date) + 30 >= mdy_hm(a_included$First_HBVCABM_date) 
sum(is.na(a_included$date_diff_cabM)) # 496
sum(a_included$date_diff_cabM, na.rm = T) # 145
a_included$date_diff_cabM[is.na(a_included$date_diff_cabM)] <- FALSE # change all the NAs to False
sag_rheum <- a_included$EMPI[a_included$date_diff_sag == TRUE]
sab_rheum <- a_included$EMPI[a_included$date_diff_sab == TRUE]
cabTotal_rheum <- a_included$EMPI[a_included$date_diff_cabG == TRUE]
cabM_rheum <- a_included$EMPI[a_included$date_diff_cabM == TRUE]
no_hbv_test_rheum <- setdiff(a_included$EMPI, 
                             union(union(a_included$EMPI[a_included$date_diff_sag == TRUE],
                                         a_included$EMPI[a_included$date_diff_sab == TRUE]),
                                   a_included$EMPI[a_included$date_diff_cabG == TRUE]))
hbv_dna_rheum <- intersect(a_included$EMPI, lab_HBVdna$EMPI)
hbv_eAg_rheum <- intersect(a_included$EMPI, lab_HBVeag$EMPI)
length(sag_rheum) #468
length(sab_rheum) #328
length(cabTotal_rheum) #264
length(cabM_rheum) #145
length(no_hbv_test_rheum) #177
length(intersect(cabTotal_rheum, intersect(sab_rheum,sag_rheum))) #194
length(intersect(sab_rheum,sag_rheum)) #302
length(intersect(cabTotal_rheum, sab_rheum)) #209
length(intersect(cabTotal_rheum, sag_rheum)) #242
length(intersect(cabTotal_rheum, cabM_rheum)) #64
length(setdiff(intersect(sab_rheum,sag_rheum),cabTotal_rheum)) #108
length(setdiff(intersect(cabTotal_rheum,sag_rheum),sab_rheum)) #48
length(setdiff(intersect(cabTotal_rheum, sab_rheum),sag_rheum)) #15
setdiff(sag_rheum, union(cabTotal_rheum, sab_rheum)) %>%
  length() #118
setdiff(sab_rheum, union(cabTotal_rheum, sag_rheum)) %>%
  length() #11
setdiff(cabTotal_rheum, union(sag_rheum, sab_rheum)) %>%
  length() #7
setdiff(intersect(sag_rheum, cabM_rheum),union(cabTotal_rheum, sab_rheum)) %>%
  length() #40
setdiff(intersect(sab_rheum,intersect(sag_rheum, cabM_rheum)),cabTotal_rheum) %>%
  length() #38
setdiff(cabM_rheum,cabTotal_rheum) %>%
  length() #81
length(setdiff(sag_rheum, intersect(sab_rheum,sag_rheum))) # 166
# Calculate inappropriate screening
intersect(union(hbv_dna_rheum, cabM_rheum),a_included$EMPI) %>%
  length() #158
hbv_positive <- union(union(lab_CAb_first$EMPI[lab_CAb_first$Result=="Positive"],
                            lab_CAb_first$EMPI[lab_CAb_first$Result=="REACTIVE"]),
                      lab_CAb_first$EMPI[lab_CAb_first$Result=="POSITIVE"]) %>%
  union(lab_CAb_first$EMPI[lab_CAb_first$Result=="POS"]) %>%
  union(lab_SAg_first$EMPI[lab_SAg_first$Result=="POSITIVE"])
hbv_positive_a <- intersect(hbv_positive, a_included$EMPI)
length(hbv_positive_a)
length(unique(hbv_positive_a))
inappropriate_hbv <- setdiff(union(union(hbv_eAg_rheum, hbv_dna_rheum),cabM_rheum),
                             hbv_positive_a)
inappropriate_hbv <- intersect(inappropriate_hbv, a_included$EMPI)
length(inappropriate_hbv)
# Determine the inappropriate_HBV subsets of complete, partial, and no hbv screening
intersect(inappropriate_hbv,
          intersect(intersect(sag_rheum, sab_rheum),cabTotal_rheum)) %>%
  length() #59 / complete
intersect(inappropriate_hbv,
          setdiff(a_included$EMPI,
          union(no_hbv_test_rheum,
                  intersect(intersect(sag_rheum, sab_rheum),cabTotal_rheum)))) %>%
  length() #91 / partial
intersect(inappropriate_hbv, no_hbv_test_rheum) %>%
  length() #2 / no screening
# Find out how many patients had neither DMARD nor Biologics prior to drug_a ("new starts"):
new_start_empi <- 
  intersect(a_included$EMPI[a_included$date_diff_dmard == FALSE], 
            a_included$EMPI[a_included$date_diff_bio == FALSE])
length(new_start_empi) # 156 patients with neither DMARD nor Biologic prior to drug_a
# Fill out the rest of the Venn Diagram for new starts
length(intersect(new_start_empi, sab_rheum)) #77
length(intersect(new_start_empi, cabTotal_rheum)) #69
length(intersect(new_start_empi, cabM_rheum)) #32
length(intersect(new_start_empi, no_hbv_test_rheum)) #57
length(intersect(new_start_empi, intersect(cabTotal_rheum, intersect(sab_rheum,sag_rheum)))) #51
length(intersect(new_start_empi, intersect(sab_rheum,sag_rheum))) #71
length(intersect(new_start_empi, intersect(cabTotal_rheum, sab_rheum))) #54
length(intersect(new_start_empi, intersect(cabTotal_rheum, sag_rheum))) #63
length(intersect(new_start_empi, intersect(cabTotal_rheum, cabM_rheum))) #21
length(intersect(new_start_empi, setdiff(intersect(sab_rheum,sag_rheum),cabTotal_rheum))) #20
length(intersect(new_start_empi, setdiff(intersect(cabTotal_rheum,sag_rheum),sab_rheum))) #12
length(intersect(new_start_empi, setdiff(intersect(cabTotal_rheum, sab_rheum),sag_rheum))) #3
setdiff(sag_rheum, union(cabTotal_rheum, sab_rheum)) %>%
  intersect(new_start_empi) %>%
  length() #7
setdiff(sab_rheum, union(cabTotal_rheum, sag_rheum)) %>%
  intersect(new_start_empi) %>%
  length() #3
setdiff(cabTotal_rheum, union(sag_rheum, sab_rheum)) %>%
  intersect(new_start_empi) %>%
  length() # 3
setdiff(intersect(sag_rheum, cabM_rheum),union(cabTotal_rheum, sab_rheum)) %>%
  intersect(new_start_empi) %>%
  length() # 2
setdiff(intersect(sab_rheum,intersect(sag_rheum, cabM_rheum)),cabTotal_rheum) %>%
  intersect(new_start_empi) %>%
  length() #8
setdiff(cabM_rheum,cabTotal_rheum) %>%
  intersect(new_start_empi) %>%
  length() #11
a_included$EMPI[a_included$date_diff_dmard == TRUE] %>%
  length() #468 - received DMARD
a_included$EMPI[a_included$date_diff_bio == TRUE] %>%
  length() #438 - received biolgic
intersect(a_included$EMPI[a_included$date_diff_bio == TRUE],
          a_included$EMPI[a_included$date_diff_dmard == TRUE]) %>%
  length() # 384 both
intersect(a_included$EMPI[a_included$date_diff_bio == FALSE],
          a_included$EMPI[a_included$date_diff_dmard == FALSE]) %>%
  length() #156
# Same analysis for non-new start (biologic, DMARD, or both)
non_new_start_empi <- 
  unique(union(a_included$EMPI[a_included$date_diff_dmard == TRUE], 
            a_included$EMPI[a_included$date_diff_bio == TRUE]))
length(intersect(non_new_start_empi, intersect(cabTotal_rheum, intersect(sab_rheum,sag_rheum)))) #143
length(intersect(non_new_start_empi, setdiff(intersect(sab_rheum,sag_rheum),cabTotal_rheum))) #88
length(intersect(non_new_start_empi, setdiff(intersect(cabTotal_rheum,sag_rheum),sab_rheum))) #36
length(intersect(non_new_start_empi, setdiff(intersect(cabTotal_rheum, sab_rheum),sag_rheum))) #12
length(intersect(non_new_start_empi, no_hbv_test_rheum)) #120
setdiff(sag_rheum, union(cabTotal_rheum, sab_rheum)) %>%
  intersect(non_new_start_empi) %>%
  length() #111
setdiff(sab_rheum, union(cabTotal_rheum, sag_rheum)) %>%
  intersect(non_new_start_empi) %>%
  length() #8
setdiff(cabTotal_rheum, union(sag_rheum, sab_rheum)) %>%
  intersect(non_new_start_empi) %>%
  length() # 4
a_included$EMPI[a_included$date_diff_dmard == TRUE & a_included$date_diff_bio == FALSE] %>%
  unique() %>%
  length()
a_included$EMPI[a_included$date_diff_dmard == FALSE & a_included$date_diff_bio == TRUE] %>%
  unique() %>%
  length()
a_included$EMPI[a_included$date_diff_dmard == TRUE & a_included$date_diff_bio == TRUE] %>%
  unique() %>%
  length()
a_included$EMPI[a_included$date_diff_dmard == FALSE & a_included$date_diff_bio == FALSE] %>%
  unique() %>%
  length()
####
####
#### Repeat for Drug_B
b_included$DOB <- dem_in$Date_of_Birth[match(b_included$EMPI, dem_in$EMPI)]
b_included$First_Biologic_date <- biologics_first_b$Med_Date[match(b_included$EMPI, biologics_first_b$EMPI)]
sum(is.na(b_included$First_Biologic_date)) # 47 drug_b pts did not receive a biologic
b_included$First_dmard_date <- dmard_first_a$Med_Date[match(b_included$EMPI, dmard_first_b$EMPI)]
sum(is.na(b_included$First_dmard_date)) # 29 drug_a pts did not receive a dmard
b_included$First_HBVSAG_date <- lab_SAg_first$Lab_Date[match(b_included$EMPI, lab_SAg_first$EMPI)]
sum(is.na(b_included$First_HBVSAG_date)) # 99 pts receiving drug_b did not have a HBV SAG test
b_included$First_HBVSAB_date <- lab_SAb_first$Lab_Date[match(b_included$EMPI, lab_SAb_first$EMPI)]
sum(is.na(b_included$First_HBVSAB_date)) # 192 pts receiving drug_b did not have a HBV SAb test
b_included$First_HBVCABG_date <- lab_CAb_first$Lab_Date[match(b_included$EMPI, lab_CAb_first$EMPI)]
sum(is.na(b_included$First_HBVCABG_date)) # 219 pts receiving b did not have a HBV Cab IGG test
b_included$First_HBeAg_date <- lab_HBVeag_first$Lab_Date[match(b_included$EMPI, lab_HBVeag_first$EMPI)]
sum(is.na(b_included$First_HBeAg_date)) #375 pts did not have HBVeAg test
b_included$First_HBeAb_date <- lab_HBVeab_first$Lab_Date[match(b_included$EMPI, lab_HBVeab_first$EMPI)]
sum(is.na(b_included$First_HBeAb_date)) #376 pts did not have HBVeAb test
b_included$First_HBVdna_date <- lab_HBVdna_first$Lab_Date[match(b_included$EMPI, lab_HBVdna_first$EMPI)]
sum(is.na(b_included$First_HBVdna_date)) #382 pts did not have HBVeAg test
b_included$First_HBVCABM_date <- lab_CAbigm_first$Lab_Date[match(b_included$EMPI, lab_CAbigm_first$EMPI)]
sum(is.na(b_included$First_HBVCABM_date)) # 293 pts receiving B did not have a HBV CAb IgM test
# Determine number of Drug_b patients who received a biologic before receiving B
b_included$date_diff_bio <- mdy(b_included$Med_date) >= mdy(b_included$First_Biologic_date)
sum(is.na(b_included$date_diff_bio)) # 47
sum(b_included$date_diff_bio, na.rm = T) #  with > 0; 331 with >= 0
b_included$date_diff_bio[is.na(b_included$date_diff_bio)] <- FALSE # change all the NAs to False
# Determine number of drug_B patients who received a dmard before receiving B
b_included$date_diff_dmard <- mdy(b_included$Med_date) >= mdy(b_included$First_dmard_date)
sum(is.na(b_included$date_diff_dmard)) # 29
sum(b_included$date_diff_dmard, na.rm = T) # 315 with >= 0;  with >
b_included$date_diff_dmard[is.na(b_included$date_diff_dmard)] <- FALSE # change all the NAs to False
# Determine number of B patients who underwent HBV SAg screening prior to (up to 30d after) receiving B:
b_included$date_diff_sag <- mdy(b_included$Med_date) + 30 >= mdy_hm(b_included$First_HBVSAG_date) 
sum(is.na(b_included$date_diff_sag)) # 99
sum(b_included$date_diff_sag, na.rm = T) # 269
b_included$date_diff_sag[is.na(b_included$date_diff_sag)] <- FALSE # change all the NAs to False
# Determine number of B patients who underwent HBV SAb screening prior to (up to 30d after) receiving B:
b_included$date_diff_sab <- mdy(b_included$Med_date) + 30 >= mdy_hm(b_included$First_HBVSAB_date) 
sum(is.na(b_included$date_diff_sab)) # 192
sum(b_included$date_diff_sab, na.rm = T) # 173
b_included$date_diff_sab[is.na(b_included$date_diff_sab)] <- FALSE # change all the NAs to False
# Determine number of Drug_B patients who underwent HBV CAb(all) screening prior to (up to 30d after) receiving Drug_B:
b_included$date_diff_cabG <- mdy(b_included$Med_date) + 30 >= mdy_hm(b_included$First_HBVCABG_date) 
sum(is.na(b_included$date_diff_cabG)) # 219
sum(b_included$date_diff_cabG, na.rm = T) # 141
b_included$date_diff_cabG[is.na(b_included$date_diff_cabG)] <- FALSE # change all the NAs to False
# Determine number of drug_a patients who underwent HBV CAb(IgM) screening prior to (up to 30d after) receiving drug_A:
b_included$date_diff_cabM <- mdy(b_included$Med_date) + 30 >= mdy_hm(b_included$First_HBVCABM_date) 
sum(is.na(b_included$date_diff_cabM)) # 293
sum(b_included$date_diff_cabM, na.rm = T) # 82
b_included$date_diff_cabM[is.na(b_included$date_diff_cabM)] <- FALSE # change all the NAs to False
# Find out how many patients who had an outpt rheum encounter were screened for HBV SAg?
sag_rheum_b <- b_included$EMPI[b_included$date_diff_sag == TRUE]
sab_rheum_b <- b_included$EMPI[b_included$date_diff_sab == TRUE]
cabTotal_rheum_b <- b_included$EMPI[b_included$date_diff_cabG == TRUE]
cabM_rheum_b <- b_included$EMPI[b_included$date_diff_cabM == TRUE]
no_hbv_test_rheum_b <- setdiff(b_included$EMPI, union(union(sag_rheum_b,sab_rheum_b),cabTotal_rheum_b))
hbv_dna_rheum_b <- intersect(b_included$EMPI, lab_HBVdna$EMPI)
hbv_eAg_rheum_b <- intersect(b_included$EMPI, lab_HBVeag$EMPI)
length(sag_rheum_b) #269
length(sab_rheum_b) #173
length(cabTotal_rheum_b) #141
length(cabM_rheum_b) #82
length(no_hbv_test_rheum_b) #102
length(intersect(cabTotal_rheum_b, intersect(sab_rheum_b,sag_rheum_b))) #94
length(intersect(sab_rheum_b,sag_rheum_b)) #164
length(intersect(cabTotal_rheum_b, sab_rheum_b)) #99
length(intersect(cabTotal_rheum_b, sag_rheum_b)) #125
length(intersect(cabTotal_rheum_b, cabM_rheum_b)) #33
length(setdiff(intersect(sab_rheum_b,sag_rheum_b),cabTotal_rheum_b)) #70
length(setdiff(intersect(cabTotal_rheum_b,sag_rheum_b),sab_rheum_b)) #31
length(setdiff(intersect(cabTotal_rheum_b, sab_rheum_b),sag_rheum_b)) #5
setdiff(sag_rheum_b, union(cabTotal_rheum_b, sab_rheum_b)) %>%
  length() #74
setdiff(sab_rheum_b, union(cabTotal_rheum_b, sag_rheum_b)) %>%
  length() #4
setdiff(cabTotal_rheum_b, union(sag_rheum_b, sab_rheum_b)) %>%
  length() #11
setdiff(intersect(sag_rheum_b, cabM_rheum_b),union(cabTotal_rheum_b, sab_rheum_b)) %>%
  length() #23
setdiff(intersect(sab_rheum_b,intersect(sag_rheum_b, cabM_rheum_b)),cabTotal_rheum_b) %>%
  length() #24
setdiff(cabM_rheum_b,cabTotal_rheum_b) %>%
  length() #49
length(setdiff(sag_rheum_b, intersect(sab_rheum_b,sag_rheum_b))) #105
# Calculate inappropriate screening
union(hbv_dna_rheum_b, cabM_rheum_b) %>%
  length() #88
hbv_positive_b <- intersect(hbv_positive, b_included$EMPI)
length(hbv_positive_b)
inappropriate_hbv_b <- setdiff(union(union(hbv_eAg_rheum_b, hbv_dna_rheum_b),cabM_rheum_b),
                             hbv_positive_b)
inappropriate_hbv_b <- intersect(inappropriate_hbv_b, b_included$EMPI)
length(inappropriate_hbv_b) #89
89/391
# Calculate the inappropriate_HBV subsets of complete, partial, and no hbv screening
intersect(inappropriate_hbv_b,
          intersect(intersect(sag_rheum_b, sab_rheum_b),cabTotal_rheum_b)) %>%
  length() #25 / complete
intersect(inappropriate_hbv_b,
          setdiff(b_included$EMPI,
                  union(no_hbv_test_rheum_b,
                        intersect(intersect(sag_rheum_b, sab_rheum_b),cabTotal_rheum_b)))) %>%
  length() #60 / partial
intersect(inappropriate_hbv_b, no_hbv_test_rheum_b) %>%
  length() #4 / no screening
# Find out how many patients had neither DMARD nor Biologics prior to drug_b ("new starts"):
new_start_empi_b <- 
  intersect(b_included$EMPI[b_included$date_diff_dmard == FALSE], 
            b_included$EMPI[b_included$date_diff_bio == FALSE])
length(new_start_empi_b) # 17 patients with neither DMARD nor Biologic prior to drug_a
# Note: I did not repeat new_start calculations for drug_b because n=17 now
intersect(b_included$EMPI[b_included$date_diff_bio == TRUE],
          b_included$EMPI[b_included$date_diff_dmard == TRUE]) %>%
  length() # 272 both
intersect(b_included$EMPI[b_included$date_diff_bio == FALSE],
          b_included$EMPI[b_included$date_diff_dmard == FALSE]) %>%
  length() #17
b_included$EMPI[b_included$date_diff_dmard == TRUE & b_included$date_diff_bio == FALSE] %>%
  unique() %>%
  length()
b_included$EMPI[b_included$date_diff_dmard == FALSE & b_included$date_diff_bio == TRUE] %>%
  unique() %>%
  length()
b_included$EMPI[b_included$date_diff_dmard == TRUE & b_included$date_diff_bio == TRUE] %>%
  unique() %>%
  length()
b_included$EMPI[b_included$date_diff_dmard == FALSE & b_included$date_diff_bio == FALSE] %>%
  unique() %>%
  length()
####
####
####
#### 6. DEMOGRAPHICS / TABLE 1
a_included$Age <- dem_in$Age[match(a_included$EMPI, dem_in$EMPI)]
b_included$Age <- dem_in$Age[match(b_included$EMPI, dem_in$EMPI)]
summary(a_included$Age)
summary(b_included$Age)
summary(dem_in$Age[match(a_included$EMPI,dem_in$EMPI)])
summary(dem_in$Age[match(b_included$EMPI,dem_in$EMPI)])
sd(dem_in$Age[match(b_included$EMPI,dem_in$EMPI)])
table(dem_in$Gender[match(a_included$EMPI,dem_in$EMPI)]) %>%
  table()
table(dem_in$Gender[match(b_included$EMPI,dem_in$EMPI)]) %>%
  table()
table(dem_in$Race[match(a_included$EMPI,dem_in$EMPI)]) %>%
        prop.table()
(7+2+16)/678 # % Hispanic
3+12+14+1+2 # % other
17/678
table(dem_in$Race[match(b_included$EMPI,dem_in$EMPI)]) %>%
  prop.table()
(19)/391 # % Hispanic
7/391 # % other
11/391
table(a_included$date_diff_bio) %>%
  prop.table()
table(a_included$date_diff_dmard) %>%
  prop.table()
384/678
length(new_start_empi)/678
table(b_included$date_diff_bio) %>%
  prop.table()
table(b_included$date_diff_dmard) %>%
  prop.table()
272/391
length(new_start_empi_b)/391
####
####
####
#### Combined A_B_ Date comparisons
# Extract Biologics from the medication file AB
sum(biol_ab) #24684
biologics_ab <- med_in[biol_ab,]
nrow(biologics_ab) #24684
biologics_ab <- select(biologics_ab, EMPI, MRN_Type, MRN, Med = Medication, Med_Date = Medication_Date, Med_Clinic = Clinic, Encounter_number)
length(unique(biologics_ab$EMPI)) #922 unique EMPIs
# Keep only the first biologics medication date
biologics_ab_first <- biologics_ab[order(biologics_ab$EMPI, mdy(biologics_ab$Med_Date), decreasing = FALSE),]
length(unique(biologics_ab_first$EMPI)) # 922 unique EMPIs
dim(biologics_ab_first) # 24684 x 7
biologics_ab_first <- biologics_ab_first[-which(duplicated(biologics_ab_first$EMPI)),]
dim(biologics_ab_first) # 922 x 7
# Make Date Compare dataframe for combined A_B drugs
ab_included$DOB <- dem_in$Date_of_Birth[match(ab_included$EMPI, dem_in$EMPI)]
ab_included$DOD <- dem_in$Date_Of_Death[match(ab_included$EMPI, dem_in$EMPI)]
ab_included$Gender <- dem_in$Gender[match(ab_included$EMPI, dem_in$EMPI)]
ab_included$Race <- dem_in$Race[match(ab_included$EMPI, dem_in$EMPI)]
ab_included$Asian <- ifelse(ab_included$Race == "Asian-ASIAN", TRUE, FALSE)
ab_included$Aborig <- ifelse(ab_included$Race == "American Indian-AMERICAN INDIAN OR ALASKA NATIVE", TRUE, FALSE)
ab_included$White <- ifelse(ab_included$Race == "White-WHITE", TRUE, FALSE)
ab_included$HiRiskEthnicity <- ifelse(ab_included$Asian == TRUE | ab_included$Aborig == TRUE, TRUE, FALSE)
ab_included$First_Biologic_date <- biologics_first_ab$Med_Date[match(ab_included$EMPI, biologics_first_ab$EMPI)]
sum(is.na(ab_included$First_Biologic_date)) # 256 ab pts did not receive a biologic
ab_included$First_dmard_date <- dmard_first_ab$Med_Date[match(ab_included$EMPI, dmard_first_ab$EMPI)]
sum(is.na(ab_included$First_dmard_date)) # 203 drug_a/b pts did not receive a dmard  
ab_included$First_HBVSAG_date <- lab_SAg_first$Lab_Date[match(ab_included$EMPI, lab_SAg_first$EMPI)]
sum(is.na(ab_included$First_HBVSAG_date)) # 233 pts receiving drug_a and drug_b did not have a HBV SAG test
ab_included$First_HBVSAB_date <- lab_SAb_first$Lab_Date[match(ab_included$EMPI, lab_SAb_first$EMPI)]
sum(is.na(ab_included$First_HBVSAB_date)) # 426 pts receiving drug_a did not have a HBV SAb test
ab_included$First_HBVCABG_date <- lab_CAb_first$Lab_Date[match(ab_included$EMPI, lab_CAb_first$EMPI)]
sum(is.na(ab_included$First_HBVCABG_date)) # 494 pts receiving drug_a did not have a HBV Cab IGG test
# Determine number of Drug_A/B patients who received a biologic before receiving DrugA/B
ab_included$Med_Date <- ab_first$Med_Date[match(ab_included$EMPI, ab_first$EMPI)]
ab_included$date_diff_bio <- mdy(ab_included$Med_Date) >= mdy(ab_included$First_Biologic_date)
sum(is.na(ab_included$date_diff_bio)) # 256
sum(ab_included$date_diff_bio, na.rm = T) #  with > 0; 636 with >= 0
ab_included$date_diff_bio[is.na(ab_included$date_diff_bio)] <- FALSE # change all the NAs to False
# Determine number of Drug_a.b patients who received a dmard before receiving Drug_a
ab_included$date_diff_dmard <- mdy(ab_included$Med_Date) >= mdy(ab_included$First_dmard_date)
sum(is.na(ab_included$date_diff_dmard)) # 203
sum(ab_included$date_diff_dmard, na.rm = T) # 694 with >= 0; *** with >
ab_included$date_diff_dmard[is.na(ab_included$date_diff_dmard)] <- FALSE # change all the NAs to False
# Determine number of drug_a.b patients who underwent HBV SAg screening prior to (up to 30d after) receiving drug_a:
ab_included$date_diff_sag <- mdy(ab_included$Med_Date) + 30 >= mdy_hm(ab_included$First_HBVSAG_date) 
sum(is.na(ab_included$date_diff_sag)) # 233
sum(ab_included$date_diff_sag, na.rm = T) # 637
ab_included$date_diff_sag[is.na(ab_included$date_diff_sag)] <- FALSE # change all the NAs to False
# Determine number of drug_a patients who underwent HBV SAb screening prior to (up to 30d after) receiving drug_a:
ab_included$date_diff_sab <- mdy(ab_included$Med_Date) + 30 >= mdy_hm(ab_included$First_HBVSAB_date) 
sum(is.na(ab_included$date_diff_sab)) # 426
sum(ab_included$date_diff_sab, na.rm = T) # 440
ab_included$date_diff_sab[is.na(ab_included$date_diff_sab)] <- FALSE # change all the NAs to False
# Determine number of drug_a patients who underwent HBV CAb(all) screening prior to (up to 30d after) receiving drug_a:
ab_included$date_diff_cabG <- mdy(ab_included$Med_Date) + 30 >= mdy_hm(ab_included$First_HBVCABG_date) 
sum(is.na(ab_included$date_diff_cabG)) # 494
sum(ab_included$date_diff_cabG, na.rm = T) # 351
ab_included$date_diff_cabG[is.na(ab_included$date_diff_cabG)] <- FALSE # change all the NAs to False
# Add columns for new start and Complete screen
ab_included$NewStart <- ifelse(ab_included$date_diff_bio == FALSE & ab_included$date_diff_dmard == FALSE, 
                                   TRUE, FALSE)
# Three options: New Start; DMARD only; Biologic
ab_included$PriorImmuno <- ifelse(ab_included$NewStart == TRUE, "None", 
                                      ifelse(ab_included$date_diff_bio == TRUE, "Biologic", "DMARD Only"))
ab_included$CompleteHBV <- ifelse(ab_included$date_diff_sag == TRUE & ab_included$date_diff_sab == TRUE &
                                        ab_included$date_diff_cabG == TRUE, TRUE, FALSE)
# Prescriber of first drug -- extract from med_in file or enc_in file?
ab_included$Prescriber <- med_in$Provider[match(ab_included$EMPI, med_in$EMPI)]
ab_included$Prescriberii <- enc_in$Attending_MD[match(ab_included$Encounter_number, enc_in$Encounter_number)]
ab_included$Hospital <- enc_in$Hospital[match(ab_included$Encounter_number, enc_in$Encounter_number)]
# Med: tofa or toci?
ab_included$Medication <- ab_first$Med[match(ab_included$EMPI, ab_first$EMPI)]
table(ab_included$Medication)  
ab_included$Medication[ab_included$Medication == "Actemra (tocilizumab) - LMR 7253" |
                         ab_included$Medication == "Actemra subcutaneous (tocilizumab subcutaneous) - LMR 9249" |
                         ab_included$Medication == "Tocilizumab  IVPB In 100 ml" |
                         ab_included$Medication == "Tocilizumab 200 mg/10 ml (20 mg/ml) Intravenous Solution" |
                         ab_included$Medication == "Tocilizumab 400 mg/20 ml (20 mg/ml) Intravenous Solution" |
                         ab_included$Medication == "Tocilizumab 80 mg/4 ml (20 mg/ml) Intravenous Solution" |
                         ab_included$Medication == "Tocilizumab IV" |
                         ab_included$Medication == "Tocilizumab 400 mg/20ml vial 20ml vial" |
                         ab_included$Medication == "Tocilizumab 200 mg/10ml vial 10ml vial" |
                         ab_included$Medication == "Tocilizumab 162 mg/0.9 ml Subcutaneous Syringe" |
                         ab_included$Medication == "Injection, tocilizumab, 1 mg" |
                         ab_included$Medication == "Id-Tocilizumab (2013p000399) Injection 162 mg/0.9 ml" |
                         ab_included$Medication == "Actemra 162 mg/0.9 ml Subcutaneous Syringe"] <- "Toci"
ab_included$Medication[ab_included$Medication == "Id-Tofacitinib (2015p000092) Tablet 5 mg" |
                         ab_included$Medication == "Tofacitinib 5 mg Tablet" |
                         ab_included$Medication == "Xeljanz 5 mg Tablet" |
                         ab_included$Medication == "Xeljanz (tofacitinib) - LMR 8725"] <- "Tofa"
#demo$Race[demo$Race == "White-W" | demo$Race == "White-WHITE" |
#            demo$Race == "Hispanic White-WHITE@HISPANIC"] <- "White"
# Hospital: BWH, MGH, Other (FH, NSM, NWH)
ab_included$Hospital2 = ab_included$Hospital
ab_included$Hospital2[ab_included$Hospital2 == "FH" | ab_included$Hospital2 == "NSM" |
                        ab_included$Hospital2 == "NWH"] <- "Other"
table(ab_included$Medication)
length(ab_included$EMPI)
# Regression - predictors of complete screen
# Outcome - complete screen
# Variables - Toci or Tofa; Gender; White/NonWhite; New Start/ DMARD only/ Biologic; Hospital
is.factor(ab_included$Medication)
typeof(ab_included$Medication)
ab_included$Medication <- factor(ab_included$Medication)
is.factor(ab_included$White)
typeof(ab_included$White)
is.factor(ab_included$Hospital2)
typeof(ab_included$Hospital2)
ab_included$Hospital2 <- factor(ab_included$Hospital2)
is.factor(ab_included$PriorImmuno)
typeof(ab_included$PriorImmuno)
ab_included$PriorImmuno <- factor(ab_included$PriorImmuno)
ab_included$PriorImmuno <- relevel(ab_included$PriorImmuno, ref = "None")
mylogit <- glm(CompleteHBV ~ Medication + Gender + White + PriorImmuno + Hospital2, 
               data = ab_included, family = "binomial")
summary(mylogit)
confint(mylogit)
exp(cbind(OR = coef(mylogit), confint(mylogit)))
with(mylogit, null.deviance - deviance)
with(mylogit, df.null - df.residual)
with(mylogit, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))
#Compare new starts with non-new starts in tocilizumab
table(a_included$NewStart, ab_date_compare$CompleteHBV) %>%
  chisq.test()
table(51, 105,
      194, 484)
sum(ab_date_compare$Gender == "Male") #320
sum(ab_date_compare$Gender == "Female") #1070
sum(ab_date_compare$CompleteHBV[ab_date_compare$Gender == "Male"]) #79
sum(ab_date_compare$CompleteHBV[ab_date_compare$Gender == "Female"]) #244
79/320 #24.7%
244/1070 #22.8%
table(ab_date_compare$Gender, ab_date_compare$CompleteHBV) %>%
  chisq.test()
sum(ab_date_compare$HiRiskEthnicity) #46
sum(!ab_date_compare$HiRiskEthnicity) #1344
sum(ab_date_compare$CompleteHBV[ab_date_compare$HiRiskEthnicity == TRUE]) #13
sum(ab_date_compare$CompleteHBV[ab_date_compare$HiRiskEthnicity == FALSE]) #310
13/46 #28.3%
310/1344 #23.1%
table(ab_date_compare$HiRiskEthnicity, ab_date_compare$CompleteHBV) %>%
  chisq.test()
sum(ab_date_compare$NewStart) #324
sum(!ab_date_compare$NewStart) #1066
sum(ab_date_compare$CompleteHBV[ab_date_compare$NewStart == TRUE]) #73
sum(ab_date_compare$CompleteHBV[ab_date_compare$NewStart == FALSE]) #250
73/324 #22.5%
250/1066 #23.1%
table(ab_date_compare$NewStart, ab_date_compare$CompleteHBV) %>%
  chisq.test()
intersect(outpt_enc_rheum_first_b$EMPI, ab_date_compare$EMPI[ab_date_compare$date_diff_bio == TRUE]) %>%
  length() #383
intersect(outpt_enc_rheum_first_b$EMPI, ab_date_compare$EMPI[ab_date_compare$date_diff_dmard == TRUE]) %>%
  length() #424
intersect(outpt_enc_rheum_first_b$EMPI, ab_date_compare$EMPI[ab_date_compare$NewStart == TRUE]) %>%
  length() #36
intersect(outpt_enc_rheum_first_b$EMPI, ab_date_compare$EMPI[ab_date_compare$date_diff_dmard == TRUE & 
                                                               ab_date_compare$date_diff_bio == TRUE]) %>%
  length() #348
