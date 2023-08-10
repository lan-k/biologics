##find PBS code for psoriasis medicines
rm(list=ls())
library(dplyr)
library(stringr)
library(tidyr)


antidep_atc <- "N06A" #ATC code for antidepressants

med_list <- c("ADALIMUMAB","ETANERCEPT","GUSELKUMAB","INFLIXIMAB",
              "IXEKIZUMAB","RISANKIZUMAB","SECUKINUMAB","TILDRAKIZUMAB","USTEKINUMAB")

pbs_files <- read.csv(file="../PBSATC_ITMCDE_BioRq_Mar22.csv", stringsAsFactors = F)


psoriasis_meds <- pbs_files %>% 
  filter(DRUG_NAME %in% med_list) %>% 
  filter(grepl("psoria", restriction_description, ignore.case = T)) %>%
  select(PBS_ATC, DRUG_NAME, PBS_ITM_CDE, restriction_description) %>% 
  unique() %>%
  arrange(DRUG_NAME) %>%
  mutate(restriction_description = trimws(restriction_description),
         phase = str_extract(restriction_description, ("(?<=Phase: )\\b\\w+\\b\\s\\b\\w+\\b")),
         cpp=as.integer(grepl("chronic plaque psoriasis", restriction_description, ignore.case = T)),
         pa = as.integer(grepl("arthrit", restriction_description, ignore.case = T))) #psoriatic arthritis



with(psoriasis_meds, table(cpp, pa))
table(psoriasis_meds$phase)

cpp_codes <- psoriasis_meds %>% 
  filter(cpp==1) %>%
  pull(PBS_ITM_CDE) %>% unique()


pa_codes <- psoriasis_meds %>% 
  filter(pa==1) %>%
  pull(PBS_ITM_CDE) %>% unique()

antidep_meds <- pbs_files %>% 
  filter(grepl(antidep_atc, PBS_ATC, ignore.case = T)) %>%
  select(PBS_ATC, DRUG_NAME, PBS_ITM_CDE) %>% 
  unique() %>%
  arrange(DRUG_NAME)


