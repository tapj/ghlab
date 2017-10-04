library(readxl)
library(tidyverse)
library(reshape2)



AGX_SCFA <- read_excel("data-raw/AGX AGCC juin2014.xlsx",
                       skip = 1)[1:4]



AGX_SCFA %<>%
  filter(Injection==1) %>%
  bind_cols(AGX_SCFA %>%
              filter(Injection==2) %>%
              select(-Echantillon)
            ) %>%
  select(-Injection, -Injection1) %>%
  melt(id.vars=c("Echantillon")) %>%
  mutate(variable=gsub("1","",variable)) %>%
  separate(col=Echantillon, into=c("Subtract","Run","Step"), sep=" " ) %>%
  rename(SCFA = variable)

devtools::use_data(AGX_SCFA)

# Croissance

DO_croissance_CXP <- read_excel("data-raw/Bilan GH_Lab 2017.xlsx",
                                sheet = "DO croissance CXP")

DO_croissance_LLG <- read_excel("data-raw/Bilan GH_Lab 2017.xlsx",
                                sheet = "DO croissance LLG")

DO_croissance_AGX <- read_excel("data-raw/Bilan GH_Lab 2017.xlsx",
                                sheet = "DO croissance AGX bis")
colnames(DO_croissance_AGX)[1:2] = c("Substrat","0")



rbind(
DO_croissance_CXP %>%
  melt(id.vars=c("Substrat", "Mix")) %>% filter(Mix=="M2") %>% select(-Mix),

DO_croissance_LLG %>%
  rename(`0`= D0) %>%
  melt(id.vars=c("Substrat", "Mix")) %>% filter(Mix=="M2") %>%
  select(-Mix),

DO_croissance_AGX %>% melt

) %>% rename(Substrate = Substrat, Day = variable, DO = value) -> DO_substrate


devtools::use_data(DO_substrate)

# Protein

Dosage_Proteine_CXP <- read_excel("data-raw/Bilan GH_Lab 2017.xlsx",
                                  sheet = "Dosage Proteine CXP")

Dosage_proteine_LLG <- read_excel("data-raw/Bilan GH_Lab 2017.xlsx",
                                  sheet = "Dosage proteine LLG")

Dosage_proteine_ARS <- read_excel("data-raw/Bilan GH_Lab 2017.xlsx",
                                  sheet = "Dosage proteine A.RS")

Dosage_proteine_AGX <- read_excel("data-raw/Bilan GH_Lab 2017.xlsx",
                                  sheet = "Dosage proteine AGX bis", skip = 1)

#Lactate Acetate

# Production_lactate_AGX <- read_excel("data-raw/Bilan GH_Lab 2017.xlsx",
#                                      sheet = "Production lactate AGX bix",
#                                      skip = 1)
#
# Production_acetate_AGX <- read_excel("data-raw/Bilan GH_Lab 2017.xlsx",
#                                      sheet = "Production acetate AGX bis",
#                                      skip = 1)[1:10]

# cinetique

cinetique_AGX <- read_excel("data-raw/Bilan GH_Lab 2017.xlsx",
                            sheet = "cinetique continu AGX bis")



