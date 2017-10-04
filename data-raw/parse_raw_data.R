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

) %>% rename(Substrate = Substrat, Day = variable, DO = value) %>%
  mutate(Substrate = gsub("B-glycan","beta-Glucan",Substrate)) -> DO_substrate


devtools::use_data(DO_substrate, overwrite = TRUE)

# Protein

Dosage_Proteine_CXP <- read_excel("data-raw/Bilan GH_Lab 2017.xlsx",
                                  sheet = "Dosage Proteine CXP")

Dosage_proteine_LLG <- read_excel("data-raw/Bilan GH_Lab 2017.xlsx",
                                  sheet = "Dosage proteine LLG")

Dosage_proteine_ARS <- read_excel("data-raw/Bilan GH_Lab 2017.xlsx",
                                  sheet = "Dosage proteine A.RS")

Dosage_proteine_AGX <- read_excel("data-raw/Bilan GH_Lab 2017.xlsx",
                                  sheet = "Dosage proteine AGX bis", skip = 1)



colnames(Dosage_Proteine_CXP)[1:2] = c("assays",0)

colnames(Dosage_proteine_LLG)[1] = colnames(Dosage_proteine_ARS)[1] = colnames(Dosage_proteine_AGX)[1] = c("assays")

Dosage_proteine_LLG$`0` = Dosage_proteine_ARS$`0` = Dosage_proteine_AGX$`0` = 0

colnames(Dosage_proteine_AGX)[12] = 38

rbind(

Dosage_Proteine_CXP %>%
  filter(assays=="µg/µl") %>%
  mutate(Substrate = c("Pectine M1","Pectine M2", "Xylan M1", "Xylan M2", "Cellulose M1", "Cellulose M2") ) %>%
  melt(id.vars=c("Substrate","assays")) %>%
  select(-assays) %>%
  separate(Substrate, c("Substrate","Mix"), sep=" ") %>%
  filter(Mix == "M2") %>%
  select(-Mix) %>%
  mutate(value=ifelse(variable == 0 , 0,value)) %>%
  mutate(R=1),


Dosage_proteine_LLG %>%
  filter(assays=="Moyenne") %>%
  mutate(Substrate = c("Laminarin M1","Laminarin M2", "Lichenan M1", "Lichenan M2", "beta-Glucan M1", "beta-Glucan M2", "LGG M1", "LGG M2") ) %>%
  melt(id.vars=c("Substrate","assays")) %>%
  select(-assays) %>%
  separate(Substrate, c("Substrate","Mix"), sep=" ") %>%
  filter(Mix == "M2") %>%
  select(-Mix) %>%
  mutate(value=ifelse(variable == 0 , 0,value)) %>%
  mutate(R=1),


Dosage_proteine_ARS %>%
  filter(assays %in% c("Moyenne","moyenne")) %>%
  mutate(Substrate = c("Amylopectin M1","Amylopectin M2", "R_Starch M1", "R_Starch M2", "ARS M1", "ARS M2") ) %>%
  melt(id.vars=c("Substrate","assays")) %>%
  select(-assays) %>%
  separate(Substrate, c("Substrate","Mix"), sep=" ") %>%
  filter(Mix == "M2") %>%
  select(-Mix) %>%
  mutate(value=ifelse(variable == 0 , 0,value)) %>%
  mutate(R=1),

Dosage_proteine_AGX %>% slice(seq(3,36,3)) %>% filter(!is.na(`3`)) %>%
  mutate(Substrate = sort(paste(paste0("AGX-",1:3),sort(rep(1:3,3))))) %>%
  select(-`Volume µl`) %>%
  melt(id.vars=c("Substrate","assays")) %>%
  select(-assays) %>%
  separate(Substrate, c("Substrate","R"), sep=" ") %>%
  filter(variable != 38)

) %>% mutate(variable = variable %>% as.character %>% as.numeric()) %>%
  rename(Days = variable, `Protein (µg/µL)` = value)   -> Protein_assays

devtools::use_data(Protein_assays)


ggplot(Protein_assays,aes(y=`Protein (µg/µL)`,x=Days)) +
  geom_point() +
  facet_wrap(~Substrate,scales = "free") +
  geom_smooth() #+
  #ylim(0,NA)




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




cinetique_AGX %<>%
  slice(1:210) #%>%
  #tidyr::separate(`Time (min)`, c("d","time"), sep=" ")

#cinetique_AGX$time = lubridate::hms(cinetique_AGX$ti,"%h:%m:%s",origin="1970-01-01")[1:210]

#cinetique_AGX$time %<>% lubridate::parse_date_time("HMS")

#cinetique_AGX$time %<>% lubridate::hms("%h:%m:%s")[1:210]

#cinetique_AGX = cinetique_AGX[, -c(1)]

cinetique_AGX %<>%
  melt(id.vars="Time (min)") %>%
  as_tibble() %>%
  tidyr::separate(variable, c("Run","Step"), sep="-") %>%
  mutate(`Time (min)` = `Time (min)` - seconds(7))


ggplot(cinetique_AGX %>% group_by(Run,Step) %>% mutate(value=sort(value)), aes(x=`Time (min)` ,y=value)) + geom_point() + facet_grid(Run~Step)

#devtools::use_data(cinetique_AGX)









