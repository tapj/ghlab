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
  mutate(Substrate = gsub("B-glycan","beta-Glucan",Substrate),
         Day = Day %>% as.character %>% as.numeric ) -> DO_substrate


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



# Cinetique AGX

Cinetique_AGX1 <- read_excel("data-raw/Cinetique en continu AGX bis.xlsx",
                            sheet = "AGX 1")

Cinetique_AGX2 <- read_excel("data-raw/Cinetique en continu AGX bis.xlsx",
                             sheet = "AGX 2")

Cinetique_AGX3 <- read_excel("data-raw/Cinetique en continu AGX bis.xlsx",
                             sheet = "AGX 3")


colnames(Cinetique_AGX1)[-1] = paste0(colnames(Cinetique_AGX1)[-1],sort(rep(c("-R1","-R2","-R3"),9)))

colnames(Cinetique_AGX2)[-1] = paste0(colnames(Cinetique_AGX2)[-1],sort(rep(c("-R1","-R2","-R3"),9)))

colnames(Cinetique_AGX3)[-1] = paste0(colnames(Cinetique_AGX3)[-1],sort(rep(c("-R1","-R2","-R3"),9)))


rbind(
  Cinetique_AGX1 %>% melt(id.vars="600") %>% na.omit %>% as_tibble(),
  Cinetique_AGX2 %>% melt(id.vars="600") %>% na.omit %>% as_tibble(),
  Cinetique_AGX3 %>% melt(id.vars="600") %>% na.omit %>% as_tibble()
) %>%  separate(variable, c("Run","Step", "Replicate"), sep="-") %>%
  rename(Time = `600`, DO = value) -> AGX_kinetics


devtools::use_data(AGX_kinetics)



# polysacharides assays

Code <- read_excel("data-raw/Code souches DR INRA ABG 12092017.xlsx")

Code %<>% rename(Code_1 = `code Audrey`, Strain_id = `Code publication`, Code = `Souche `)

substrate =
  c(
  c("Cellulose","Xylane","Pectin",
    "ArabinoG","Galactomannane","Xyloglucan",
    "Laminarin","Lichenan","b-glucan",
    "Amylopectin") %>% paste(.,c("S1")),
  c("Cellulose","Xylane","Pectin",
    "ArabinoG","Galactomannane","Xyloglucan",
    "Laminarin","Lichenan","b-glucan",
    "Amylopectin") %>% paste(.,c("S2"))
  ) %>% sort



Strain_Substrate = NULL

for(i in substrate){


dd <- read_excel("data-raw/Polysaccharides 16-20 juin 2014.xls",
                           sheet = i, skip = 1)[2:10,]

colnames(dd) =  dd %>% colnames %>% gsub("^$","Empty",.) %>% ifelse(is.na(.),"Empty", .) %>% paste(.,1:length(colnames(dd)), sep="__")

colnames(dd)[1] = "Time"

dd %<>%
  melt(id.vars="Time") %>%
  rename(Well=variable,
         DO=value) %>%
  separate(Well, c("Strain","id"), sep="__", remove = FALSE) %>%
  select(-id) %>%
  mutate(S = i,
         Time = Time %>% as.numeric,
         DO = DO %>% as.numeric) %>%
  separate(S, c("Substrate","Plate"), sep=" ") %>%
  as_tibble()

Strain_Substrate = rbind(Strain_Substrate,dd)

}

devtools::use_data(Strain_Substrate, overwrite = TRUE)


