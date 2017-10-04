library(readxl)
library(tidyverse)
library(reshape2)



AGX_SCFA <- read_excel("C:/Users/tapju/OneDrive - Danone/Danone/GHLAB/Analysis/ghlab/data-raw/AGX AGCC juin2014.xlsx",
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



