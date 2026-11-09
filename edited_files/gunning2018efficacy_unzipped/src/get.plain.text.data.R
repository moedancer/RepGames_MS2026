library(tidyverse)

## Baseline AA/HSE in S-2013
.dat.hypoth.circuit2 %>%
  filter(Experiment == "S-2013" & response_name == "AA/HSE" & circuit2 == "1_PRE")

## Baseline AA/HSE in L-2014
.dat.hypoth.circuit2 %>%
  filter(Experiment == "L-2014" & response_name == "AA/HSE" & circuit2 == "1_BASELINE")

## Baseline AHI in S-2013
.infest.df %>%
  filter(Experiment == "L-2014" & response_name == "PrIH (AHI)" & circuit2 == "1_BASELINE")

## Baseline vs Intervention AA/HSE in S-2013
.dat.hypoth.circuit2 %>%
  filter(Experiment == "S-2013" & response_name == "AA/HSE" & (circuit2 == "2_FUM" | circuit2 == "1_PRE"))
