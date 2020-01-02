library(dplyr)
library(magrittr)
library(ggplot2)
library(janitor)
library(readxl)
library(stringr)
library(tidyr)



df <- read_xlsx("data/CA Clubs survey 2019 (Responses).xlsx")

source("R/clean_data_initial.R")

# drop name of contact and email
df_clean <- df_clean %>% 
  select(-(3:5))

source("R/amend_club_data.R")

df_pre_amend <- df_clean
df_clean <- df_temp

source("R/clean_data_main.R")

# Courts, membership and recruitment ----

# Croquet-only club

# Club land ownership
g_club_land_ownership <- df_clean %>% 
  ggplot(aes(x = Federation,fill = `Club Land Ownership`)) +
  geom_bar()

# Plays year-round

# Number of courts

# Number of full size courts

# Membership
g_total_membership <- df_clean %>% 
  ggplot(aes(x = n_MemTotal)) + 
  geom_histogram(binwidth = 10, fill = "white", colour = "black")

tbl_membership_by_fed <- df_clean %>% 
  group_by(Federation) %>% 
  summarise(
    Clubs = n(),
    NumPlayers = sum(n_MemTotal),
    NumRetired = sum(n_MemRetired),
    NumActive = sum(n_MemActive),
    Num2ndClub = sum(n_Mem2nd),
    NumU25s = sum(n_MemYoung),
    NumUnique = NumPlayers - Num2ndClub
  ) %>% 
  adorn_totals(where = "row")

# GC and AC
tbl_GC_vs_AC_by_fed <- df_clean %>% 
  group_by(Federation) %>% 
  summarise(
    Clubs = n(),
    NumPlayers = sum(n_MemTotal),
    NumGCOnly = sum(n_PlaysGC),
    NumACOnly = sum(n_PlaysAC),
    NumBoth = sum(n_PlaysBoth),
    NumConverted = sum(n_GCToAC, na.rm = TRUE)
  ) %>% 
  adorn_totals(where = "row")

# GC only clubs
tbl_GC_only_by_fed <- df_clean %>% 
  group_by(Federation, `Does your club consider itself a GC/AC-only club, or both?`) %>% 
  summarise(
    Clubs = n()
  ) %>% 
  ungroup() %>% 
  spread(`Does your club consider itself a GC/AC-only club, or both?`, Clubs, fill = 0L) %>% 
  adorn_totals(where = c("row", "col"))

# Taking up AC
tbl_taking_up_ac <- df_clean %>% 
  group_by(Federation) %>% 
  summarise(n = n(),
            PlaysSC = sum(PlaysShortCroquet == "Yes", na.rm = TRUE),
            NumSCPlayers = sum(n_PlaysShortCroquet, na.rm = TRUE),
            NumConverted = sum(n_GCToAC, na.rm = TRUE)
            ) %>% 
  adorn_totals()

# Number entering Fixtures Calendar events
tbl_enters_CA_events <- df_clean %>% 
  count(Federation, n_EntersFCTournaments)

# Feedback for the CA 1
df_what_can_ca_do_better <- df_clean %>% 
  filter(!is.na(`What can the CA do better for your club?`)) %>% 
  select(ClubName, Federation, `What can the CA do better for your club?`) %>% 
  mutate(WordCount = sapply(strsplit(.$`What can the CA do better for your club?`, split = " "), length)) %>% 
  filter(WordCount > 3)

# Other feedback for the CA
df_feedback_for_ca <- df_clean %>% 
  filter(!is.na(`What other feedback does your club have for the CA?`)) %>% 
  select(ClubName, Federation, `What other feedback does your club have for the CA?`) %>% 
  mutate(WordCount = sapply(strsplit(.$`What other feedback does your club have for the CA?`, split = " "), length)) %>% 
  filter(WordCount > 3)



  
  

