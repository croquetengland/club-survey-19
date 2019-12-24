library(dplyr)
library(magrittr)
library(ggplot2)
library(janitor)
library(readxl)
library(stringr)
library(tidyr)



df <- read_xlsx("data/CA Clubs survey 2019 (Responses).xlsx")

source("R/clean_data.R")

# drop name of contact and email
df_clean <- df_clean %>% 
  select(-(3:5))

# Membership
g_total_membership <- df_clean %>% 
  ggplot(aes(x = `Number of members (total) at 30 September 2019`)) + 
  geom_histogram(binwidth = 10, fill = "white", colour = "black")

tbl_membership_by_fed <- df_clean %>% 
  group_by(`Your Federation`) %>% 
  summarise(
    Clubs = n(),
    NumPlayers = sum(`Number of members (total) at 30 September 2019`),
    NumRetired = sum(`Approximately how many of these are of retirement age?`),
    NumActive = sum(`Of the total, how many are active players?`),
    Num2ndClub = sum(`Of the total, how many are 2nd club members (i.e. their primary CA club is NOT your club)`),
    NumU25s = sum(`How many young person members (under 25 on 1 January 2019) do you have?`),
    NumUnique = NumPlayers - Num2ndClub
  ) %>% 
  adorn_totals(where = "row")

# GC and AC
tbl_GC_vs_AC_by_fed <- df_clean %>% 
  group_by(`Your Federation`) %>% 
  summarise(
    Clubs = n(),
    NumPlayers = sum(`Number of members (total) at 30 September 2019`),
    NumGCOnly = sum(`How many players play GC only?`),
    NumACOnly = sum(`How many players play AC only?`),
    NumBoth = sum(`How many players play BOTH AC and GC?`),
    NumConverted = sum(`How many of your existing GC players have taken up AC?`, na.rm = TRUE)
  ) %>% 
  adorn_totals(where = "row")

# GC only clubs
tbl_GC_only_by_fed <- df_clean %>% 
  group_by(`Your Federation`, `Does your club consider itself a GC/AC-only club, or both?`) %>% 
  summarise(
    Clubs = n()
  ) %>% 
  ungroup() %>% 
  spread(`Does your club consider itself a GC/AC-only club, or both?`, Clubs, fill = 0L) %>% 
  adorn_totals(where = c("row", "col"))

# Taking up AC
tbl_taking_up_ac <- df_clean %>% 
  group_by(`Your Federation`) %>% 
  summarise(n = n(),
            PlaysSC = sum(`Does your club play Short Croquet?` == "Yes", na.rm = TRUE),
            NumSCPlayers = sum(`If so, how many players play Short Croquet?`, na.rm = TRUE),
            NumConverted = sum(`How many of your existing GC players have taken up AC?`, na.rm = TRUE)
            ) %>% 
  adorn_totals()

# Number entering Fixtures Calendar events
tbl_enters_CA_events <- df_clean %>% 
  count(`Your Federation`, `How many of your club players enter CA Fixtures Calendar Tournaments?`)

# Feedback for the CA 1
df_what_can_ca_do_better <- df_clean %>% 
  filter(!is.na(`What can the CA do better for your club?`)) %>% 
  select(`Club Name`, `Your Federation`, `What can the CA do better for your club?`) %>% 
  mutate(WordCount = sapply(strsplit(.$`What can the CA do better for your club?`, split = " "), length)) %>% 
  filter(WordCount > 3)

# Other feedback for the CA
df_feedback_for_ca <- df_clean %>% 
  filter(!is.na(`What other feedback does your club have for the CA?`)) %>% 
  select(`Club Name`, `Your Federation`, `What other feedback does your club have for the CA?`) %>% 
  mutate(WordCount = sapply(strsplit(.$`What other feedback does your club have for the CA?`, split = " "), length)) %>% 
  filter(WordCount > 3)



  
  

