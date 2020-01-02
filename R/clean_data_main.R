df_clean_backup <- df_clean

# Simplify column names
df_clean <- df_clean %>% 
  rename(
    ClubName = `Club Name`,
    Federation = `Your Federation`,
    CroquetOnly = `Is your club a croquet-only club?`,
    PlaysYearRound = `Does your club play year-round?`,
    n_CourtsTotal = `How many courts do you have (total)?`,
    n_CourtsFull = `Of the total number, how many courts are full size?`,
    n_CourtsHalf = `Of the total number, how many courts are 3/4 size or smaller?`,
    n_DaysClubInUse = `How many days a week is your club in use, during a 'typical' week?`,
    n_MemTotal = `Number of members (total)`,
    n_MemRetired = `Approximately how many of these are of retirement age?`,
    n_MemYoung = `How many young person members do you have?`,
    n_MemActive = `Of the total, how many are active players?`,
    n_MemSocial = `Of the total, how many are associate/social/non-playing?`,
    n_Mem2nd = `Of the total, how many are 2nd club members (i.e. their primary CA club is NOT your club)`,
    n_MemIdeal = `What is the ideal (maximum) number of members  for your club?`,
    n_MemFull = `Are you full? ( ie you have a waiting list or easily replace leavers each year)`,
    n_MemWastage = `What is your average "wastage" (leavers) each year ?`,
    n_PlaysGC = `How many players play GC only?`,
    n_PlaysAC = `How many players play AC only?`,
    n_PlaysBoth = `How many players play BOTH AC and GC?`,
    PlaysShortCroquet = `Does your club play Short Croquet?`,
    n_PlaysShortCroquet = `If so, how many players play Short Croquet?`,
    n_GCToAC = `How many of your existing GC players have taken up AC?`,
    n_CoachesActive = `How many active coaches do you have?`,
    n_CoachesQual = `How many of these active coaches have CA qualifications (badges)?`,
    n_RefsQualRegAC = `How many qualified registered AC Referees do you have?`,
    n_RefsQualRegGC = `How many qualified registered GC Referees do you have?`,
    n_RefsDualQual = `How many of your Referees are dual-qualified in AC and GC?`,
    Competitions = `Please select all that apply`,
    n_EntersFCTournaments = `How many of your club players enter CA Fixtures Calendar Tournaments?`
  )

# Club land ownership ----
# Observed similarity
df_clean <- df_clean %>% 
  mutate(`Who owns the land on which the club is situated?` = ifelse(`Who owns the land on which the club is situated?` == "Playing fields assoc.",
                                                                     "Playing Fields Association",
                                                                     `Who owns the land on which the club is situated?`)
         )

# Simplify categories by grouping into similar themes
strings1 <- c("trust", "estate", "heritage", "charity")
strings2 <- c("university", "sports", "tennis", "water", "land owner", "village hall")
df_clean <- df_clean %>% 
  mutate(`Club Land Ownership` = ifelse(
    grepl(paste(strings1, collapse = "|"), x = `Who owns the land on which the club is situated?`, ignore.case = T),
    "A trust/estate/charity",
    ifelse(
      grepl(paste(strings2, collapse = "|"), x = `Who owns the land on which the club is situated?`, ignore.case = T),
      "Other local land owner",
      `Who owns the land on which the club is situated?`
    )
  )
  )

top_owner_cats <-df_clean %>% count(`Club Land Ownership`) %>% arrange(desc(n)) %>% head(6)

df_clean <- df_clean %>% 
  mutate(`Club Land Ownership` = ifelse(
    `Club Land Ownership` %in% top_owner_cats$`Club Land Ownership`,
    `Club Land Ownership`,
    "Other"
  ))

# Number of lawns total
df_clean <- df_clean %>% 
  mutate(NumCourtsTotal = as.factor(
    ifelse(n_CourtsTotal >5, 
           ">5", 
           n_CourtsTotal)
    )
  )

df_clean %>% 
  select(ClubName,
         n_CourtsTotal,
         n_CourtsFull,
         n_CourtsHalf,
         NumCourtsTotal) %>% 
  mutate(LawnsTot = n_CourtsFull + n_CourtsHalf, 
         IsCorr = n_CourtsTotal == LawnsTot) %>% 
  View()
