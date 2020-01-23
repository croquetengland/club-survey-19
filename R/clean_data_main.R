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
    MemFull = `Are you full? ( ie you have a waiting list or easily replace leavers each year)`,
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

# Number of courts total ----
df_clean <- df_clean %>% 
  mutate(NumCourtsTotal = as.factor(
    ifelse(n_CourtsTotal >5, 
           "5+", 
           n_CourtsTotal)
    )
  )

# Check to see if these match
df_clean %>% 
  select(ClubName,
         n_CourtsTotal,
         n_CourtsFull,
         n_CourtsHalf,
         NumCourtsTotal) %>% 
  mutate(CourtsTot = n_CourtsFull + n_CourtsHalf, 
         IsCorr = n_CourtsTotal == CourtsTot) 

# correct Crawley CC
df_clean$n_CourtsFull[str_detect(df_clean$ClubName, "Crawley")] <- 2

# Populate new list of clubs with correct court count
df_incorrect_court_count<- df_clean %>% 
  select(ClubName,
         n_CourtsTotal,
         n_CourtsFull,
         n_CourtsHalf,
         NumCourtsTotal) %>% 
  mutate(CourtsTot = n_CourtsFull + n_CourtsHalf, 
         IsCorr = n_CourtsTotal == CourtsTot) %>% 
  filter(IsCorr != T)

# Croquet only club ----

# amendments to be made if necessary

# Plays year round ----
strings1 <- c("indoor", "indoors")
strings2 <- c("For many years we have only played during the typical croquet season, but this winter we are trialling staying open all year-round.",
              "We aim to play all year round, but the lawns are often too wet to play in the winter.",
              "We have a rough lawn which is open during the winter, but no formal sessions",
              "In winter, play often restricted to certain lawns for maintenance/rest of lawns",
              "My club runs sessions all year-round, weather permitting.")
strings3 <- c("My club plays during the typical croquet season and closes for winter.",
              "May to Septemebr")
df_clean <- df_clean %>% 
  mutate(YearRound = ifelse(
    grepl(paste(strings1, collapse = "|"), x = PlaysYearRound, ignore.case = T),
    "Plays indoors during winter.",
    ifelse(
      grepl(paste(strings2, collapse = "|"), x = PlaysYearRound, ignore.case = T),
      "All year-round, weather permitting.",
      ifelse(
        grepl(paste(strings3, collapse = "|"), x = PlaysYearRound, ignore.case = T),
        "Typical croquet season",
        "Extended croquet season"
      )
    )
  )
  )

# Ideal club membership ----
string1 <- c(0, 999)
df_clean <- df_clean %>% 
  mutate(NumMemIdeal = gsub(".*-","",n_MemIdeal)) %>% 
  mutate(NumMemIdeal = gsub("[^0-9\\.]","",NumMemIdeal)) %>% 
  mutate(NumMemIdeal = as.integer(NumMemIdeal)) %>% 
  mutate(NumMemIdeal = ifelse(NumMemIdeal %in% string1, NA, NumMemIdeal))

# Wastage
df_clean <- df_clean %>%
  mutate(NumMemWastage  = case_when(
    n_MemWastage > 0 & n_MemWastage <=5 ~ "5 or fewer",
    n_MemWastage > 5 & n_MemWastage <= 10 ~ "5 to 10",
    n_MemWastage > 10 & n_MemWastage < 999 ~ "10 or more",
    TRUE ~ "Not Known"
  )) %>% 
  mutate(NumMemWastage = factor(NumMemWastage, levels = c("5 or fewer", "5 to 10", "10 or more", "Not Known")))
