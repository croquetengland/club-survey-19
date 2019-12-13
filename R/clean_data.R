df_clean <- df

# Tidy up column headers ----
colnames(df_clean)[2:51] <- substring(colnames(df_clean), first = 4)[2:51]

colnames(df_clean) <- trimws(colnames(df_clean), which = "left")

colnames(df_clean)[grepl(") ", substr(colnames(df_clean), 1, 3))] <- substring(colnames(df_clean), first = 4)[grepl(") ", substr(colnames(df_clean), 1, 3))]

# Replace & with 'and'
df_clean <- df_clean %>% 
  mutate_if(is.character, funs(str_replace(., "&", "and")))

# Remove duplicates ----
df_clean <- df_clean %>% 
  filter(!`Club Name` %in% c("I have to fill this in to see the form",
                                "Pinchbeck C.C."))

# Change some columns to numeric
df_clean <- df_clean %>% 
  mutate(`If so, how many players play Short Croquet?` = as.numeric(`If so, how many players play Short Croquet?`)
  )

# Include list of amendments that clubs have sent in, for audit trail ----
