# Initial cleaning of data set for ease of further manipulation

df_clean <- df

# Tidy up column headers ----
colnames(df_clean)[2:51] <- substring(colnames(df_clean), first = 4)[2:51] # remove question numbers from all except timestamp

colnames(df_clean) <- trimws(colnames(df_clean), which = "left") # remove white spaces from left if exist

colnames(df_clean)[grepl(") ", substr(colnames(df_clean), 1, 3))] <- substring(colnames(df_clean), first = 4)[grepl(") ", substr(colnames(df_clean), 1, 3))] # remove subquestion number (contains right bracket so easy to find)

colnames(df_clean)[str_detect(colnames(df_clean), "September")] <- "Number of members (total)" # remove date from total numbers colname

colnames(df_clean)[str_detect(colnames(df_clean), "January")] <- "How many young person members do you have?" # remove date from u25 numbers colname

# Replace & with 'and'
df_clean <- df_clean %>% 
  mutate_if(is.character, funs(str_replace(., "&", "and")))


# Change some columns to numeric
df_clean <- df_clean %>% 
  mutate(`If so, how many players play Short Croquet?` = as.numeric(`If so, how many players play Short Croquet?`)
  )

