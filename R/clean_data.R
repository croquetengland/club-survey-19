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
#' Chester - correction on no. playing SC 
#' Nil return from Luctonians
#' Livery Company not included
#' Pseudo response from e.g. Brodsworth
#' Tyneside - correct wastage number, 4 in 4 out, add GC ref
#' Mumbles Croquet Club - list of corrections
#' Maldon CC - feedback in Q26 and 27
#' Cathy Ferguson - granted extension, club not known
#' Crake Valley - chased Q8 and Q9
#' Hamptworth - handwritten form to be transcribed by C Roberts
#' Nottingham - chase numbers of AC/GC/both players
#' 11 hard copy forms to input

# Known issues to sort out: ----
#' Requested further information from some clubs e.g. Notts
#' Imputed some data where reasonable to do so
#' Tried to keep transcriptions as honest as possible; some minor typo fixes
#' A few clubs estimated numbers, some did not make sense (e.g. total no. of qualified coaches > no. of active coaches)
#' ASSUMPTION: all data submitted by clubs was correct and that data entry has been correct
#' Used code '999' in some numerical fields where there was no limit or answer unclear
#' Known duplicates - Pinchbeck, Worthing
#' Need to link to unique name and clubDbID on CA system

# Useful questions for the future:
#' John Bevington, Bedford: ageing committees
#' Ian Bond, Surviton: financial health of clubs
#' Ask about membership numbers in?



