# Include list of amendments that clubs have sent in, for audit trail
df_temp <- df_clean

# Individual corrections by club ----
#' Chester - correction on no. playing SC to 14
df_temp$`If so, how many players play Short Croquet?`[str_detect(df_temp$`Club Name`, "Chester")] <- 14 # best guess

#' Nil return from Luctonians
# No action req
# No action req

#' Livery Company not included
# No action req

#' Tyneside - correct wastage number, 4 in 4 out, add GC ref
df_temp$`What is your average "wastage" (leavers) each year ?`[str_detect(df_temp$`Club Name`, "Tyneside")] <- 4 # average as 4 in 4 out
df_temp$`How many qualified registered GC Referees do you have?`[str_detect(df_temp$`Club Name`, "Tyneside")] <- 1 

#' Mumbles Croquet Club - list of corrections
df_temp$`What is your average "wastage" (leavers) each year ?`[str_detect(df_temp$`Club Name`, "Mumble")] <- NA # Q9c wastage not relevant for club in 1st year
df_temp$`Does your club play Short Croquet?`[str_detect(df_temp$`Club Name`, "Mumble")] <- "No"
df_temp$`When beginners join your club, which form of croquet do you introduce them to first - GC, AC or Short Croquet?`[str_detect(df_temp$`Club Name`, "Mumble")] <- "GC"
df_temp$`How many of your existing GC players have taken up AC?`[str_detect(df_temp$`Club Name`, "Mumble")] <- 0
df_temp$`Does your club have an active registered club handicapper(s)?`[str_detect(df_temp$`Club Name`, "Mumble")] <- "None"
df_temp$`Does your club have a 'wearing of whites' rule? (not inc winter arrangements)`[str_detect(df_temp$`Club Name`, "Mumble")] <- NA # Indicates they currently doen't have a whites policy

#' Maldon CC - feedback in Q26 and 27
df_temp$`What can the CA do better for your club?`[str_detect(df_temp$`Club Name`, "Maldon")] <- "
a. To advise Clubs when significant changes/advice are made to the Website i.e. Safeguarding: Data Protection,
b. The CA could do better in promoting croquet through the media in all its' forms - yes, it does a lot but it could and should do more."

df_temp$`What other feedback does your club have for the CA?`[str_detect(df_temp$`Club Name`, "Maldon")] <- paste0(
  df_temp$`What other feedback does your club have for the CA?`[str_detect(df_temp$`Club Name`, "Maldon")], ", 
  a. The Croquet Gazette is great for clubs and players.  An increase in other publicity to overcome the image of the game being elitist,
b. Magazine - an excellent publication - could it perhaps include a regular feature on how individual clubs manage themselves on a day to day basis.
I suspect that we share a lot of common concerns, i.e. security of tenure, funding day to day existence, ground management etc etc and could benefit from knowing how other clubs manage")

#' Cathy Ferguson - granted extension, club not known
# Digging around CA database suggested this is from Hove Beach who have submitted a return.

#' Crake Valley - chased Q8 and Q9
# This was input directly as a new entry including amended answers to Q8 and Q9

#' Hamptworth - handwritten form to be transcribed by C Roberts

#' Nottingham - chase numbers of AC/GC/both players
df_temp$`How many players play GC only?`[str_detect(df_temp$`Club Name`, "Nottingham")] <- 20
df_temp$`How many players play AC only?`[str_detect(df_temp$`Club Name`, "Nottingham")] <- 63
df_temp$`How many players play BOTH AC and GC?`[str_detect(df_temp$`Club Name`, "Nottingham")] <- 40

#' Brodsworth Hall - incomplete

#' Bexley
df_temp$`How many days a week is your club in use, during a 'typical' week?`[str_detect(df_temp$`Club Name`, "Bexley")] <- 3

#' Typo on Southport - should be 0 members u25, 69 active members.
df_temp$`How many young person members do you have?`[str_detect(df_temp$`Club Name`, "Southport")] <- 0
df_temp$`Of the total, how many are active players?`[str_detect(df_temp$`Club Name`, "Southport")] <- 69


# Known issues and assumptions to sort out ----
#' Imputed some data where reasonable to do so - e.g. taken midpoints, taken upper limit of range was two numbers
#' Tried to keep transcriptions as honest as possible; some minor typo fixes
#' Some clubs estimated numbers, some did not make sense (e.g. total no. of qualified coaches > no. of active coaches)
#' ASSUMPTION: all data submitted by clubs was correct and that data entry has been correct (but happy to be corrected)
#' Used code '999' in some numerical fields where there was no limit or answer unclear

#' Known duplicates - Pinchbeck, Worthing - need to consolidate the duplicate mismatches
df_temp <- df_temp %>% 
  filter(!`Club Name` %in% c("I have to fill this in to see the form"))

# df_temp %>% 
#   filter(str_detect(`Club Name`, "Pinchbeck")) %>% 
#   View()

# One further duplicate from Pinchbeck and one from Worthing
#' Need to link to unique name and clubDbID on CA system


# Useful questions for the future: ----
#' John Bevington, Bedford: ageing committees
#' Ian Bond, Surbiton: financial health of clubs
#' Linking in to previous Fed level responses
#' Ask about membership numbers in?



