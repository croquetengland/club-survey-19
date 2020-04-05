library(dplyr)
library(magrittr)
library(ggplot2)
library(janitor)
library(readxl)
library(stringr)
library(tidyr)

library(wordcloud) # handling and visualisatin text data
library(RColorBrewer)
library(tm)
library(SnowballC)
library(tidytext)

df <- read_xlsx("data/CA Clubs survey 2019 (Responses).xlsx")

# df_fullclublist <- read_xlsx("data/ClubsListMatched_2019_12_09.xlsx")

split_checklist <- function(df){
  df %>% 
    select(ClubName, Federation, split) %>% 
    mutate(split = str_split(split, ",")) %>% 
    unnest() %>% 
    mutate(split = str_trim(split, side = c("left")))
}

# Initial data cleaning ----
source("R/clean_data_initial.R")

# Process amendments to submissions ----
source("R/amend_club_data.R")

# drop name of contact and email
df_clean <- df_clean %>% 
  select(-(3:5))

df_pre_amend <- df_clean
df_clean <- df_temp

# Run primary data cleaning ----
source("R/clean_data_main.R")

# Courts, membership and recruitment ----
# Croquet-only club

# * Club land ownership----
g_club_land_ownership <- df_clean %>% 
  mutate(`Club Land Ownership` = str_replace(`Club Land Ownership`, " \\(.*\\)", "")) %>% 
  ggplot(aes(x = Federation, fill = `Club Land Ownership`)) +
  geom_bar()

# * Plays year-round----
g_plays_year_round <- df_clean %>% 
  ggplot(aes(x = YearRound)) + 
  geom_bar()

# * Number of courts----
g_num_courts <- df_clean %>% 
  select(ClubName, Federation, n_CourtsTotal, n_CourtsFull, n_CourtsHalf, NumCourtsTotal) %>% 
  mutate(FullCourtsOnly = n_CourtsHalf == 0 & n_CourtsTotal > 0) %>% 
  # gather(key, TotalNumberOfCourts, n_CourtsFull:n_CourtsHalf) %>% 
  ggplot(aes(x = NumCourtsTotal, fill = FullCourtsOnly)) + 
  geom_bar()

# * Number of full size courts

# * Club usage
g_club_usage <- df_clean %>% 
  ggplot(aes(x = n_DaysClubInUse)) +
  geom_bar() + 
  facet_wrap(~ Federation)


# Membership ----
g_total_membership <- df_clean %>% 
  ggplot(aes(x = n_MemTotal, fill = NumCourtsTotal)) + 
  geom_histogram(binwidth = 10, colour = "black") + 
  scale_x_continuous(breaks=seq(0,max(df_clean$n_MemTotal), 20)) 

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
  select(1:3,8,6, everything()) %>% 
  adorn_totals(where = "row")

# * Ideal membership----
df_clean <- df_clean %>% 
  mutate(MemPercentFull = n_MemTotal/NumMemIdeal*100)

g_membership_percentage_full <- df_clean %>% 
  ggplot(aes(x = MemPercentFull)) + 
  geom_histogram(binwidth = 10, colour = "black")

# * Wastage----
g_member_wastage <- df_clean %>% 
  ggplot(aes(x = NumMemWastage)) + 
  geom_bar()

# * Formal structures to integrate ----
df_structures_to_integrate_new_members <- df_clean %>%
  rename(split = `What formal/informal structures exist to integrate new members into your club? Check all that apply.`) %>% 
  split_checklist() %>% # custom function
  filter(split != "partnering a new member with an experienced player") %>% 
  rename(NewMemIntegration = split)

top_reasons <- df_structures_to_integrate_new_members %>% 
  count(NewMemIntegration, sort = TRUE) %>% 
  head(6) %>% 
  pull(NewMemIntegration)

df_structures_to_integrate_new_members <- df_structures_to_integrate_new_members %>% 
  mutate(Other = ifelse(NewMemIntegration %in% top_reasons, NA, NewMemIntegration),
         NewMemIntegration = ifelse(NewMemIntegration %in% top_reasons, NewMemIntegration, "Other"))

g_new_mem_integration <-
  df_structures_to_integrate_new_members %>% 
  ggplot(aes(x = NewMemIntegration)) + 
  geom_bar()

# * New member journey----
df_new_member_journey <- df_clean %>% 
  filter(!is.na(`What is the typical 'new member journey' into your club? How and when do you typically market and recruit for new members?`)) %>% # remove NA
  select(ClubName, Federation, New_member_journey = `What is the typical 'new member journey' into your club? How and when do you typically market and recruit for new members?`) %>% 
  mutate(WordCount = sapply(strsplit(.$New_member_journey, split = " "), length)) %>% # word count
  filter(WordCount > 3)

# * Recruitment issues----
df_recruitment_issues <- df_clean %>% 
  filter(!is.na(`Do you struggle to recruit each year? What are the main recruitment issues facing your club? Is the trend getting better or worse?`)) %>% # remove NA
  select(ClubName, Federation, Recruitment_issues = `Do you struggle to recruit each year? What are the main recruitment issues facing your club? Is the trend getting better or worse?`) %>% 
  mutate(WordCount = sapply(strsplit(.$Recruitment_issues, split = " "), length)) %>% # word count
  filter(WordCount > 3)

strings1 <- c("young","retire","old","infirm","death") #ignored 'age' as it can be misleading e.g. words containing 'age' such as 'manage'
num_age_affected <- df_clean %>% 
  filter(str_detect(`Do you struggle to recruit each year? What are the main recruitment issues facing your club? Is the trend getting better or worse?`, 
                    regex(paste(strings1, collapse = "|"), ignore_case = TRUE))) %>% 
  select(ClubName, Federation, `Do you struggle to recruit each year? What are the main recruitment issues facing your club? Is the trend getting better or worse?`)


# GC and AC ----
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

# * GC only clubs----
tbl_GC_only_by_fed <- df_clean %>% 
  group_by(Federation, `Does your club consider itself a GC/AC-only club, or both?`) %>% 
  summarise(
    Clubs = n()
  ) %>% 
  ungroup() %>% 
  spread(`Does your club consider itself a GC/AC-only club, or both?`, Clubs, fill = 0L) %>% 
  adorn_totals(where = c("row", "col"))

# Taking up AC----
tbl_taking_up_ac <- df_clean %>% 
  group_by(Federation) %>% 
  summarise(n = n(),
            PlaysSC = sum(PlaysShortCroquet == "Yes", na.rm = TRUE),
            NumSCPlayers = sum(n_PlaysShortCroquet, na.rm = TRUE),
            NumConverted = sum(n_GCToAC, na.rm = TRUE)
            ) %>% 
  adorn_totals()

# * What form of AC are beginners introduced to ----
df_croquet_for_beginners <- df_clean %>%
  rename(split = `When beginners join your club, which form of croquet do you introduce them to first - GC, AC or Short Croquet?`) %>% 
  split_checklist() %>% # custom function
  # filter(split != "partnering a new member with an experienced player") %>% 
  rename(WhichCroquetForBeginners = split)

top_reasons <- df_croquet_for_beginners %>% 
  count(WhichCroquetForBeginners, sort = TRUE) %>% 
  head(4) %>% 
  pull(WhichCroquetForBeginners)

df_croquet_for_beginners <- df_croquet_for_beginners %>% 
  mutate(Other = ifelse(WhichCroquetForBeginners %in% top_reasons, NA, WhichCroquetForBeginners),
         WhichCroquetForBeginners = ifelse(WhichCroquetForBeginners %in% top_reasons, WhichCroquetForBeginners, "Other"))

g_which_croquet_for_beginners <-
  df_croquet_for_beginners %>% 
  ggplot(aes(x = WhichCroquetForBeginners)) + 
  geom_bar()

# * How to encourage GC to AC ----
df_convert_GC_to_AC <- df_clean %>%
  rename(split = `How do you encourage your existing GC players to take up AC?`) %>% 
  split_checklist() %>% # custom function
  rename(HowGCToAC = split)

top_reasons <- df_convert_GC_to_AC %>% 
  count(HowGCToAC, sort = TRUE) %>% 
  head(4) %>% 
  pull(HowGCToAC)

df_convert_GC_to_AC <- df_convert_GC_to_AC %>% 
  mutate(Other = ifelse(HowGCToAC %in% top_reasons, NA, HowGCToAC),
         HowGCToAC = ifelse(HowGCToAC %in% top_reasons, HowGCToAC, "Other"))

g_how_gc_to_ac <-
  df_convert_GC_to_AC %>% 
  ggplot(aes(x = HowGCToAC)) + 
  geom_bar()

# Officials, competitons and clothing ----
# * Number of coaches and refs ----
tbl_coaches_referees <- df_clean %>% 
  group_by(Federation) %>% 
  summarise(ActiveCoaches = sum(as.numeric(n_CoachesActive), na.rm = TRUE),
            QualCoaches = sum(n_CoachesQual, na.rm = TRUE),
            QualRefs_AC = sum(n_RefsQualRegAC, na.rm = TRUE),
            QualRefs_GC = sum(n_RefsQualRegGC, na.rm = TRUE),
            QualRefs_dual = sum(n_RefsDualQual, na.rm = TRUE)
            ) %>% 
  ungroup() %>% 
  adorn_totals()

# * Number of handicappers ----
tbl_handicappers <- df_clean %>%
  group_by(Federation) %>% 
  summarise(
    Clubs = n(),
    wo_HCer = sum(HC_AC_reg_Q + HC_AC_inf_Q + HC_GC_reg_Q + HC_GC_inf_Q + HC_both_Q == 0),
    AC_Reg = sum(HC_AC_reg_Q),
    AC_Inf = sum(HC_AC_inf_Q),
    GC_Reg = sum(HC_GC_reg_Q),
    GC_Inf = sum(HC_GC_inf_Q),
    Both_Reg = sum(HC_AC_reg_Q & HC_GC_reg_Q & HC_both_Q),
    Both_Eith = sum(((!HC_AC_reg_Q & HC_GC_reg_Q) | (HC_AC_reg_Q & !HC_GC_reg_Q)) & HC_both_Q),
    Both_Inf = sum((!HC_AC_reg_Q & !HC_GC_reg_Q) & HC_both_Q)
  )

# * Competitions ----
df_competitions <- df_clean %>%
  rename(split = Competitions) %>% 
  split_checklist() %>% # custom function
  rename(Competitions = split)

top_reasons <- df_competitions %>% 
  count(Competitions, sort = TRUE) %>% 
  head(5) %>% 
  pull(Competitions)

df_competitions <- df_competitions %>% 
  mutate(Other = ifelse(Competitions %in% top_reasons, NA, Competitions),
         Competitions = ifelse(Competitions %in% top_reasons, Competitions, "Other"))

g_competitions <-
  df_competitions %>% 
  ggplot(aes(x = Competitions)) + 
  geom_bar()

# *Number entering Fixtures Calendar events
tbl_enters_CA_events <- df_clean %>% 
  group_by(Federation) %>% 
  summarise(TotalMembers = sum(n_MemTotal, na.rm = TRUE),
            EntersCAFixtures = sum(n_EntersFCTournaments, na.rm = TRUE)
            ) %>% 
  adorn_totals(where ="row") %>% 
  mutate(Percentage = scales::percent(EntersCAFixtures/TotalMembers, accuracy = 0.1))

# Marketing ----
# * How do clubs market themselves ----
df_marketing <- df_clean %>%
  rename(split = `How does your club market/advertise itself?`) %>% 
  split_checklist() %>% # custom function
  filter(!split %in% c("magazines)", "newspaper", "radio")) %>%
  rename(Marketing = split)

top_reasons <- df_marketing %>% 
  count(Marketing, sort = TRUE) %>% 
  head(5) %>% 
  pull(Marketing)

df_marketing <- df_marketing %>% 
  mutate(Other = ifelse(Marketing %in% top_reasons, NA, Marketing),
         Marketing = ifelse(Marketing %in% top_reasons, Marketing, "Other"))

g_marketing <-
  df_marketing %>% 
  ggplot(aes(x = Marketing)) + 
  geom_bar()

# * Corporates, charity, events ----
df_events <- df_clean %>% 
  filter(!is.na(`Does your club run corporate events, charity events or host local groups (e.g. schools, U3A, businesses) on a regular basis? Please describe in more detail.`)) %>% # remove NA
  select(ClubName, Federation, Events = `Does your club run corporate events, charity events or host local groups (e.g. schools, U3A, businesses) on a regular basis? Please describe in more detail.`) %>% 
  mutate(WordCount = sapply(strsplit(.$Events, split = " "), length)) %>% # word count
  filter(WordCount > 3)

# * Grants and sponsorship ----
df_grants <- df_clean %>% 
  filter(!is.na(`Does your club attract sponsorship or grants from outside the CA? Please give more details. Are they event-specific or year-round?`)) %>% # remove NA
  select(ClubName, Federation, GrantsAndSponsorship = `Does your club attract sponsorship or grants from outside the CA? Please give more details. Are they event-specific or year-round?`) %>% 
  mutate(WordCount = sapply(strsplit(.$GrantsAndSponsorship, split = " "), length)) %>% # word count
  filter(WordCount > 3)

# * NCD ----
df_ncd <- df_clean %>% 
  filter(!is.na(`Has your club taken part in National Croquet Day in the past? Was it useful for your club and will your club take part in National Croquet Week 2020?`)) %>% # remove NA
  select(ClubName, Federation, NationalCroquetDay = `Has your club taken part in National Croquet Day in the past? Was it useful for your club and will your club take part in National Croquet Week 2020?`) %>% 
  mutate(WordCount = sapply(strsplit(.$NationalCroquetDay, split = " "), length)) %>% # word count
  filter(WordCount > 3)


# Feedback ----
# * Feedback for the CA - what can CA do better ----
df_what_can_ca_do_better <- df_clean %>% 
  filter(!is.na(`What can the CA do better for your club?`)) %>% # remove NA
  select(ClubName, Federation, What_can_CA_do_better = `What can the CA do better for your club?`) %>% 
  mutate(WordCount = sapply(strsplit(.$What_can_CA_do_better, split = " "), length)) %>% # word count
  filter(WordCount > 3)


# * Other feedback for the CA ----
df_feedback_for_ca <- df_clean %>% 
  filter(!is.na(`What other feedback does your club have for the CA?`)) %>% 
  select(ClubName, Federation, Feedback_for_CA =`What other feedback does your club have for the CA?`) %>% 
  mutate(WordCount = sapply(strsplit(.$Feedback_for_CA, split = " "), length)) %>% 
  filter(WordCount > 3)


# Experimental----
# * Playing with the tm package ----
# df_temp <- df_what_can_ca_do_better %>%
#   pull(`What can the CA do better for your club?`) %>% 
#   gsub("'s", "", .) %>% 
#   tm::VectorSource() %>% 
#   tm::Corpus() %>% 
#   tm_map(removeNumbers) %>%
#   tm_map(removePunctuation) %>%
#   tm_map(stripWhitespace)
#   
# df_temp <- tm_map(df_temp, content_transformer(tolower))
# df_temp <- tm_map(df_temp, removeWords, stopwords("english"))
# df_temp <- tm_map(df_temp, stemDocument)

# toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x)) # option, remove if unneeded
# docs <- tm_map(docs, toSpace, "'s")

# dtm <- TermDocumentMatrix(df_temp)
# m <- as.matrix(dtm)
# v <- sort(rowSums(m),decreasing=TRUE)
# d <- data.frame(word = names(v),freq=v)
# head(d, 10)

# set.seed(1234)
# wordcloud(words = d$word, freq = d$freq, min.freq = 1,
#           max.words=200, random.order=FALSE, rot.per=0.35, 
#           colors=brewer.pal(8, "Dark2"))


tidy_do_better <- df_what_can_ca_do_better %>%
  mutate(What_can_CA_do_better = gsub("'s", "", .$What_can_CA_do_better)) %>% # remove apostrophe
  # mutate(`What can the CA do better for your club?` = gsub("[[:punct:]]", "", .$`What can the CA do better for your club?`)) %>% # remove special characters
  mutate(What_can_CA_do_better = tm::removeNumbers(What_can_CA_do_better)) %>% # keep only letters
  unnest_tokens(word, What_can_CA_do_better) %>% 
  mutate(lemma = textstem::lemmatize_words(word)) %>% # use lemmatize_words from textstem package
  # mutate_at("word", funs(wordStem((.), language = "en"))) # option using stemmer
  mutate(words_orig = word,
         word = lemma)

cleaned_do_better <- tidy_do_better %>%
  anti_join(get_stopwords())

# View response
cleaned_do_better %>%
  count(word, sort = TRUE)

# define a nice color palette
pal <- brewer.pal(8,"Dark2")

# plot the 50 most common words
cleaned_do_better %>% 
  count(word) %>% 
  with(wordcloud(word, n, random.order = FALSE, max.words = 50, colors=pal))

# play with sentiment analysis - not that useful
bing <- get_sentiments("bing")
do_better_sentiment <- cleaned_do_better %>%
  inner_join(bing) %>%
  count(ClubName, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)



tidy_feedback <- df_feedback_for_ca %>%
  mutate(Feedback_for_CA = tm::removeNumbers(Feedback_for_CA)) %>% # keep only letters
  mutate(Feedback_for_CA = gsub("'s", "", .$Feedback_for_CA)) %>% # remove apostrophe
  unnest_tokens(word, Feedback_for_CA) %>% 
  mutate(lemma = textstem::lemmatize_words(word)) %>% # use lemmatize_words from textstem package
  # mutate_at("word", funs(wordStem((.), language = "en"))) # option using stemmer
  mutate(words_orig = word,
         word = lemma)

cleaned_feedback <- tidy_feedback %>%
  anti_join(get_stopwords())

cleaned_feedback %>% 
  count(word, sort = TRUE)

cleaned_feedback %>% 
  count(word) %>% 
  with(wordcloud(word, n, random.order = FALSE, max.words = 50, colors=pal))

# Play with LDA for feedback column
# https://www.tidytextmining.com/topicmodeling.html
library(topicmodels)

word_counts <- cleaned_feedback %>% 
  count(ClubName, Federation, word, sort = TRUE)

feedback_dtm <- word_counts %>% 
  cast_dtm(Federation, word, n)

# set a seed so that the output of the model is predictable, create a k = N topic model
feedback_lda <- LDA(feedback_dtm, k = 5, control = list(seed = 1234))

# per topic per word probabilities
feedback_topics <- tidy(feedback_lda)
  
top_terms <- feedback_topics %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms

top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()

# which topics are associated to which document?
feedback_gamma <- tidy(feedback_lda, matrix = "gamma")

# reorder titles in order of topic 1, topic 2, etc before plotting
feedback_gamma %>%
  mutate(document = reorder(document, gamma * topic)) %>%
  ggplot(aes(factor(topic), gamma)) +
  geom_boxplot() +
  facet_wrap(~ document)

  
