##-----------------
# 1. Set UP 
##-----------------

# Open Libraries
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(rstatix)

options(stringsAsFactors = FALSE)

##-------------------------------------------------
## 2. Load Raw IAT (implicit association test) Data 
##-------------------------------------------------
IAT_raw <- read.csv(
  "../data/raw_data/RawIATData_Exp3B.csv"
)

glimpse(IAT_raw)

##----------------------------------------------------------------
# 3. Participant-Level Exclusions: Complete First Participation Only 
##----------------------------------------------------------------

# 1) Count trials per participation
participation_summary <- IAT_raw %>%
  count(subject, name = "n_trials")

# Show distribution of total num of trials
table(participation_summary$n_trials)

# Indicate the number of attempts per participant
participation_summary <- IAT_raw %>%
  count(subject,time, name = "n_trials") %>%
  arrange(subject) %>%
  group_by(subject)%>%
  mutate(attempt = row_number())%>%
  ungroup()

# Show number of trials and attempts
table(participation_summary$n_trials, participation_summary$attempt)

# 2) Keep only the complete (114 trials) and first participation
valid_participation <- participation_summary %>%
  filter(n_trials == 114, attempt == 1) 

valid_participation %>%nrow()

# 4) Filter raw data by complete first participation
IAT <- IAT_raw %>%
semi_join(
  valid_participation,
  by = c("subject", "time")
)

# 5) Sanity check 
# Each subject appears once
IAT %>%
  distinct(subject, time) %>%
  count(subject) %>%
  filter(n != 1)

# Each participation has exactly 114 trials
IAT %>%
  count(subject, time) %>%
  filter(n != 114)

## ------------------------------------
## 4. Participant-Level Quality Metrics
## ------------------------------------

# Count how many total participants 
nrow(IAT%>%group_by(subject)%>%summarise(count=n()))

#Error Rate of each participants
error_summary <- IAT %>%
  group_by(subject) %>%
  summarise(
    error_rate = 1 - mean(correct, na.rm = TRUE),
    n_trials   = n(),
    .groups = "drop"
  )

boxplot(error_summary$error_rate)
# Most participants have reasonable error rate with some participants with high error rates.

# Save subject ids for those with error rate greater than .33
high_error_subjects <- error_summary %>%
  filter(error_rate > .33) %>%
  pull(subject)


## -------------------------------------------------------------------
## 5. Block-Level Feature Engineering for Treatment and Method Factors
## -------------------------------------------------------------------

# Derive block-level variables from the experimental condition string:
# 1) moralleft: which side of keys moral and immoral was mapped (0: immoral-left vs. 1: moral-left)
# 2) distract : distractor type (color- vs. vain-related)
# 3) congruent: whether morality and attractiveness share same response key (c) vs. not (i)
# 4) block order: which block came first (congruent first vs. incongruent first) 

IAT <- IAT %>%
  mutate(
    ## Moral side mapping
    moralleft = if_else(str_sub(condition, 1, 3) == "IMM", 0, 1),
    
    ## Distractor condition
    distract = if_else(str_sub(condition, 8, 9) == "co", 
                       "color", "vain"),
    
    ## Congruency
    congruent = case_when(
      str_sub(condition, 5, 6) == "CI" & blocknum %in% 1:3 ~ "c",
      str_sub(condition, 5, 6) == "CI" & blocknum %in% 4:6 ~ "i",
      str_sub(condition, 5, 6) == "IC" & blocknum %in% 1:3 ~ "i",
      str_sub(condition, 5, 6) == "IC" & blocknum %in% 4:6 ~ "c",
      TRUE ~ NA_character_
      ),
      
    ## Block order
    order = if_else(
      str_sub(condition, 5, 6) == "CI", 
      "cFirst", "iFirst"
      )
    )
  
## ---------------------------------------------------
## 6. Trial-Level Feature Engineering for Target Word
## ---------------------------------------------------

# Note: make a variable "target" to indicate which type of words were presented 

IAT <- IAT %>%
  mutate(
    target = case_when(
      blocknum %in% c(1, 4) ~ str_sub(trialcode, 7, 7),
      blocknum %in% c(2, 5) ~ str_sub(trialcode, 9, 9),
      blocknum %in% c(3, 6) ~ str_sub(trialcode, 8, 8),
      TRUE ~ NA_character_
    ),
    target = recode(
      target,
      "m" = "moral",
      "i" = "immoral",
      "a" = "attractive",
      .default = "distractor"
    )
  )

## -------------------------------------------------------------------
## 7. Coding & Sanity Checks
## -------------------------------------------------------------------

# 1. Checks Block-Level Variables

# Each subject should have only one moral key mapping and distractor condition
IAT %>%
  distinct(subject, moralleft, distract, order) %>%
  count(subject) %>%
  filter(n != 1)

# Check moralleft with condition
IAT %>%
  count(condition, moralleft)

# Congruency should have equal number across levels, but not be missing
table(IAT$congruent, useNA = "ifany")

# Check congruent with order X block number
IAT %>%
  count(order, blocknum, congruent)

# Distractor condition should be approximately balanced across subjects
IAT %>%
  distinct(subject, distract) %>%
  count(distract)

# 2. Checks trial-level coding: Target Words

table(IAT$target, IAT$trialcode)

# larger view in greater table
IAT %>%
  filter(blocknum %in% c(3, 6),
         target %in% c("moral", "immoral", "attractive")) %>%
  head()

# 3. Sanity Checks: Response–Correctness Consistency

# Note: 
# Response keys are 33 = left key, 36 = right key, 57 = spacebar (used for vanity distractor)
# correct == 1 indicates the response matched the instructed key mapping.


# 1) target == "distractor" trials responded with spacebar (57) should always be correct.

distractor_violations <- IAT %>%
  filter(target == "distractor", response == 57) %>%
  filter(!(correct == 1))

distractor_violations %>%
  nrow() 

# Expected 0 row, but there are errors in 'correct'

## -------------------------------------------------------------------
## 8.  Data Clenaing: Re-define 'correct'
## -------------------------------------------------------------------


IAT <- IAT %>%
  mutate(
    revised_correct = case_when(
      ## Distractor target trials
      target == "distractor" & response == 57 ~ 1,
      
      ## Moral and immoral target trials in Moral-left Block
      moralleft == 1 & target == "moral" & response == 33 ~ 1,
      moralleft == 1 & target == "immoral" & response == 36 ~ 1,

      
      ## Moral and immoral target trials in Moral-right Block
      moralleft == 0 & target == "moral" & response == 36 ~ 1,
      moralleft == 0 & target == "immoral" & response == 33 ~ 1,
      
      ## Attractive target trials in moral-left Block
      target == "attractive" & moralleft == 1 & congruent == 1 & response == 33 ~ 1,
      target == "attractive" & moralleft == 1 & congruent == 0 & response == 36 ~ 1,      

      ## Attractive target trials in moral-right Block
      target == "attractive" & moralleft == 0 & congruent == 1 & response == 36 ~ 1,
      target == "attractive" & moralleft == 0 & congruent == 0 & response == 33 ~ 1,      
      
      
      TRUE ~ 0
    )
  )

## Re-Sanity Check for 'correct' variable

# 1) target == "distractor" trials responded with spacebar (57) should always be correct.

distractor_violations <- IAT %>%
  filter(target == "distractor", response == 57) %>%
  filter(!(revised_correct == 1))

distractor_violations %>%
  nrow() 
# Expect 0 row

# 2) When moralleft = 1,
#    if target = moral, then the response with left key (33) is correct (== 1)
#    if target = immoral, then the response with right key (36) is correct (== 1)
# When moralleft = 0,
#    if target = moral, then the response with right key (36) is correct (== 1)
#    if target = immoral, then the response with left key (33) is correct (== 1)

morimm_violations <- IAT %>%
  filter(moralleft == 1, target == "moral", response == 33) %>%
  filter(!(revised_correct == 1))

morimm_violations %>%
  nrow() 
# Expect 0 row

# 3) When target = attractive,
#   if moralleft = 1, congruent = 1, then response with left key (33) is correct
#   if moralleft = 1, congruent = 0, then response with right key (36) is correct
#   if moralleft = 0, congruent = 1, then response with right key (36) is correct
#   if moralleft = 0, congruent = 0, then response with right key (33) is correct

attractive_violations <- IAT %>%
  filter(
    (moralleft == 1 & congruent == 1 & response == 33) |
      (moralleft == 1 & congruent == 0 & response == 36) |
      (moralleft == 0 & congruent == 1 & response == 36) |
      (moralleft == 0 & congruent == 0 & response == 33)
  ) %>%
  filter(revised_correct != 1) 

attractive_violations %>%
  nrow() 
# Expect 0 row

# make new clean IAT data with revised correct variable
IAT_clean <- IAT %>%
  mutate(correct = revised_correct, .keep = "unused") %>%
  relocate(correct, .after = response) 

## ----------------------------------
## 9. Load and Clean Demographic Data
## ----------------------------------

dat_demo <- read.csv(
  "../data/raw_data/RawDemoData_Exp3B.csv")

glimpse(dat_demo)

dat_demo <- dat_demo %>%
  # select only the relevant columns
  select(
    subject, time,
    gender_response, age_response,
    ethnicity_response, english_response,
    political_response
  ) %>%
  #set the ordered levels of political orientation
  mutate(
    political_response = factor(
      political_response,
      levels = c(
        "Extremely Liberal", "Moderately Liberal", "Slightly Liberal",
        "Middle of the Road", "Slightly Conservative",
        "Moderately Conservative", "Extremely Conservative"
      ),
      labels = 1:7,
      ordered = TRUE
    )
  )


# Identify Duplicate Participant
dat_demo %>%
  count(subject) %>%
  filter(n > 1)

demo_clean <- dat_demo %>%
  arrange(subject, time) %>%     
  group_by(subject) %>%
  slice(1) %>%                   
  ungroup()

# Sanity Check: Each participant should appear exactly once
demo_clean %>%
  count(subject) %>%
  filter(n != 1)


## -------------------------------------------
## 10. Merge Readiness Check
## -------------------------------------------

# 1) Identify and change any overlapping column names
intersect(names(IAT_clean), names(demo_clean))

# change the column 'time' in demo_clean to 'time_demo'
demo_clean <- demo_clean%>%
  rename(time_demo = time)

# 2) Check join key (subject)
# any missing
sum(is.na(demo_clean$subject))
sum(is.na(IAT_clean$subject))

# data structure match
class(IAT_clean$subject) == class(demo_clean$subject)

# 3) Cardinality expectation check
# Expected relationship: 114 Trial in IAT data row = One demo data row 

# Check if all subject has 114 trials
IAT_clean %>%
  count(subject) %>%
  filter(n != 114) 

# 4) Check Incongruency in subject between two data
# Subject in IAT but missing Demo
anti_join(IAT_clean, demo_clean, by = "subject") %>% distinct(subject)

# Subject in Demo but missing IAT
anti_join(demo_clean, IAT_clean, by = "subject")
# Two participants with demographics but no valid IAT data. 
# , suggesting jumping to demo survey without completing IAT tasks.
# These can be safely excluded from the final data.

## ------------------------------------------
## 11. Merge Cleaned IAT and Demographic Data
## ------------------------------------------

# Merge by subject exists in IAT_clean (left join)
dat <- IAT_clean %>%
  left_join(demo_clean, by = "subject")

# check if there are any deleted, duplicated, or altered rows
isTRUE(
  nrow(dat) == nrow(IAT_clean) &
    n_distinct(dat$subject) == n_distinct(IAT_clean$subject)
)

## -------------------------
## 12. Final Sanity Checks
## -------------------------

# Preview
head(dat,20)

# Total number of participant
nrow(dat %>% count(subject))

# Total number of observations and variables
dim(dat)

# trials per participation not 114
dat %>%
  count(subject, name = "n_trials")%>%
  filter(n_trials != 114)

# Any duplicate participants
dat %>%
  distinct(subject, time) %>%
  count(subject) %>%
  filter(n != 1)

# Save final clean data

write.csv(
  dat,
  "../data/processed_data/data_Std3B_clean.csv",
  row.names = FALSE
)

