# Load required packages
library(tidyverse, warn.conflicts = FALSE)
library(magrittr)
library(bigrquery)
library(caret)

# Collect the data
con <- DBI::dbConnect(drv = bigquery(), project = "learnclinicaldatascience")

diabetes_notes <- tbl(con, "course4_data.diabetes_notes") %>%
  collect()

goldStd <- tbl(con, "course4_data.diabetes_goldstandard")

goldStd

# Explore the different note types
diabetes_notes %>%
  group_by(NOTE_TYPE) %>%
  count(NOTE_TYPE)

# The following regex patterns were formed after a random manual review of notes

# Using regex patterns for a first attempt through dataset to classify each complication
diabetes_notes %<>%
  mutate(diabetic_neuropathy_pred = case_when(str_detect(string = TEXT, pattern=regex("(?<![a-zA-Z])(neuopathic pain)|neuropathy|tingling|numbness|numb |(nerve pain)(?![a-zA-z])", ignore_case = TRUE)) ~ 1, TRUE ~ 0),
         diabetic_nephropathy_pred = case_when(str_detect(string = TEXT, pattern=regex("(?<![a-zA-Z])nephropathy(?![a-zA-z])", ignore_case = TRUE)) ~ 1, TRUE ~ 0),
         diabetic_retinopathy_pred = case_when(str_detect(string = TEXT, pattern=regex("(?<![a-zA-Z])retinopathy|optic nerve damage|visual complaints(?![a-zA-z])", ignore_case = TRUE)) ~ 1, TRUE ~ 0)) %>%
  mutate(any_diabetic_complication_pred = case_when(diabetic_neuropathy_pred == 1 | diabetic_nephropathy_pred == 1 | diabetic_retinopathy_pred == 1 ~ 1, TRUE ~ 0))

# Consider each condition separately. 
# Review performance of our algorithm vs the gold standard for that condition, and refine condition-specific algorithm  
# 1. Retinopathy 
retinopathy_notes <- diabetes_notes %>%  
  select(NOTE_ID, TEXT, diabetic_retinopathy_pred) %>%
  collect()

retinopathy_notes

retinopathy_goldStd <- goldStd %>%
  select(NOTE_ID, DIABETIC_RETINOPATHY) %>%
  collect()

retinopathy_goldStd

# Check retinopathy accuracy
retinopathy_goldStd %>%
  inner_join(retinopathy_notes) %>% 
  filter(DIABETIC_RETINOPATHY != diabetic_retinopathy_pred)

# 5 notes were misclassified. Let's manually review these notes to identify cause of mismatch
# Strategy 1: Identify and discard notes with denial words "no retinopathy" and "does not have any serious visual complaints"
# Strategy 2: Identify and discard notes where chosen keywords appear in the "FAMILY HISTORY" section. 

# Strategy 1:
retinopathy_notes %<>%
  mutate(diabetic_retinopathy_pred = case_when(str_detect(TEXT, regex(pattern = "no retinopathy|does not have any serious visual complaints", ignore_case = TRUE)) ~ 0, TRUE ~ diabetic_retinopathy_pred)) %>%
  collect()

# Check accuracy again
retinopathy_goldStd %>%
  inner_join(retinopathy_notes) %>% 
  collect()%>%
  filter(DIABETIC_RETINOPATHY != diabetic_retinopathy_pred)

# Strategy 2:
# If retinopathy is identified in the family history section, exclude it. 
# It was noted as part of Strategy 1 the NOTE_ID = 1 did not have the ":" separator after "FAMILY HISTORY"

retinopathy_notes_to_exclude <- retinopathy_notes %>%
  separate_rows(TEXT, sep = "\n\n") %>%
  filter(str_detect(TEXT, "FAMILY HISTORY")) %>%
  mutate(EXCLUDE = case_when(str_detect(string = TEXT, pattern=regex("(?<![a-zA-Z])retinopathy|optic nerve damage|visual complaints(?![a-zA-z])", ignore_case = TRUE)) ~ 1, TRUE ~ 0)) %>%
  filter(EXCLUDE == 1) %>%
  select(NOTE_ID)

# Update these notes to reverse our original prediction
retinopathy_notes %<>%
  mutate(diabetic_retinopathy_pred = case_when(NOTE_ID %in% retinopathy_notes_to_exclude$NOTE_ID ~ 0, TRUE ~ diabetic_retinopathy_pred))

# Check final accuracy
retinopathy_goldStd %>%
  inner_join(retinopathy_notes) %>% 
  collect()%>%
  filter(DIABETIC_RETINOPATHY != diabetic_retinopathy_pred)

# Record 16 shows up as diabetic and is incorrectly flagged by us, 
# but it is not evident even from reading the notes that this patient has any diabetic complications

retinopathy_notes %<>%
  select(NOTE_ID, diabetic_retinopathy_pred)

# 2. Nephropathy
nephropathy_notes <- diabetes_notes %>%  
  select(NOTE_ID, TEXT, diabetic_nephropathy_pred) %>%
  collect()

nephropathy_notes

# Check nephropathy accuracy
nephropathy_goldStd <- goldStd %>%
  select(NOTE_ID, DIABETIC_NEPHROPATHY) %>%
  collect()

nephropathy_goldStd

# Check accuracy
nephropathy_goldStd %>%
  inner_join(nephropathy_notes) %>% 
  collect()%>%
  filter(DIABETIC_NEPHROPATHY != diabetic_nephropathy_pred)

# 6 notes were misclassified. Let's manually review these

# Add "renal disease" to search criteria - notes 13, 27 (renal disease after a long history of poorly controlled DM - 13, )
# "can cause.....nephropathy - note 41. (LATER TO DO - Exclude via windowing techniques)
# Exclude "reflux nephropathy" as this is not a complication due to diabetes - note 82
# Include "renal insufficiency secondary to DM" - note 85
# Include renal disease (ESRD) secondary to diabetes - note 108


# Update algorithm for nephropathy

nephropathy_notes %<>% 
  mutate(diabetic_nephropathy_pred = case_when(str_detect(TEXT, regex(pattern = "renal (disease|insufficiency) .* (DM|diabetes)", ignore_case = TRUE)) ~ 1, TRUE ~ diabetic_nephropathy_pred)) %>%
  mutate(diabetic_nephropathy_pred = case_when(str_detect(TEXT, regex(pattern = "can cause .* nephropathy|normal rectal|reflux nephropathy", ignore_case = TRUE)) ~ 0, TRUE ~ diabetic_nephropathy_pred)) %>%
  collect()

nephropathy_notes

# Check accuracy again
nephropathy_goldStd %>%
  inner_join(nephropathy_notes) %>% 
  collect()%>%
  filter(DIABETIC_NEPHROPATHY != diabetic_nephropathy_pred)

# All notes are now classified correctly
nephropathy_notes %<>%
  select(NOTE_ID, diabetic_nephropathy_pred)

# 3. Neuropathy
neuropathy_notes <- diabetes_notes %>%  
  select(NOTE_ID, TEXT, diabetic_neuropathy_pred) %>%
  collect()

neuropathy_goldStd <- goldStd %>%
  select(NOTE_ID, DIABETIC_NEUROPATHY) %>%
  collect()

neuropathy_goldStd

# Check neuropathy accuracy
neuropathy_goldStd %>%
  inner_join(neuropathy_notes) %>% 
  collect()%>%
  filter(DIABETIC_NEUROPATHY != diabetic_neuropathy_pred)

# 17 notes misclassified

# Exclude denies conditions
neuropathy_notes %<>% 
  mutate(diabetic_neuropathy_pred = case_when(str_detect(TEXT, regex(pattern = "((no history of)|(no mention of)|(no problems with)|denies) (diabetes|neuropathy|tingling|numbness|numb |(nerve pain))", ignore_case = TRUE)) ~ 0, TRUE ~ diabetic_neuropathy_pred)) %>%
  mutate(diabetic_neuropathy_pred = case_when(str_detect(TEXT, regex(pattern = "(no numbness)|(without tingling)", ignore_case = TRUE)) ~ 0, TRUE ~ diabetic_neuropathy_pred)) %>%
  collect()

# Check neuropathy accuracy
neuropathy_goldStd %>%
  inner_join(neuropathy_notes) %>% 
  collect()%>%
  filter(DIABETIC_NEUROPATHY != diabetic_neuropathy_pred)

# 14 notes are misclassified. Let's do a manual review

# 3: Cannot detect
# 21: denies any comorbid complications of the diabetes including kidney disease, heart disease, stroke, vision loss, or neuropathy
# 26: Cannot detect, unless we change the algorithm to first find diabetes then neuropathy. No mention of diabetes
# 33: no history of hypertension, diabetes...
# 38: Cannot detect
# 43: no problems with edema or lower extremity numbness or tingling
# 48: denies any numbness or tingling
# 64: denies bowel or bladder dysfunction, saddle area hypoesthesia, numbness, tingling, weakness
# 73: Cannot detect
# 82: Cannot detect (neuropathy does not have to do with diabetes) 
# 86: Cannot detect
# 114: Denies blackouts, seizures, loss of memory, hallucinations, weakness, numbness
# 132: No weakness, numbness or tingling
# 138: Due to "carpal tunnel syndrome"

# We find that there are notes in which neuropathy is still negated.
# Use windowing to isolate those notes that have the negation within 8 words before the neuropathy condition

# The function below has been adapted from the Week 4 function extract_text_window 
# Given a dataframe, keyword and window size, it will:
# (i) Extract all text windows of size 8 before and upto every word
# (ii) Filter the text windows to return only those that contain the keyword (which may be a regex pattern)
extract_text_leading_window <- function(dataframe, keyword, window_size) {
  dataframe %>% 
    group_by(NOTE_ID) %>% 
    mutate(WORDS = TEXT) %>% 
    separate_rows(WORDS, sep = "[ \n]+") %>% 
    mutate(INDEX = seq(from = 1, to = n(), by = 1.0),
           WINDOW_START = case_when(INDEX - window_size < 1 ~ 1,
                                    TRUE ~ INDEX - window_size),
           WINDOW = word(string = TEXT, start = WINDOW_START, end = INDEX, sep = "[ \n]+")) %>% 
    ungroup() %>% 
    filter(str_detect(string = WORDS, pattern = regex(keyword, ignore_case = TRUE)))
}

# Use windowing to get the list of notes that contain negation of neuropathy 
neuropathy_notes_to_update <- neuropathy_notes %>%
  extract_text_leading_window("diabetes|(neuopathic pain)|neuropathy|tingling|numbness|numb|(nerve pain)", 8) %>%
  filter(diabetic_neuropathy_pred == 1) %>%
  filter(str_detect(string = WINDOW, pattern = regex("no |denies ", ignore_case = TRUE))) %>%
  distinct(NOTE_ID)

neuropathy_notes_to_update

# Update these notes to reverse our original prediction
neuropathy_notes %<>%
  mutate(diabetic_neuropathy_pred = case_when(NOTE_ID %in% neuropathy_notes_to_update$NOTE_ID ~ 0, TRUE ~ diabetic_neuropathy_pred))

# Check neuropathy accuracy
neuropathy_goldStd %>%
  inner_join(neuropathy_notes) %>% 
  collect()%>%
  filter(DIABETIC_NEUROPATHY != diabetic_neuropathy_pred)

neuropathy_notes %<>%
  select(NOTE_ID, diabetic_neuropathy_pred)

# 7 notes are still misclassified. See results from manual review above.
# These could not be detected by simple text processing and will require domain expertise
# One note, NOTE_ID = 97 which was earlier classified correctly, is now misclassified
# This is because it was detected via windowing to contain denial of neuropathy.
# However in fact, the denial did not relate to neuropathy

# Combine predictions across the 3 conditions to yield our final prediction dataset

diabetes_notes_pred <- neuropathy_notes %>%
  inner_join(nephropathy_notes, by = 'NOTE_ID') %>%
  inner_join(retinopathy_notes, by = 'NOTE_ID') %>%
  mutate(any_diabetic_complication_pred = case_when(diabetic_neuropathy_pred == 1 | diabetic_nephropathy_pred == 1 | diabetic_retinopathy_pred == 1 ~ 1, TRUE ~ 0))

diabetes_notes_pred

# Final accuracy check

goldStd %>%
  collect()%>%
  inner_join(diabetes_notes_pred) %>% 
  filter(ANY_DIABETIC_COMPLICATION != any_diabetic_complication_pred)

# 7 notes misclassified altogether