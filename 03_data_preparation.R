load(".RData")

library(tidyverse)
library(sjmisc)

d_coded_comments <- bind_cols(qwen_ageism, 
                              qwen_bodyshaming %>% select(id, bodyshaming), 
                              qwen_conspiracy %>% select(id, conspiracy), 
                              qwen_criminalization %>% select(id, criminalization), 
                              qwen_dehumanizing %>% select(id, dehumanizing), 
                              qwen_insults %>% select(id, insults), 
                              qwen_patronizing %>% select(id, patronizing), 
                              qwen_political %>% select(id, political), 
                              qwen_sexism %>% select(id, sexism), 
                              qwen_threatsaggression %>% select(id, threatsaggression), 
                              .name_repair = "unique")
id_cols <- d_coded_comments[ , c("id...2","id...14","id...16","id...18","id...20",
                                 "id...22","id...24","id...26","id...28","id...30")]

all(apply(id_cols, 1, function(x) length(unique(x)) == 1)) # TRUE

# remove redundant ids 
d_coded_comments <- d_coded_comments %>% rename(id = id...2) %>% select(-contains("..."))

# make nchars for qwen codes 


library(dplyr)

vars <- c("ageism", "bodyshaming", "conspiracy", "criminalization",
          "dehumanizing", "insults", "patronizing", "political",
          "sexism", "threatsaggression")

d_coded_comments <- d_coded_comments %>%
  mutate(across(all_of(vars), nchar, .names = "{.col}_nchar"))

d_coded_comments %>% select(ends_with("_nchar")) %>% summary() # all need cleaning 


# extract numbers from codes

extract_numbers <- function(x) {
  matches <- str_extract_all(x, "[0-9]+")   # list of all digit-runs per string, in order
  out <- sapply(matches, paste, collapse = "_")
  out[is.na(x)] <- NA                        # preserve true NAs instead of turning them into ""
  out
}

d_coded_comments <- d_coded_comments %>%
  mutate(across(all_of(vars), extract_numbers, .names = "{.col}_nums"))


d_coded_comments %>% 
  select(ends_with("_nums")) %>%
  sjmisc::frq(min.frq = 50, out = "viewer")
