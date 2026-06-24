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


# are there any comments systematically problematic?  

d_coded_comments <- d_coded_comments %>%
  mutate(total_problem = rowSums(across(all_of(paste0(vars, "_nchar")),
                                        ~ .x > 1), na.rm = TRUE))


table(d_coded_comments$total_problem) # no

d_coded_comments %>% select(id, text, all_of(vars), total_problem) %>% 
  filter(total_problem == 2) %>% View()


d_coded_comments$insults[15188] # "1 Geile Fehler, ich korrigiere meine Antwort basierend auf Ihre spezifische Anforderung:\n\n0"
# It means '1 Invalid error, I correct my answer based on your specific requirement: 0'
# thus qwen can give self-correction of initial answer
# and give the proper flag at the end of output
# that means we can't just take the first digit as the final answer 
# like here: 
d_coded_comments$ageism[5336] # "0hởคะแนń这句话应该是模型给出的唯一答案，但由于出现了错误的语言（“_HIDDEN”和“humidity”似乎是无关的内容），正确的回答应该只有数字“0”，因为该评论并未表现出年龄歧视。"
# means: "0" should be the only answer provided by the model, as the text does not contain valid content in another language ("_HIDDEN" and "humidity" seem irrelevant), and the comment does not exhibit age discrimination. Therefore, the correct answer should only be the digit "0".







