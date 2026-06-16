library(tidyverse)

# list of videos #### 
videolist_full <- read_csv("videolist_search1125_2026_04_10-10_22_15.csv")

videolist <- videolist_full %>% filter(videoId %in% c("XVv1RiQ4YVk",
                                                      "sO5HcLGDJXY",
                                                      "f86tbIrUJ7w",
                                                      "dw5JnGAvjcc",
                                                      "RqkxDg0HWVQ",
                                                      "R2w2bAbtzkc",
                                                      "qIMhz-wZXbs",
                                                      "qWTLvtdGVAo")) # 8 videos for comments analysis


# bind comments from 8 videos #### 

comments_files <- list.files(pattern = "^videoinfo_.*\\.csv$", full.names = TRUE)

comments_na <- comments_files  %>% 
  set_names() %>%
  map(read_csv) %>%
  list_rbind(names_to = "source_file") %>%
  mutate(
    videoId = str_extract(source_file, "(?<=videoinfo_)[^_]+(?=_\\d{4}_)"),
    .before = 1
  ) %>%
  select(-source_file)

# drop empty comments #### 

comments <- comments_na %>% filter(!is.na(text))

count_comments <- left_join(x = videolist %>% select(videoId, commentCount) %>% arrange(-commentCount), 
          y = comments %>% count(videoId) %>% arrange(-n)) # there are small differences 

count_comments <- count_comments %>% 
  rename(comm_n_videolist = commentCount, 
         comm_n_file = n) %>% 
  mutate(difference = comm_n_file - comm_n_videolist)


