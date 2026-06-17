source("01_data_load.R")

library(purrr)
library(ellmer)

# run `ollama serve` in terminal first! 
# SpeakLeash/bielik-11b-v2.3-instruct:Q6_K model is used 

bielik_model <- "SpeakLeash/bielik-11b-v2.3-instruct:Q6_K"

# classifying function #### 

classify_comment <- function(comment_text, prompt_template, model) {
  tryCatch({
    chat <- chat_ollama(model = model)
    prompt <- paste0(prompt_template, comment_text)
    chat$chat(prompt)
  },
  error = function(e) NA_character_
  )
}

classify_with_timing <- function(df, text_col, new_col, prompt_template, model) {
  n <- nrow(df)
  start_time <- proc.time()
  
  result <- df %>%
    mutate(!!new_col := map_chr(seq_len(n), function(i) {
      cli::cli_progress_message("Processing comment {i}/{n}...")
      classify_comment(
        comment_text    = .data[[text_col]][i],
        prompt_template = prompt_template,
        model           = model
      )
    }))
  
  elapsed <- proc.time() - start_time
  cli::cli_alert_success("Done! {n} comments in {round(elapsed['elapsed'], 1)}s ({round(elapsed['elapsed']/n, 1)}s per comment)")
  
  result
}



# Ageism #### 

ageism_prompt <- "Zaklasyfikuj następujący komentarz zamieszczony pod wideo na YouTube jako wyrażający lub nie wyrażający ageizm, czyli uprzedzenia wobec osoby lub grupy ludzi z uwagi na jej czy ich wiek. Przykładowe wyrażenia wskazujące na ageizm podane są w nawiasie (młodzi odklejeńcy, dzieciaki, gówniarze i narkomani, dzieci do szkoły, stary człowieczek, młody odklejeniec, młody szczoch, stary PRL komuch, chłopcy z piaskownicy, wiekowy człowiek a sieczka w mózgu, młoda idiotka, skretyniałe staruchy, głupie dzieci, 20-letnie dziecko; młode matoły bez przyszłości; gdzie są ich rodzice; współczuję rodzicom tej nieszczęśnicy; małe dzieci; pannica; panienka; dzieciątka; dzidzia)\n. Odpowiedz, podając wyłącznie jedną cyfrę 1, jeżeli komentarz wyraża ageizm, albo 0, jeżeli tego nie wyraża. Nie udzielaj żadnych innych odpowiedzi ani nie uzasadniaj wykonanej klasyfikacji\n. Komentarz: "



# ageism_comments_test_10 <- comments[1:10, ] %>%
#   mutate(ageism = map_chr(seq_len(n), function(i) {
#     start_time <- proc.time()
#     cli::cli_progress_message("Processing comment {i}/{n}...")
#     classify_comment(comment_text = text[i],
#                      prompt_template = ageism_prompt,
#                      model = bielik_model)
#     elapsed <- proc.time() - start_time
#     cli::cli_alert_success("Done! {n} comments in {round(elapsed['elapsed'], 1)}s ({round(elapsed['elapsed']/n, 1)}s per comment)")
#   }))

# ageism_comments_test_10 <- classify_with_timing( 

#   df              = comments[1:10, ],
#   text_col        = "text",
#   new_col         = "ageism",
#   prompt_template = ageism_prompt,
#   model           = bielik_model
# )


# ageism_comments_test_10 output has <|im_sep|>, function updated 
# but for error handling eventually removed from the function :) 
# TODO use it for data cleaning regmatches(response, regexpr("[01]", response)) # to avoid <|im_sep|> in responses

ageism_comments_test_20 <- classify_with_timing(
  df              = comments[20:30, ],
  text_col        = "text",
  new_col         = "ageism",
  prompt_template = ageism_prompt,
  model           = bielik_model
) # works well but if did 11 comments in 8.6s (0.8s per comment), full dataset will be ~7 h

nrow(comments) * 0.8 / 60 / 60

# full job 
ageism_comments <- classify_with_timing(
  df              = comments,
  text_col        = "text",
  new_col         = "ageism",
  prompt_template = ageism_prompt,
  model           = bielik_model
) #  Done! 30574 comments in 44477.9s (1.5s per comment)

# save.image()

ageism_comments %>% count(ageism) %>% arrange(-n) %>% print(n = 1000)

ageism_comments <- ageism_comments %>% mutate(char_ageism = nchar(ageism))
summary(ageism_comments$char_ageism)

# smaller model test 

bielik_model_45 <- "SpeakLeash/bielik-4.5b-v3.0-instruct:Q8_0"

test_45_ageism_comments <- classify_with_timing(
  df              = comments[1000:1050, ],
  text_col        = "text",
  new_col         = "ageism",
  prompt_template = ageism_prompt,
  model           = bielik_model_45
) # Done! 51 comments in 92s (1.8s per comment)

# some are extremely long? 

test_45_ageism_comments <- test_45_ageism_comments %>% mutate(char_ageism = nchar(ageism))
summary(test_45_ageism_comments$char_ageism)

# new prompt start - role for model 

role_ageism_prompt <- paste("Jesteś klasyfikatorem, zawsze odpowiadasz wyłącznie jedną cyfrą: 1 albo 0.", 
                            ageism_prompt)


role_test_45_ageism_comments <- classify_with_timing(
  df              = comments[1000:1050, ],
  text_col        = "text",
  new_col         = "ageism",
  prompt_template = role_ageism_prompt,
  model           = bielik_model_45
) # Done! 51 comments in 54.3s (1.1s per comment)

# some are extremely long? 

role_test_45_ageism_comments <- role_test_45_ageism_comments %>% mutate(char_ageism = nchar(ageism))
summary(role_test_45_ageism_comments$char_ageism)


model_qwen <- "qwen2.5:14b"

role_test_qwen_ageism_comments <- classify_with_timing(
  df              = comments[1000:1050, ],
  text_col        = "text",
  new_col         = "ageism",
  prompt_template = role_ageism_prompt,
  model           = model_qwen
) # Done! 51 comments in 29s (0.6s per comment) / fast! /

role_test_qwen_ageism_comments <- role_test_qwen_ageism_comments %>% mutate(char_ageism = nchar(ageism))
summary(role_test_qwen_ageism_comments$char_ageism) # wow 

role_test_qwen_ageism_comments %>% count(ageism)

role_test_qwen_ageism_comments %>% select(text, ageism) %>% 
  filter(ageism == 1) %>% View 
# quite well but too sensitive for general hate speech, some of them are not ageism
# TODO update prompt to be more strict to ageism 
