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
    response <- chat$chat(prompt)
    regmatches(response, regexpr("[01]", response)) # to avoid <|im_sep|> in responses
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

# ageism_comments_test_10 <- classify_with_timing( # output has <|im_sep|>, function updated
#   df              = comments[1:10, ],
#   text_col        = "text",
#   new_col         = "ageism",
#   prompt_template = ageism_prompt,
#   model           = bielik_model
# )

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
)
