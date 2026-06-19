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

ageism_prompt_strict <- "Jesteś klasyfikatorem, zawsze odpowiadasz wyłącznie jedną cyfrą: 1 albo 0. Zaklasyfikuj następujący komentarz zamieszczony pod wideo na YouTube jako wyrażający lub nie wyrażający ageizm, czyli uprzedzenia wobec osoby lub grupy ludzi z uwagi na jej czy ich wiek. Przykładowe wyrażenia wskazujące na ageizm podane są w nawiasie (młodzi odklejeńcy, dzieciaki, gówniarze i narkomani, dzieci do szkoły, stary człowieczek, młody odklejeniec, młody szczoch, stary PRL komuch, chłopcy z piaskownicy, wiekowy człowiek a sieczka w mózgu, młoda idiotka, skretyniałe staruchy, głupie dzieci, 20-letnie dziecko; młode matoły bez przyszłości; gdzie są ich rodzice; współczuję rodzicom tej nieszczęśnicy; małe dzieci; pannica; panienka; dzieciątka; dzidzia)\n. Odpowiedz, podając wyłącznie jedną cyfrę 1, wtedy i tylko wtedy, jeżeli komentarz wyraża ageizm, zgodnie z podaną definicją. Odpowiedz 0, jeżeli tego nie wyraża. Jeżeli komentarz wyraża inne rodzaje mowy nienawiści, ale nie ageism, odpowiedz 0. Nie udzielaj żadnych innych odpowiedzi poza 1 albo 0 ani nie uzasadniaj wykonanej klasyfikacji\n. Komentarz: "


role_test_qwen_ageism_comments <- classify_with_timing(
  df              = role_test_qwen_ageism_comments,
  text_col        = "text",
  new_col         = "ageism_strict",
  prompt_template = ageism_prompt_strict,
  model           = model_qwen
) # 

role_test_qwen_ageism_comments <- role_test_qwen_ageism_comments %>%
  mutate(char_ageism_strict = nchar(ageism_strict))
summary(role_test_qwen_ageism_comments$char_ageism_strict) # wow 

role_test_qwen_ageism_comments %>% count(ageism_strict)

role_test_qwen_ageism_comments %>% select(text, ageism_strict) %>% 
  filter(ageism_strict == 1) %>% View 

aps <- "Jesteś klasyfikatorem, zawsze odpowiadasz wyłącznie jedną cyfrą: 1 albo 0. Zaklasyfikuj następujący komentarz zamieszczony pod wideo na YouTube jako wyrażający lub nie wyrażający ageizm, czyli uprzedzenia wobec osoby lub grupy ludzi z uwagi na jej czy ich wiek. Przykładowe wyrażenia wskazujące na ageizm podane są w nawiasie (młodzi odklejeńcy, dzieciaki, gówniarze i narkomani, dzieci do szkoły, stary człowieczek, młody odklejeniec, młody szczoch, stary PRL komuch, chłopcy z piaskownicy, wiekowy człowiek a sieczka w mózgu, młoda idiotka, skretyniałe staruchy, głupie dzieci, 20-letnie dziecko; młode matoły bez przyszłości; gdzie są ich rodzice; współczuję rodzicom tej nieszczęśnicy; małe dzieci; pannica; panienka; dzieciątka; dzidzia)\n. Odpowiedz, podając wyłącznie jedną cyfrę 1, wtedy i tylko wtedy, jeżeli komentarz wyraża ageizm, zgodnie z podaną definicją. Odpowiedz 0, jeżeli tego nie wyraża. Jeżeli komentarz wyraża inne rodzaje mowy nienawiści, ale nie ageizm, odpowiedz 0. Nie udzielaj żadnych innych odpowiedzi poza 1 albo 0 ani nie uzasadniaj wykonanej klasyfikacji\n. Komentarz: "


# aps == ageism_prompt_strict # typo ageism / ageizm

# ageism qwen full job #### 

qwen_ageism <- classify_with_timing(
  df              = comments,
  text_col        = "text",
  new_col         = "ageism",
  prompt_template = aps,
  model           = model_qwen
) # Done! 30574 comments in 15257.5s (0.5s per comment)


# save.image()

sexism_prompt <- "Jesteś klasyfikatorem, zawsze odpowiadasz wyłącznie jedną cyfrą: 1 albo 0. Zaklasyfikuj następujący komentarz zamieszczony pod wideo na YouTube jako wyrażający lub nie wyrażający seksizm, czyli jawną dewaluację kobiet lub podporządkowywanie kobiet mężczyznom, a także promowanie negatywnych stereotypów dotyczących kobiet. Przykładowe wyrażenia wskazujące na seksizm podane są w nawiasie (blond włosa manipulantka - o mężczyźnie, rozpieszczone cioty, głupie dziewki, współczuję rodzicom, Julka; Julcia; Julia; julka; julcia; julia; Greta; kolejna Greta; osoba aktywistyczna; pannica; panienka; osobopostać; Gretka; dziewczynka; ono; brakuje jej porządnego bolca i dostaje pierdolca; cioty; chłopa jej trzeba; lalunia; srajda; do burdelu; osoby ekologiczne)\n. Odpowiedz, podając wyłącznie jedną cyfrę 1, wtedy i tylko wtedy, jeżeli komentarz wyraża seksizm, zgodnie z podaną definicją. Odpowiedz 0, jeżeli tego nie wyraża. Jeżeli komentarz wyraża inne rodzaje mowy nienawiści, ale nie seksizm, odpowiedz 0. Nie udzielaj żadnych innych odpowiedzi poza 1 albo 0 ani nie uzasadniaj wykonanej klasyfikacji\n. Komentarz: "


qwen_sexism <- classify_with_timing(
  df              = comments,
  text_col        = "text",
  new_col         = "sexism",
  prompt_template = sexism_prompt,
  model           = model_qwen
) # Done! 30574 comments in 15775.7s (0.5s per comment)

# save.image()

bodyshaming_prompt <- "Jesteś klasyfikatorem, zawsze odpowiadasz wyłącznie jedną cyfrą: 1 albo 0. Zaklasyfikuj następujący komentarz zamieszczony pod wideo na YouTube jako wyrażający lub nie wyrażający body shaming, czyli nieproszoną, zazwyczaj negatywną opinię o czyimś ciele; intencją osoby wyrażającej opinię nie musi być skrzywdzenie osoby, o której się wypowiada. Przykładowe wyrażenia wskazujące na body shaming podane są w nawiasie (tępa blondyna, co ona ma takie gały, blondi, tłuste włosy; jesteście brzydkie; a w arafatce to po ustach widać, że nie tyko siedzi ale całuje się z asfaltem; tleniony; farbowany)\n. Odpowiedz, podając wyłącznie jedną cyfrę 1, wtedy i tylko wtedy, jeżeli komentarz wyraża body shaming, zgodnie z podaną definicją. Odpowiedz 0, jeżeli tego nie wyraża. Jeżeli komentarz wyraża inne rodzaje mowy nienawiści, ale nie body shaming, odpowiedz 0. Nie udzielaj żadnych innych odpowiedzi poza 1 albo 0 ani nie uzasadniaj wykonanej klasyfikacji\n. Komentarz: "

qwen_bodyshaming <- classify_with_timing(
  df              = comments,
  text_col        = "text",
  new_col         = "bodyshaming",
  prompt_template = bodyshaming_prompt,
  model           = model_qwen
) # Done! 30574 comments in 15389.4s (0.5s per comment)


# save.image()

insults_prompt <- "Jesteś klasyfikatorem, zawsze odpowiadasz wyłącznie jedną cyfrą: 1 albo 0. Zaklasyfikuj następujący komentarz zamieszczony pod wideo na YouTube jako wyrażający lub nie wyrażający obelgi, czyli wypowiedź intencjonalnie nakierowaną na obrazę osoby, do której się odnosi, poprzez celowe jej poniżanie lub przypisywanie jej cech negatywnych; może odnosić się do cech jednostkowych lub cech ogólnych grupy osób. Przykładowe wyrażenia wskazujące na obelgi podane są w nawiasie (błazny, niepełnosprawni, pojeby, gnoje, pajace, tiktokowe przygłupy, lenie, idioci, pokolenie tępaków, nieroby, banda szumowin, zryte psychiki, nie powinni się rozmnażać, imbecyl, chory umysłowo, oszołomy, niekumaci, puste głowy, badania psychiatryczne, darmozjady, psychiczne nieroby, świry, dziadostwo, hołota, głąby, kanalie, cwaniaki, sztywni uparci, sekciarze, bełkot aktywisty, miernoty, ameby umysłowe, matoły, szambo w głowie, kretynka, hipokrytka, wariatka, hołota, Nawet w Choroszczy nie ma takich pacjentów; chorzy ludzie; kwalifikują się do psychiatry; ułomna umysłowo; Ale Tworki pewnie są pełne takich pensjonariuszy; rozjechany procesor; tacy ludzie chodzą po ulicach bez opieki; mądrość je goniła ale zawsze były szybsze; Świecie, Tworki itd witają; ostatnie odklejenie)\n. Odpowiedz, podając wyłącznie jedną cyfrę 1, wtedy i tylko wtedy, jeżeli komentarz wyraża obelgi, zgodnie z podaną definicją. Odpowiedz 0, jeżeli tego nie wyraża. Jeżeli komentarz wyraża inne rodzaje mowy nienawiści, ale nie obelgi, odpowiedz 0. Nie udzielaj żadnych innych odpowiedzi poza 1 albo 0 ani nie uzasadniaj wykonanej klasyfikacji\n. Komentarz: "

qwen_insults <- classify_with_timing(
  df              = comments,
  text_col        = "text",
  new_col         = "insults",
  prompt_template = insults_prompt,
  model           = model_qwen
) # Done! 30574 comments in 15589.8s (0.5s per comment) 


# save.image()

threatsaggression_prompt <- "Jesteś klasyfikatorem, zawsze odpowiadasz wyłącznie jedną cyfrą: 1 albo 0. Zaklasyfikuj następujący komentarz zamieszczony pod wideo na YouTube jako wyrażający lub nie wyrażający groźby/agresję, czyli jawną zapowiedź lub propozycję użycia przemocy fizycznej lub psychicznej; także nawoływanie do przemocy. Przykładowe wyrażenia wskazujące na groźby/agresję podane są w nawiasie (rury wydechowe w ich kierunku, rozjeżdżanie tych terrorystów, przejechać ich, potrącić ich, lać ich, Kubeł z moczem i kałem na łeb, Sprzedać im buta w ryj, potraktować gazem pieprzowym, przywiązać do latarni, duszenie zapaśnicze zrobić, odrąbać grabie, do wora i do wody, pałą ich, pieprzowy gaz, zimna woda, śmierdzący spryskiwacz, stare jaja, niech się wyloguje z życia; naprawdę są ostanim pokoleniem; nie będą się rozmnażać bo są ostatnim pokoleniem; Jak tak patrzę na to ostatnie pokolenie, to jestem za aborcją; powinni siedzieć; 8 lat więzienia to za mało; pokój bez klamek; puścić na nich kibiców zamiast policji; proponuję wiadro butaprenu; Jak się przykleili to można ich też zgwałcić he he; Czy ostatnie pokolenie jest wysterylizowane?; )\n. Odpowiedz, podając wyłącznie jedną cyfrę 1, wtedy i tylko wtedy, jeżeli komentarz wyraża groźby/agresję, zgodnie z podaną definicją. Odpowiedz 0, jeżeli tego nie wyraża. Jeżeli komentarz wyraża inne rodzaje mowy nienawiści, ale nie groźby/agresję, odpowiedz 0. Nie udzielaj żadnych innych odpowiedzi poza 1 albo 0 ani nie uzasadniaj wykonanej klasyfikacji\n. Komentarz: " 


qwen_threatsaggression <- classify_with_timing(
  df              = comments,
  text_col        = "text",
  new_col         = "threatsaggression",
  prompt_template = threatsaggression_prompt,
  model           = model_qwen
) # Done! 30574 comments in 17889.6s (0.6s per comment)

# save.image()

dehumanizing_prompt <- "Jesteś klasyfikatorem, zawsze odpowiadasz wyłącznie jedną cyfrą: 1 albo 0. Zaklasyfikuj następujący komentarz zamieszczony pod wideo na YouTube jako wyrażający lub nie wyrażający dehumanizację, czyli określanie ludzi mianem zwierząt lub przedmiotów nieożywionych. Przykładowe wyrażenia wskazujące na dehumanizację podane są w nawiasie (to to, śmieci, szkodniki, łajzy, ścierwa, bezmózgi, ściery, na śmietnik, pasożyci, ścieki nie ludzie, ameby, gówna, odpady, zmanipulowane ameby, cyrk, szczury, dzicz, sekciarstwo, To coś nie nadaje się do życia w społeczeństwie; monstra; to coś; aktywiszcze; mięsne progi zwalniające)\n. Odpowiedz, podając wyłącznie jedną cyfrę 1, wtedy i tylko wtedy, jeżeli komentarz wyraża dehumanizację, zgodnie z podaną definicją. Odpowiedz 0, jeżeli tego nie wyraża. Jeżeli komentarz wyraża inne rodzaje mowy nienawiści, ale nie dehumanizację, odpowiedz 0. Nie udzielaj żadnych innych odpowiedzi poza 1 albo 0 ani nie uzasadniaj wykonanej klasyfikacji\n. Komentarz: "


qwen_dehumanizing <- classify_with_timing(
  df              = comments,
  text_col        = "text",
  new_col         = "dehumanizing",
  prompt_template = dehumanizing_prompt,
  model           = model_qwen
) # 
