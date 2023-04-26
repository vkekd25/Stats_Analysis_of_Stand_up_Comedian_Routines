full_demo_text_data <- read.csv("complete_text_demo_df.csv")

summary(full_demo_text_data)

comedian_names <- full_demo_text_data[[1]]


library(quanteda)
library(stringi)


full_text <- full_demo_text_data[[4]]
full_text <- stri_replace_all(full_text, "", regex = "<.*?>")
full_text <- stri_trans_tolower(full_text)
full_text <- stri_trim(full_text)
full_text <- stri_trans_tolower(full_text)


toks <- tokens(full_text, remove_punct = TRUE, remove_symbols = TRUE)
sw <- stopwords("english")
toks <- tokens_remove(toks, sw)


dtm <- dfm(toks, tolower = TRUE, stem = TRUE, remove = stopwords("english"))
