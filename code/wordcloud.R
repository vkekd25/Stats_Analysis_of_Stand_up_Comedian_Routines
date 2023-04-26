# Guide used: https://www.pluralsight.com/guides/visualization-text-data-using-word-cloud-r
library(readr)
library(dplyr)
library(e1071)
library(mlbench)

library(tm)
library(SnowballC) # 
library(wordcloud)
library(RColorBrewer) # colors

# load the dataset
complete_text_demo_df <- read.csv("clean_complete_text_demo_df.csv")
glimpse(complete_text_demo_df)

# text will be the transcript vector of the data frame
text <- complete_text_demo_df$transcript_df...3.
head(complete_text_demo_df)

# replace special characters like curly quotations, etc. with an apostrophe (probably not needed since that will be
# truncated via stemDocument later on)
# text <- gsub("[\u2018\u2019\u201A\u201B\u2032\u2035\u201C\u201D\u2013\u2026]", "'", text)

# process of creating corpus for text (explanations for each step on the website link)
corpus <- Corpus(VectorSource(text))
#corpus <- tm_map(corpus, stemDocument)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, PlainTextDocument)
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, removeNumbers)

# convert to document term matrix for creating frequency data
DTM <- TermDocumentMatrix(corpus)
mat <- as.matrix(DTM)
f <- sort(rowSums(mat), decreasing = TRUE)
dat <- data.frame(word = names(f), freq = f)

# see top 30 words used
head(dat, 30)

# create wordcloud for top 200 words
# something to note is that u266a shows up in the wordcloud as a frequent word;
# it's unicode for an eighth note (haven't removed it yet)
pal = brewer.pal(9,"Dark2")
wordcloud::wordcloud(words = dat$word, freq = dat$freq, max.words = 200, random.order = FALSE, colors = pal)

# subset sexual orientation
complete_text_demo_df$sexual.orientation
straight_comedians <- subset(complete_text_demo_df, sexual.orientation == "straight")
nostraight_comedians <- subset(complete_text_demo_df, sexual.orientation == "female loving" | sexual.orientation == "lesbian" | sexual.orientation == "bisexual" | sexual.orientation == "homosexual")

# for straight_comedians
text <- straight_comedians$transcript_df...3.

corpus <- Corpus(VectorSource(text))
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, PlainTextDocument)
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, removeNumbers)

DTM <- TermDocumentMatrix(corpus)
mat <- as.matrix(DTM)
f <- sort(rowSums(mat), decreasing = TRUE)
dat1 <- data.frame(word = names(f), freq = f)

pal = brewer.pal(9,"Dark2")
wordcloud::wordcloud(words = dat1$word, freq = dat1$freq, max.words = 100, random.order = FALSE, colors = pal)

# for nostraight_comedians
text <- nostraight_comedians$transcript_df...3.

corpus <- Corpus(VectorSource(text))
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, PlainTextDocument)
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, removeNumbers)

DTM <- TermDocumentMatrix(corpus)
mat <- as.matrix(DTM)
f <- sort(rowSums(mat), decreasing = TRUE)
dat2 <- data.frame(word = names(f), freq = f)

pal = brewer.pal(9,"Dark2")
wordcloud::wordcloud(words = dat2$word, freq = dat2$freq, max.words = 200, random.order = FALSE, colors = pal)

# https://bookdown.org/Maxine/tidy-text-mining/compare-word-frequency.html(compare word frequency)
# Compare word frequency for sexual orientation
total <- dat %>%
  mutate(sexual.orientation = "total")
straight <- dat1 %>%
  mutate(sexual.orientation = "straight")
nostraight <- dat2 %>%
  mutate(sexual.orientation = "nostraight")

so <- bind_rows(total, straight, nostraight)

library(tidyr)
comparison_df <- so %>%
  add_count(sexual.orientation, wt = freq, name = "total_word") %>%
  mutate(proportion = freq / total_word) %>%
  select(-total_word, -freq) %>%
  pivot_wider(names_from = sexual.orientation, values_from = proportion, values_fill = list(proportion = 0)) %>%
  pivot_longer(3:4, names_to = "other", values_to = "proportion")

comparison_df

library(ggplot2)
library(scales)

comparison_df %>% 
  filter(proportion > 1 / 1e5) %>% 
  ggplot(aes(proportion, `total`)) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(aes(color = abs(`total` - proportion)),
              alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) + 
  scale_x_log10(labels = label_percent()) +
  scale_y_log10(labels = label_percent()) + 
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") + 
  facet_wrap(~ other) + 
  guides(color = FALSE)

# a simple correlation test
cor.test(data = filter(comparison_df, other == "nostraight"),
         ~ proportion + `total`)
cor.test(data = filter(comparison_df, other == "straight"),
         ~ proportion + `total`)

