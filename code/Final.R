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

generation <- rep(NA, 343)

generation[cleaned_text_df$age <= 40] <- "Millenial"
generation[cleaned_text_df$age >= 41 & cleaned_text_df$age <= 56] <- "Generation X"
generation[cleaned_text_df$age >= 57 & cleaned_text_df$age <= 75] <- "Baby Boomer"
generation[cleaned_text_df$age >= 76] <- "Silent Generation"

which(is.na(generation) == TRUE)
cleaned_text_df$b_year[which(is.na(generation) == TRUE)]

generation[c(35:37,286:287)] <- "Baby Boomer"
generation[c(102,272:276)] <- "Silent Generation"
generation[c(248,268)] <- "Generation X"

cleaned_text_df <- cbind(cleaned_text_df, generation = generation)


##### example with Generations

# compare two groups via subsetting
millenial_comedians <- subset(cleaned_text_df, generation == "Millenial")
genX_comedians <- subset(cleaned_text_df, generation == "Generation X")
boomer_comedians <- subset(cleaned_text_df, generation == "Baby Boomer")
silentGen_comedians <- subset(cleaned_text_df, generation == "Silent Generation")


newcreateFrequencyList <- function(dataframe){
  # text is the transcript vector of the data frame
  text <- dataframe$reg_text
  
  # replace special characters like curly quotations, etc. with an apostrophe (probably not needed since that will be
  # truncated via stemDocument later on)
  text <- gsub("[\u2018\u2019\u201A\u201B\u2032\u2035\u201C\u201D\u2013\u2026]", "'", text)
  
  text <- gsub("[Nn] i g g a|[Nn]\\*gga", "nigga", text)
  text <- gsub("[Ff]\\*ck", "fuck", text)
  text <- gsub("[Ss]h\\*t", "shit", text)
  text <- gsub("[Bb]\\*tch", "bitch", text)
  text <- gsub("([a-z])([\\s\\!\\?\\.])([A-Z])", "\\1\\2 \\3", text)
  # process of 
  corpus <- Corpus(VectorSource(text))
  corpus <- tm_map(corpus, PlainTextDocument)
  corpus <- tm_map(corpus, tolower)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeWords, stopwords("en"))
  corpus <- tm_map(corpus, stemDocument)
  corpus <- tm_map(corpus, stripWhitespace)
  
  DTM <- TermDocumentMatrix(corpus)
  mat <- as.matrix(DTM)
  f <- sort(rowSums(mat), decreasing = TRUE)
  dat <- data.frame(word = names(f), freq = f)
  rownames(dat) <- NULL
  return(dat)
}

millenial_tdm <- newcreateFrequencyList(millenial_comedians)
genX_tdm <- newcreateFrequencyList(genX_comedians)
boomer_tdm <- newcreateFrequencyList(boomer_comedians)
silentGen_tdm <- newcreateFrequencyList(silentGen_comedians)


millenialFreq <- millenial_tdm[1:200,]
genXFreq <- genX_tdm[1:200,]
boomerFreq <- boomer_tdm[1:200,]
silentGenFreq <- silentGen_tdm[1:200,]

firstMerge <- merge(millenialFreq, genXFreq, by.x="word", by.y="word", all = TRUE)
colnames(firstMerge) <- c("word", "Millenials", "Generation X")
secondMerge <- merge(firstMerge, boomerFreq, by.x = "word", by.y = "word", all = TRUE)
colnames(secondMerge) <- c("word", "Millenials", "Generation X", "Baby Boomers")
finalMerge <- merge(secondMerge, silentGenFreq, by.x = "word", by.y = "word", all = TRUE)
colnames(finalMerge) <- c("word", "Millenials", "Generation X", "Baby Boomers","Silent Generation")


finalDTM <- finalMerge[,2:5]
rownames(finalDTM) <- finalMerge[,1]

for(i in 1:nrow(finalDTM)){
  for(j in 1:4){
    if(is.na(finalDTM[i,j]) == TRUE){
      finalDTM[i,j] <- 0
    }
  }
}


library(wordcloud)
comparison.cloud(finalDTM, random.order=FALSE, colors = c("#000000", "#009E73", "#56B4E9", "#E69F00"), title.size = 1)

c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")






png("wordcloud_packages.png", width = 12, height = 8, units = "in", res = 500)
