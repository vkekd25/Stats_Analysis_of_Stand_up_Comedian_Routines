#### Testing on Generations

m <- millenial_tdm[1:1000,]
g <- genX_tdm[1:1000,]
b <- boomer_tdm[1:1000,]
s <- silentGen_tdm[1:1000,]


bigfirstMerge <- merge(m, g, by.x="word", by.y="word", all = TRUE)
colnames(bigfirstMerge) <- c("word", "Millenials", "Generation X")
bigsecondMerge <- merge(bigfirstMerge, b, by.x = "word", by.y = "word", all = TRUE)
colnames(bigsecondMerge) <- c("word", "Millenials", "Generation X", "Baby Boomers")
bigfinalMerge <- merge(bigsecondMerge, s, by.x = "word", by.y = "word", all = TRUE)
colnames(bigfinalMerge) <- c("word", "Millenials", "Generation X", "Baby Boomers","Silent Generation")


bigfinalDTM <- bigfinalMerge[,2:5]
rownames(bigfinalDTM) <- bigfinalMerge[,1]

for(i in 1:nrow(bigfinalDTM)){
  for(j in 1:4){
    if(is.na(bigfinalDTM[i,j]) == TRUE){
      bigfinalDTM[i,j] <- 0
    }
  }
}


xexpected <- apply(bigfinalDTM, 1, mean) ### "expected" row-wise values

#### see, e.g., https://www.statisticssolutions.com/chi-square-goodness-of-fit-test/

xnumerator <- (bigfinalDTM - xexpected)^2 ; xnumerator

xdenominator <- matrix(xexpected, nrow=nrow(bigfinalDTM), ncol=ncol(bigfinalDTM)) ; xdenominator

obs_chisq <- sum(xnumerator / xdenominator) ; obs_chisq

obs_df <- (nrow(bigfinalDTM) - 1) * (ncol(bigfinalDTM) - 1) ; obs_df

######### what's the p-value

pchisq(obs_chisq, df=obs_df, lower.tail = FALSE) ### lower tail FALSE because we're looking for area under right tail

#### GROUPs are significantly different in their vocabulary


##### Testing on Race x Gender


wM <- whiteMale_tdm[1:1000,]
wF <- whiteFemale_tdm[1:1000,]
pM <- pocMale_tdm[1:1000,]
pF <- pocFemale_tdm[1:1000,]


bigfirstMerge <- merge(wM, wF, by.x="word", by.y="word", all = TRUE)
colnames(bigfirstMerge) <- c("word", "White Males", "White Females")
bigsecondMerge <- merge(bigfirstMerge, pF, by.x = "word", by.y = "word", all = TRUE)
colnames(bigsecondMerge) <- c("word", "WhiteMales", "White Females", "Female POC")
bigfinalMerge <- merge(bigsecondMerge, pM, by.x = "word", by.y = "word", all = TRUE)
colnames(bigfinalMerge) <- c("word", "White Males", "White Females", "Female POC","Male POC")


bigfinalDTM <- bigfinalMerge[,2:5]
rownames(bigfinalDTM) <- bigfinalMerge[,1]

for(i in 1:nrow(bigfinalDTM)){
  for(j in 1:4){
    if(is.na(bigfinalDTM[i,j]) == TRUE){
      bigfinalDTM[i,j] <- 0
    }
  }
}


xexpected <- apply(bigfinalDTM, 1, mean) ### "expected" row-wise values

#### see, e.g., https://www.statisticssolutions.com/chi-square-goodness-of-fit-test/

xnumerator <- (bigfinalDTM - xexpected)^2 ; xnumerator

xdenominator <- matrix(xexpected, nrow=nrow(bigfinalDTM), ncol=ncol(bigfinalDTM)) ; xdenominator

obs_chisq <- sum(xnumerator / xdenominator) ; obs_chisq

obs_df <- (nrow(bigfinalDTM) - 1) * (ncol(bigfinalDTM) - 1) ; obs_df

######### what's the p-value

pchisq(obs_chisq, df=obs_df, lower.tail = FALSE) ### lower tail FALSE because we're looking for area under right tail

#### GROUPs are significantly different in their vocabulary



