getwd()
setwd("/Users/byungwookkang")

library(rvest)
library(dplyr)

file_names <- list.files("html")

setwd("C:/Users/betty/OneDrive/Surface Pro 2017/UCLA/R Studio/Stats 141SL/Final Project/HTML Files")


complete_coll_data <- list()

for (file in file_names){
  
  html_data <- read_html(file)
  
  title_data <- html_data %>%
    rvest::html_nodes('h1') %>%
    rvest::html_text()
  
  text_data <- html_data %>%
    rvest::html_nodes('p[style]') %>%
    rvest::html_text()
  
  
  indiv_data <- list(title_data, text_data)
  
  complete_coll_data <- c(complete_coll_data, list(indiv_data))
  
}