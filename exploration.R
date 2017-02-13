#Garbage collector
rm(list = ls()); gc()
#######################################################################################
#Load libraries
library(stringr)
library(tibble)
#######################################################################################
set.seed(220117)
#######################################################################################
#Set working directory
setwd("F:\\Kaggle - Stack Exchange")
#######################################################################################
#Reading files and prelimnary analysis
biology = read.csv("biology.csv")
cooking = read.csv("cooking.csv")
crypto = read.csv("crypto.csv")
robotics = read.csv("robotics.csv")
travel = read.csv("travel.csv")

#Meta Category ID Df
category_meta = data.frame("Subject" = c("biology", "cooking", "crypto", 
                                         "robotics", "travel"),
                           "ID" = c(1:5))

#Assign categories to files
biology <- add_column(biology, category = rep(1,nrow(biology)), .after = 3)
cooking <- add_column(cooking, category = rep(2,nrow(cooking)), .after = 3)
crypto <- add_column(crypto, category = rep(3,nrow(crypto)), .after = 3)
robotics <- add_column(robotics, category = rep(4,nrow(robotics)), .after = 3)
travel <- add_column(travel, category = rep(5,nrow(travel)), .after = 3)

#Merge all data
all_cats = rbind(biology,cooking,crypto,robotics,travel)

#Removing punctuations
all_cats$title = gsub(pattern = "[.,!?]" , replacement = "",
                     x = all_cats$title) %>% tolower() %>% 

#######################################################################################
#defining tf-idf function
tf_idf <- function(x) {
  x_update = gsub(pattern = "[.,!?]" , replacement = "", x)
  words = str_split(x_update, " ")[[1]] %>% unique()
  weightage = c()
  for (i in words) {
    count = 1
    len_x = gregexpr(" ", x)[[1]] %>% length() + 1
    tf = gregexpr(pattern =  i, x, ignore.case = T)[[1]] %>% length()/len_x
    # print(tf)
    idf = log(nrow(biology)/grepl(i, biology$title) %>% sum())
    # print(idf)
    print(i)
    print(tf*idf)
  }
  # return(weightage)
}

#Probably filter out all words with tf-idf < 0.3
tf_idf(biology$title[6])


text = "Do you think I have gone crazy boy?

I'm just enjoying my J and   talking shit bruh    !
-#$%&*("

#Remove spaces, punctuations, and split strings by words

stopwords_regex = paste(stopwords('en'), collapse = '\\b|\\b')
# stopwords_regex = paste0('\\b', stopwords_regex, '\\b')

tokenizer <- function(text) {
  output = gsub('[[:punct:]]' , replacement = "", text) %>% tolower()
  output = gsub("[\r\n+]", " ", output)
  output = stringr::str_replace_all(output, stopwords_regex, '')
  output = gsub("\\s+", " ", str_trim(output))
  output =  str_split(output," ", simplify = T)
  return(output)
  
}

text = tokenizer(text = text)
text

