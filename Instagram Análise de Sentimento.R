rm(list=ls())


#------------------------------------------------------------------------
#Variáveis
library(svDialogs)
user_hashtag <- dlgInput("Digite a Hastag que quer pesquisar no Instagram (sem#)", Sys.info()["user"])$res
user_quantidade <- dlgInput("Quantos posts?", Sys.info()["user"])$res

#------------------------------------------------------------------------

library(jsonlite)
library(stringr)
library("jpeg")
library(tidyr)
library(utf8)

#---------------------------------------------------------
#Download JSON File from Instagram for a specific Hashtag
#---------------------------------------------------------
hashtag <- user_hashtag
url_start <- str_glue("http://instagram.com/explore/tags/{hashtag}/?__a=1")
json <- fromJSON(url_start)
edge_hashtag_to_media <- json$graphql$hashtag$edge_hashtag_to_media
end_cursor <- edge_hashtag_to_media$page_info$end_cursor
posts <- edge_hashtag_to_media$edges$node

#-----------------------------
#Extract Information per Post
#-----------------------------
index <- 1
post_id <- list()
post_text <- list()
post_time <- list()
post_likes <- list()
post_owner <- list()
post_img_url <- list()

extractInfo <- function(index){
  print("extractInfo function called")
  maxrows <- nrow(posts)
  for(i in 1:maxrows){
    if(i == maxrows){
      assign("index", index, envir = .GlobalEnv)
      assign("post_id", post_id, envir = .GlobalEnv)
      assign("post_text", post_text, envir = .GlobalEnv)
      assign("post_time", post_time, envir = .GlobalEnv)
      assign("post_img_url", post_img_url, envir = .GlobalEnv)
      assign("post_likes", post_likes, envir = .GlobalEnv)
      assign("post_owner", post_owner, envir = .GlobalEnv)
      getNewPosts(index)
    } else {
      post_id[index] <- posts[i,5]
      if(length(posts$edge_media_to_caption$edges[[i]][["node"]][["text"]])==0){
        post_text[index] <- "no-text"
        print("no text in post")
      } else {
        temp <- posts$edge_media_to_caption$edges[[i]][["node"]][["text"]]
        post_text[index] <- gsub("\n", " ", temp)
      }
      
      post_time[index] <- toString(as.POSIXct(posts[i,7], origin="1970-01-01"))
      post_img_url[index] <- posts[i,9]
      post_likes[index] <- posts[i,11]
      post_owner[index] <- posts[i,12]
      
      #optional: download image
      #img_dir <- str_glue("images/{index}_{hashtag}_post_img.jpg")
      #download.file(posts[i,8], img_dir, mode = 'wb')
      
      index <- index + 1
      
      if (index == user_quantidade)
        break
    }
  }    
}

#------------------------------
#Get New Posts from Instagram
#------------------------------
getNewPosts <- function(index){
  print("getNewPosts function called")
  url_next <- str_glue("{url_start}&max_id={end_cursor}")
  json <- fromJSON(url_next)
  edge_hashtag_to_media <- json$graphql$hashtag$edge_hashtag_to_media
  end_cursor <- edge_hashtag_to_media$page_info$end_cursor
  posts <- edge_hashtag_to_media$edges$node
  assign("end_cursor", end_cursor, envir = .GlobalEnv)
  assign("posts", posts, envir = .GlobalEnv)
  print(index)
  Sys.sleep(5)
  extractInfo(index)
}

#Start the Madness
extractInfo(index)


#-----------------------------
#Export Dataframe to CSV()
#-----------------------------
table <- do.call(rbind.data.frame, Map('c', post_id, post_img_url, post_likes, post_owner, post_text, post_time))
colnames(table) <- c("ID", "URL", "Likes", "Owner", "Text", "Date")
time <- Sys.time()
filename <- str_glue("D:\\1.csv")
write.csv(table, filename, fileEncoding = "UTF-8")

write.table(post_text, file = "d:\\Text.txt",row.names = F,col.names = F)



#------------------------------



library("NLP")
library("twitteR")
library("syuzhet")
library("tm")
library("SnowballC")
library("stringi")
library("topicmodels")
library("syuzhet")
library("twitteR")
library("ROAuth")
library("ggplot2")
library("tm")
library("rio")


tweets <- read.csv("d:\\1.csv", header=F)

google_tweets <- tweets

google_text<- google_tweets$V6

google_text <- gsub("[^\x01-\x7F]", "", google_text)

#convert all text to lower case
google_text<- tolower(google_text)

# Replace blank space (â€œrtâ€)
google_text <- gsub("rt", "", google_text)

# Replace @UserName
google_text <- gsub("@\\w+", "", google_text)

# Remove punctuation
google_text <- gsub("[[:punct:]]", "", google_text)

# Remove links
google_text <- gsub("http\\w+", "", google_text)

# Remove tabs
google_text <- gsub("[ |\t]{2,}", "", google_text)

# Remove blank spaces at the beginning
google_text <- gsub("^ ", "", google_text)

# Remove blank spaces at the end
google_text <- gsub(" $", "", google_text)

#create corpus
google_tweets.text.corpus <- Corpus(VectorSource(google_text))

#getting emotions using in-built function
mysentiment_google<-get_nrc_sentiment((google_text))

#calculationg total score for each sentiment
Sentimentscores_google<-data.frame(colSums(mysentiment_google[,]))

names(Sentimentscores_google)<-"Score"
Sentimentscores_google<-cbind("sentiment"=rownames(Sentimentscores_google),Sentimentscores_google)
rownames(Sentimentscores_google)<-NULL


#plotting the sentiments with scores
ggplot(data=Sentimentscores_google,aes(x=sentiment,y=Score))+geom_bar(aes(fill=sentiment),stat = "identity")+
  theme_minimal() +
  theme(legend.position="none")+
  xlab("Sentimentos")+ylab("Quantidade")+ggtitle("Sentimentos revelados no Instagram")+
  scale_x_discrete(labels=c('Raiva','Antecipação','Desgosto','Medo','Alegria','Negativo','Positivo','Tristeza','Surpresa','Confiança'))


ggsave("Analise-sentimento.png", width = 68, height = 38, units = "cm")
