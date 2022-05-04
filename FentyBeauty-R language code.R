setwd(/Users/praharshita/Desktop/BI_project)
getwd()
#Target company is Fenty Beauty which sells makeup and skin care
install.packages("rtweet")
library(rtweet)
#Get the tweets talking about fenty beauty and from @fentybeauty
searchFenty <- search_tweets("Fenty",n=1000,include_rts = FALSE,lang="en")
View(searchFenty)
#Search tweets for Walmarts account
FentyTweets <- get_timeline("@fentybeauty",n=1000,include_rts = FALSE,lang="en")
View(FentyTweets)
#Combine the data from both data frames
AllData <- rbind(searchFenty,FentyTweets)
#Lets plot tweets by minute 
install.packages("ggplot2")
library(ggplot2)
AllData %>%
  ts_plot(by="mins")+
  ggplot2::theme_minimal()+
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))+
  ggplot2::labs(
    X=NULL,Y=NULL,
    title="Frequency of Fenty beauty Tweets by the minute",
    subtitle="Tweet counts aggregated using three-hour intervals"
  )
#Export the data frames into a .xlsx file for future use
install.packages("writexl")
library(writexl)
write_xlsx(AllData,"FentyData.xlsx")

#Sentiment Analysis using syuzhet and afinn
#Syuzhet package
install.packages("syuzhet")
library(syuzhet)
senti_syuzhet<-get_sentiment(AllData$text,method="syuzhet")
#Afinn package
install.packages("textdata")
library(textdata)
install.packages("tidytext")
library(tidytext)
library(textdata)
get_sentiments("afinn")
senti_afinn<-get_sentiment(AllData$text,method="afinn")
#Add the two sentiments columns to the data set
Sentiment_Fenty<-cbind(AllData,senti_syuzhet,senti_afinn)
#Use grepl() to get tweets about specific products
#Some makeup products:foundation,lip gloss,lipstick,blush,highlighter,bronzer,skin tint,concealer
#Some skincare products:Face mask,sunscreen,moisturizer
containsFoundation<-grepl("foundation",Sentiment_Fenty$text,ignore.case = TRUE)
containsLipGloss<-grepl("gloss",Sentiment_Fenty$text,ignore.case = TRUE)
containsBlush<-grepl("blush",Sentiment_Fenty$text,ignore.case = TRUE)
containsHighlighter<-grepl("highlight",Sentiment_Fenty$text,ignore.case = TRUE)
containsBronzer<-grepl("bronzer",Sentiment_Fenty$text,ignore.case = TRUE)
containsConcealer<-grepl("concealer",Sentiment_Fenty$text,ignore.case = TRUE)
containsTint<-grepl("tint",Sentiment_Fenty$text,ignore.case = TRUE)
containsFaceMask<-grepl("face mask",Sentiment_Fenty$text,ignore.case = TRUE)
containsSunscreen<-grepl("sunscreen",Sentiment_Fenty$text,ignore.case = TRUE)
containsMoisturizer<-grepl("moisturizer",Sentiment_Fenty$text,ignore.case = TRUE)
#Now merge all the findings into a data set and export as .xlsx for future use
Final_Fenty<-cbind(Sentiment_Fenty,containsFoundation,containsLipGloss,containsBlush,containsHighlighter,containsBronzer,containsTint,containsFaceMask,containsSunscreen,containsMoisturizer,containsConcealer)
write_xlsx(Final_Fenty,"FentyDataFinal.xlsx")
#Now for better visualization we clean the data and remove some words
attach(Final_Fenty)
plot(created_at,senti_syuzhet,type="h")
plot(created_at,senti_afinn,type="h")
#Clean the Final_Fenty Data Frame
#Cleaning the data set
#Remove duplicated cases
dup<-duplicated(status_id)
table(dup)
dupcase <- status_id[duplicated(status_id)]
newdata<-sentdata[!duplicated(sentdata$status_id),]
#Create dataset with only needed variables
data_tweet<-newdata[,c("status_id","text")]
data_tweet<-data.frame(doc_id=status_id,text=text,stringsAsFactors =FALSE)
#Construct corpus
install.packages("tm")
library(tm)
install.packages("tmap")
library(tmap)
tweet1<-Corpus(DataframeSource(data_tweet))
tweet1[[1]]$content
#noise Removal in tweets
removeRT<-function(x){gsub("(rt|via)((?:\\b\\W\\*@\\w+)+)","",x)}
tweet2=tm_map(tweet1,content_transformer(removeRT))
#Hashtag removal
removeHashtag<-function(x){gsub("#\\S+","",x)}
tweet3=tm_map(tweet2,content_transformer(removeHashtag))
#URL removal
removeURL<-function(x){gsub("http[^[:space:]]*","",x)}
tweet4=tm_map(tweet3,content_transformer(removeURL))
tweet4[[1]]$content
#HTML remover
unescapeHTML<-function(str){return(gsub("<.*?>","",str))}
tweet5=tm_map(tweet4,content_transformer(unescapeHTML))
#Mention removal
removeMention<-function(x){gsub("@\\w+","",x)}
tweet6=tm_map(tweet5,content_transformer(removeMention))
tweet6[[1]]$content
#Carriage removal
removeCarriage<-function(x){gsub("[\r\n]","",x)}
tweet7=tm_map(tweet6,content_transformer(removeCarriage))
#Emoticon removal
removeEmoticon<-function(x){gsub("[^\x01-\x7F]","",x)}
tweet8=tm_map(tweet7,content_transformer(removeEmoticon))
tweet8[[34]]$content
#Lower Case
tweet9=tm_map(tweet8,content_transformer(tolower))
#Remove punctuation
tweet10=tm_map(tweet9,removePunctuation)
#Remove numbers
tweet11=tm_map(tweet10,removeNumbers)
#Remove stopwords
tweet12=tm_map(tweet11,removeWords,stopwords("english"))
#Remove specific stopwords
tweet13=tm_map(tweet12,removeWords,c("savage","lingerie","drake","baby","fenty","asap rocky","a$ap rocky","beauty","rihanna","boo","lol","im","amp","robyn","yall","asap","rocky","ass","barbados","shes","celebrity","really","since","cause","mama","wanna"))
#Strip white space
tweet14=tm_map(tweet13,stripWhitespace)
#Word stemming
tweet15=tm_map(tweet14,stemDocument)
#Words lemmatization
install.packages("textstem")
library(textstem)
tweet16=tm_map(tweet15,lemmatize_strings)
tweet17<-data.frame(text=sapply(tweet16,as.character),stringsAsFactors = FALSE)
tweet18<-cbind(Index=rownames(tweet17),status_id,tweet17,senti_syuzhet,senti_afinn,containsFoundation,containsLipGloss,containsBlush,containsHighlighter,containsBronzer,containsTint,containsFaceMask,containsSunscreen,containsMoisturizer,containsConcealer)
rownames(tweet18)<-1:nrow(tweet18)
View(tweet18)
install.packages("dplyr")
install.packages("magrittr")
library(dplyr)
library(magrittr)
worddata<- tweet18 %>%
  unnest_tokens(word,text)
#Wordcloud
install.packages("wordcloud")
png("WordCloud.png",width = 20,height = 15,units = 'in',res = 300)
library(wordcloud)
worddata %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word,n,max.words=100,colors=brewer.pal(6,"Dark2")))
Concealer<-tweet18[which(tweet18$containsConcealer=="TRUE"),]
concealerData<-Concealer %>%
  unnest_tokens(word,text)
png("WordCloudConcealer.png",width = 30,height = 15,units = 'in',res = 300)
concealerData %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word,n,max.words=100,colors=brewer.pal(6,"Dark2")))  
Foundation<-tweet18[which(tweet18$containsFoundation=="TRUE"),]
foundationData<-Foundation %>%
  unnest_tokens(word,text)
png("WordCloudFoundation.png",width = 30,height = 15,units = 'in',res = 300)
foundationData %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word,n,max.words=100,colors=brewer.pal(6,"Dark2")))  
Bronzer<-tweet18[which(tweet18$containsBronzer=="TRUE"),]
bronzerData<-Bronzer %>%
  unnest_tokens(word,text)
bronzerData %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word,n,max.words=100,colors=brewer.pal(6,"Dark2")))  
Gloss<-tweet18[which(tweet18$containsLipGloss=="TRUE"),]
glossData<-Gloss %>%
  unnest_tokens(word,text)
glossData %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word,n,max.words=100,colors=brewer.pal(6,"Dark2")))  
Tint<-tweet18[which(tweet18$containsTint=="TRUE"),]
tintData<-Tint %>%
  unnest_tokens(word,text)
tintData %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word,n,max.words=100,colors=brewer.pal(6,"Dark2"))) 
Highlighter<-tweet18[which(tweet18$containsHighlighter=="TRUE"),]
highlighterData<-Bronzer %>%
  unnest_tokens(word,text)
highlighterData %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word,n,max.words=100,colors=brewer.pal(6,"Dark2")))  
#Import the final tweets into a .xlsx file
write_xlsx(tweet18,"FentyBeauty_Cleaned&Final.xlsx")
























