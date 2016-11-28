install.packages("rJava")
install.packages("wordcloud")
install.packages("KoNLP")
install.packages("ggplot2")
install.packages("tm")
install.packages("twitteR")
install.packages("ROAuth")

library(rJava)
library(wordcloud)
library(KoNLP)
library(ggplot2)
library(tm)
library(twitteR)
library(ROAuth)
library(data.table)

useSejongDic()

consumer_key <- "q9bCPZhZfRZZzgo5KiMNONC3L"
consumer_secret <- "aAtdx4nUGVp7yMFokk84G36qLojN5a0UPI0xWrGBCJxjNFRwfh"
access_token <- "158198949-QjPRHeg9i7SDSZjKtTGvMZYTszOZcXbLVoWtdvTa"
access_secret <- "BuUFr12FmR8CKQ63jn68fhgdi6HQdfT1WCZmxWeGD4SoE"
setup_twitter_oauth(consumer_key, consumer_secret, access_token,access_secret)


keyword <- enc2utf8("#박근혜")
searchWord <-searchTwitter(keyword, n=5000)

searchWord.df <- twListToDF(searchWord)
searchWord.df.rmRT <- searchWord.df[searchWord.df$isRetweet==F,]


#특수문자 및 숫자,영어 제거
remove.lines <- gsub("\\d", "", searchWord.df$text)
remove.lines <- gsub("\\t", "", remove.lines)
remove.lines <- gsub("\\n", "", remove.lines)
# remove.lines <- gsub("\\W", "", remove.lines)
remove.lines <- gsub("[A-Za-z]", "", remove.lines)
remove.lines <- gsub('[[:punct:]]', "", remove.lines)
remove.lines <- gsub('[[:cntrl:]]', "", remove.lines)
remove.lines <- gsub('❤', "", remove.lines)
remove.lines <- gsub('[ㄱ-ㅎ]', '', remove.lines)
remove.lines <- gsub('[ㅏ-ㅠ]', '', remove.lines)
remove.lines <- gsub("@[[:graph:]]*", "", remove.lines)
remove.lines <- gsub("http://[[:graph:]]*", "", remove.lines)
remove.lines <- gsub("박근혜", "", remove.lines)
remove.lines <- gsub("대통령", "", remove.lines)
remove.lines <- gsub("없다더", "", remove.lines)

nouns.list <- lapply(remove.lines, extractNoun)

nouns.unlist <- unlist(nouns.list)
nouns.unlist <- Filter(function(x) {nchar(x) <= 15}, nouns.unlist) 

# head(nouns.unlist)
#dh.wordcount <- table(nouns.unlist)
#dt.wordcount <- data.table(dh.wordcount)
#dt.wordcount.rmLevel <-  droplevels(dt.wordcount)
#names(dt.wordcount.rmLevel) <- c("text", "cnt")



myCorpus_ <- Corpus(VectorSource(nouns.unlist))
# myCorpus_ <- tm_map(myCorpus_, removePunctuation)
# myCorpus_ <- tm_map(myCorpus_, removeNumbers)
# myCorpus_ <- tm_map(myCorpus_, tolower)

myTdm <- TermDocumentMatrix(myCorpus_)
findFreqTerms(myTdm)

m <- as.matrix(myTdm)
wordFreq <- sort(rowSums(m), decreasing = TRUE)

set.seed(375)
pal <- brewer.pal(8,"Dark2")
#pal <-brewer.pal(9, "Set1")



wordcloud(words = names(wordFreq), scale=c(7,0.7), 
          freq = wordFreq, min.freq = 4, random.order = F,
          random.color=T, 
          use.r.layout=FALSE, rot.per=0.25, colors = pal)
warnings()
