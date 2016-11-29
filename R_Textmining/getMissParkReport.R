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
library(RColorBrewer)

useSejongDic()
addNouns <- data.frame(c("정유라", "황교안", "최순실", "헬조선",
                         "최순득", "장시호", "청와대", "비선실세",
                         "차은택", "송성각", "비아그라", "박정희",
                         "주진우", "정호성", "세월호", "우병우", 
                         "손석희", "서울대병원장", "새누리당", "정진석",
                         "탄핵", "길라임", "일베", "방송장악", "오마이뉴스",
                         "이명박", "하야" , "대국민 담화", "대국민담화"
                         ), 
                       c("ncn"))
mergeUserDic(addNouns)
consumer_key <- "q9bCPZhZfRZZzgo5KiMNONC3L"
consumer_secret <- "aAtdx4nUGVp7yMFokk84G36qLojN5a0UPI0xWrGBCJxjNFRwfh"
access_token <- "158198949-QjPRHeg9i7SDSZjKtTGvMZYTszOZcXbLVoWtdvTa"
access_secret <- "BuUFr12FmR8CKQ63jn68fhgdi6HQdfT1WCZmxWeGD4SoE"
setup_twitter_oauth(consumer_key, consumer_secret, access_token,access_secret)


keyword <- enc2utf8("박근혜")
searchWord <-searchTwitter(keyword, n=7000)

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
remove.lines <- gsub("#", "", remove.lines)
remove.lines <- gsub("<([[:alpha:]][[:alnum:]]*)(.[^>]*)>([.^<]*)", "", remove.lines)

nouns.list <- lapply(remove.lines, extractNoun)

nouns.unlist <- unlist(nouns.list)
nouns.unlist <- Filter(function(x) {nchar(x) <= 12}, nouns.unlist) 

# head(nouns.unlist)
#dh.wordcount <- table(nouns.unlist)
#dt.wordcount <- data.table(dh.wordcount)
#dt.wordcount.rmLevel <-  droplevels(dt.wordcount)
#names(dt.wordcount.rmLevel) <- c("text", "cnt")

help(KoNLP)

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
#pal <-brewer.pal(8, "Set2")
#pal <-brewer.pal(12, "Set3")

help("brewer.pal")
windowsFonts(malgun=windowsFont("맑은 고딕"))
png("wordcloud.png", width=1000,height=800)
wordcloud(words = names(wordFreq), scale=c(6,0.6), 
          freq = wordFreq, min.freq = 5, random.order = F,
          max.words = 300,
          #random.color = T,
          use.r.layout=FALSE, rot.per=0.25, colors = pal,family="malgun"
          )
dev.off()

warnings()
