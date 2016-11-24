install.packages("KoNLP")
install.packages("plyr")
install.packages("chron")
install.packages("rJava")
install.packages("wordcloud")
install.packages("ggplot2")
install.packages("munsell")
install.packages("tm")
install.packages("labeling")
install.packages("data.table")
library(wordcloud)
library(rJava)
library(KoNLP)
library(plyr)
library(ggplot2)
library(tm)
library(labeling)
library(data.table)

getwd()

.jinit()
#세종사전사용 
useSejongDic()

#세종사전에 추가할 학교명을 read함.
schoolList <- read.delim("./workspace_R/R_Textmining/all_school_name.txt", 
                         fileEncoding = "UTF-8", sep="\t", header = F)
#컬럼명을 변경함. 
names(schoolList) <- c("name_list", "attribute")

#학교명사전 추가
mergeUserDic(schoolList)

#comment 목록취득(workspace에 csv파일 복사해놓을것 )
csvFile <- readLines("./workspace_R/R_Textmining/comments.csv", encoding = "UTF-8")
#csvFile <- iconv(csvFile, localeToCharset()[1], "UTF-8")
# zz <- textConnection(csvFile)
# commentData <- read.csv(zz,header=F,quote="\"") # give text input
# names(commentData) <- c("message")
# close(zz)


#특수문자 및 숫자,영어 제거
remove.lines <- gsub("\\d", " ", csvFile)
remove.lines <- gsub("\\t", " ", remove.lines)
remove.lines <- gsub("\\n", " ", remove.lines)
remove.lines <- gsub("\\W", " ", remove.lines)
remove.lines <- gsub("[A-Za-z]", " ", remove.lines)
remove.lines <- gsub('[[:punct:]]', "", remove.lines)
remove.lines <- gsub('[[:cntrl:]]', "", remove.lines)
remove.lines <- gsub('❤', "", remove.lines)
remove.lines <- gsub('[ㄱ-ㅎ]', '', remove.lines)
remove.lines <- gsub('[ㅏ-ㅠ]', '', remove.lines)


#띄어쓰기 없이 특정byte이상 문장이 존재하는 경우에는 KoNLP에서 ArrayIndexOutOfBoundsException 발생함. 
remove.lines <- gsub('구로구', ' ', remove.lines)
remove.lines <- gsub('장평중학교', '장평중학교 ', remove.lines)

#명사만 골라서 취득한다. 
#이유없이 extractNoun에서 ArrayIndexOutOfBoundsException 발생하는 경우가 있음.
#정확한 원인파악은 되지 않지만, rJava lib를 다시 읽어드리고 .jinit()처리를 하면 정상적으로 동작함.  
nouns.list <- lapply(remove.lines, extractNoun)

#리스트에 존재하는 character를 가지고 학교명을 취득하기 위한 함수
getSchoolName <- function (nouns.list, schoolList) {
    df1 <- data.frame(row.names = c('name_list', 'attribute'))
    for(charList in nouns.list) {
      for(word in charList) {
        temp <- schoolList[schoolList$name_list==word,]
        if(!is.null(temp)) {
          df1 <- rbind(df1, temp)
        }
      }
    }
    df1
}
#rbind에 대한 속도 개선
getSchoolName2 <- function (nouns.list, schoolTable) {
  lt1 <- list()
  i <- 1
  for(charList in nouns.list) {
    for(word in charList) {
      temp <- schoolTable[schoolTable$name_list==word,]
      if(nrow(temp)!=0) {
        lt1[[i]] <- as.character(temp$name_list)
        i <- i+1
      }
    }
  }
  lt1
}


#학교명을 추출함. 현재는 rbind처리함에 따라 엄청느림. 
name.list <- getSchoolName2(nouns.list,as.data.table(schoolList))
name.df <- data.frame(matrix(unlist(name.list), byrow=T))
names(name.df) <- c("name_list")

#level을 제거함. 
name.df.rmLevel <-  droplevels(name.df)

#학교명작성 빈도수를 table을 통해 보여줌. 
name.cnt <- table(name.df.rmLevel$name_list)

#data.frame으로 변경함. 
name.cnt.df <- as.data.frame(name.cnt)

#컬럼명 변경
names(name.cnt.df) <- c("name", "cnt")

#cnt기준으로 sort
name.cnt.sortval<-name.cnt.df[order(desc(name.cnt.df$cnt)),]

# Text mining처리
myCorpus_ <- Corpus(VectorSource(name.df.rmLevel$name_list))
myTdm <- TermDocumentMatrix(myCorpus_)
findFreqTerms(myTdm)

termFrequency <- rowSums(as.matrix(myTdm))
termFrequency <- subset(termFrequency, termFrequency >= 10)

#ggplot을 통한 view
ggplot(data.frame(term = names(termFrequency), 
                  freq = termFrequency), 
                  aes(term, freq)) + geom_bar(stat="identity") + coord_flip()



#wordcloud를 통한 View
m <- as.matrix(myTdm)
wordFreq <- sort(rowSums(m), decreasing = TRUE)
set.seed(1234)
pal <- brewer.pal(8,"Dark2")

wordcloud(words = names(wordFreq), scale=c(4.3,0.75), max.words=150, 
          freq = wordFreq, min.freq = 3, random.order = F, 
          use.r.layout=FALSE, rot.per=0.2, colors = pal)
