Sys.setenv(JAVA_HOME="C:\\Program Files\\Java\\jdk1.8.0_121")
library(XML)
library(rvest)
library(RCurl)
library(RColorBrewer)
library(wordcloud)
library(rJava)
library(KoNLP)


#네이버 영화 리뷰 url출력
url <- "https://movie.naver.com/movie/point/af/list.nhn?&page="
df.movie <- NULL
for (pageNO in 1:100) {
  url2 <- paste(url,pageNO,sep="")
  url2 <- read_html(url2,encoding = "CP949")
  url2
  doc <- htmlParse(url2)
  doc
  title2 <- xpathSApply(doc, "//div[@id='old_content']//a[@class='movie']", xmlValue)
  point2 <- xpathSApply(doc, "//div[@id='old_content']//td[@class='point']", xmlValue)
  review2 <- xpathSApply(doc,"//div[@id='old_content']//td[@class='title']", xmlValue)
  review2 <- gsub('\n', '', review2)
  review2 <- gsub('\t', '', review2)
  review2 <- gsub('신고', '', review2)
  df <- data.frame(title=title2, point=point2, review=review2)
  df.movie <- rbind(df.movie, df)
}
df.movie


#df.movie의 제목만 텍스트파일로저장
write.table(df.movie$title,"C:\\Users\\ss\\Desktop\\movie_webScrapping\\movie_title_only.txt", sep="\t", row.names=FALSE)


#9개 영화와 나머지 리뷰빈도 수를 추려내 wordcloud로 출력
pal2<-brewer.pal(10,"Paired")
a <- length(which(df.movie$title=="기생충"))
b <- length(which(df.movie$title=="알라딘"))
c <- length(which(df.movie$title=="걸캅스"))
d <- length(which(df.movie$title=="엑스맨: 다크 피닉스"))
e <- length(which(df.movie$title=="로켓맨"))
f <- length(which(df.movie$title=="빅샤크2: 해저2만리"))
g <- length(which(df.movie$title=="고질라: 킹 오브 몬스터"))
h <- length(which(df.movie$title=="어벤져스: 엔드게임"))
i <- length(which(df.movie$title=="이웃집 토토로"))
etc <- 1000 - (a+b+c+d+e+f+g+h+i)
frequency <- c(a, b, c, d, e, f, g, h, i, etc)
name <- c("기생충", "알라딘", "걸캅스", "엑스맨:다크피닉스", "로켓맨", "빅샤크2:해저2만리", "고질라:킹 오브 몬스터", "어벤져스:엔드게임", "이웃집 토토로", "기타")
wordcloud(name, frequency, scale=c(10, 1), random.order=F, random.color=T, rot.per=.3, colors=pal2)


#상위 9개 영화만 barplot으로 출력
freq2 <- table(df.movie$title)
top <- head(sort(freq2, decreasing=TRUE), n=9)
colors <- brewer.pal(9, "Set3")
name <- c("기생충", "알라딘", "엑스맨:다크 피닉스", "걸캅스", "로켓맨", "빅샤크2:해저2만리", "어벤져스:엔드게임", "고질라:킹 오브 몬스터", "이웃집 토토로")
bp <- barplot(top, names.arg=name, main="영화 리뷰 차트", col=colors, xlab="영화제목", ylab="리뷰 빈도 수", ylim=c(0, 350))
text(x=bp, y=top, labels=round(top, 0), pos=1)


#아까 출력한 txt파일을 불러와서 wordcloud출력
text <- readLines(file.choose())
text <- gsub(" ", "", text)
text <- gsub("\"", "", text)
text <- unlist(text)
text_word_count <- table(text)
pal2 <- brewer.pal(12,"Paired")
wordcloud(names(text_word_count), text_word_count, scale=c(7, 1), min.freq=1, 
          random.order=F, random.color = T, rot.per=.1, colors=pal2)


#기생충 영화의 리뷰에 대한 워드 클라우드
useSejongDic()
df.parasite <- df.movie[df.movie$title == "기생충",]
write.table(df.parasite$review,"C:\\Users\\ss\\Desktop\\movie_webScrapping\\movie_review_only.txt", sep="\t", row.names=FALSE)
parasiteReview <- readLines(file.choose())
noun <- sapply(parasiteReview, extractNoun, USE.NAMES=F)
noun2 <- unlist(noun)
noun2 <- gsub("\"", "", noun2)
noun2 <- gsub("x", "", noun2)
noun2 <- gsub("을", "", noun2)
noun2 <- gsub("를", "", noun2)
noun2 <- gsub("이", "", noun2)
noun2 <- gsub("가", "", noun2)
noun2 <- gsub("^^", "", noun2)
noun2 <- gsub("그걸", "", noun2)
noun2 <- gsub("하", "", noun2)
noun2 <- Filter(function(x){ nchar(x)>=2}, noun2)
parasite_review_wordCount <- table(noun2)
pal3 <- brewer.pal(12, "Paired")
wordcloud(names(parasite_review_wordCount), parasite_review_wordCount, scale=c(7, 1), random.order = F, random.color = T, rot.per=0.3, colors=pal3)
