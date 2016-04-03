search.string <- "#BhajrangiBhaijaan -RT"
no.of.tweets <- 1400
tweets <- searchTwitter(search.string, n=no.of.tweets, lang="en")
tweety <- do.call("rbind", lapply(tweets, as.data.frame))
f <- "1.csv"
cnt <- 1
while(file.exists(f))
{
	cnt <- cnt + 1
	f <- toString(cnt)
	f <- paste(f, "csv", sep=".")
}
file.create(f)
write.csv(tweety,file=f)
tweety <- read.csv(f,sep=",")
apply(tweety[, -1], 1, function(r) paste(names(tweety)[-1], r, sep=":", collapse=" "))
tweets.text <- sapply(tweets, function(x) x$getText())
tweets.text <- gsub("rt", "", tweets.text)
tweets.text <- gsub("@\\w+", "", tweets.text)
tweets.text <- gsub("[[:punct:]]", "", tweets.text)
tweets.text <- gsub("http\\w+", "", tweets.text)
tweets.text <- gsub("[ |\t]{2,}", "", tweets.text)
tweets.text <- gsub("^ ", "", tweets.text)
tweets.text <- gsub(" $", "", tweets.text)
tweets.text <- gsub("[^[:alnum:]///' ]", "", tweets.text)
tweets.text.corpus <- Corpus(VectorSource(tweets.text))
tweets.text.corpus <- tm_map(tweets.text.corpus, removeWords, stopwords(),lazy=TRUE)
text.corpus <- tweets.text.corpus
text.corpus <- tm_map(text.corpus, tolower, lazy=TRUE)
text.corpus <- tm_map(text.corpus, removeNumbers, lazy=TRUE)
text.corpus <- tm_map(text.corpus, removePunctuation, lazy=TRUE)
text.corpus <- tm_map(text.corpus, stripWhitespace, lazy=TRUE)
text.corpus <- tm_map(text.corpus, stemDocument, lazy=TRUE)
#wordcloud(tweets.text.corpus,min.freq = 2, scale=c(7,0.5),colors=brewer.pal(8, "Dark2"),  random.color= TRUE, random.order = FALSE, max.words = 150)
print("corpus established")
cnt <- 1
while(file.exists(paste(cnt,"-corpy/1.txt",sep=""))) cnt <- cnt + 1
fd <- paste(cnt,"corpy",sep="-")
dir.create(fd)
setwd(fd)
writeCorpus(tweets.text.corpus)
setwd('..')
cat("successful\n")
