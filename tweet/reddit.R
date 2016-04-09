#source('auth.R')
source('analyze.R')
require('RedditExtractoR')
library('RedditExtractoR')
analyze = function(directory,textLoc){
	ch <- paste("1.csv",sep="")
		setwd(directory)
			B <- read.csv(ch,sep=',')
		setwd('..')
	B<-iconv(B, 'UTF-8', 'ASCII')
	entityscores1<-sentimentalanalysis(B,textLoc)
	finalsum1 <- sum(entityscores1$score)
	return(finalsum1)
}

extractme = function(moviename,start_date=NULL,end_date=NULL)
{
	search.string <- get_reddit(search_terms=moviename)
	tweets <- search.string$comment
	 if(is.null(tweety))
		 return
	 f <- "1.csv"
	 cnt <- 1
	 file.create(f)
	 write.csv(tweets,file=f)
	 tweety <- read.csv(f,sep=",")
	tweets.text <- gsub("rt", "", tweets)
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
	print("corpus established")
	cnt <- 1
	while(file.exists(paste(cnt,"-corpy/1.txt",sep=""))) cnt <- cnt + 1
	fd <- paste(cnt,"corpy",sep="-")
	dir.create(fd)
	setwd(fd)
	writeCorpus(tweets.text.corpus)
	setwd('..')
	cat("successful\n")
}
review = function(moviename,start_date=NULL,end_date=NULL,textLoc)
{
	dir.create(file.path(".",moviename), showWarnings = FALSE)
	setwd(file.path(".",moviename))
	while(!file.exists('1.csv'))
	{
		extractme(moviename,start_date,end_date)
	}	
	setwd('..')
	scoring <- analyze(moviename,textLoc)
	print(paste(scoring," is the score of the moviename ",moviename,sep=" "))
}
main_review = function(moviename,start_date=NULL,end_date=NULL)
{
	mainPath <- getwd()
	review(moviename,start_date,end_date,mainPath)
	setwd(mainPath)
}
collect_info = function()
{
vinod <- read.csv(file="dcsv.csv",header=TRUE,sep="\t")
vin <- vinod
i <- sapply(vinod, is.factor)
vin[i] <- lapply(vinod[i], as.character)
main_review(vin[1,1],as.Date(vin[1,4]),as.Date(vin[1,3]))
}

