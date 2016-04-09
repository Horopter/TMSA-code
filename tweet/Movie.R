source('auth.R')
source('analyze.R')
analyze = function(directory,textLoc){
	for( i in 1:30 )
	{
		ch <- toString(i)
		ch <- paste(ch,".csv",sep="")
		setwd(directory)
		if(i==1)
			B <- read.csv(ch,sep=',')
		else
			B <- rbind(B, read.csv(ch,sep=','))
		setwd('..')
	}
	# B for Bahubali and Bb for BhajrangiBhaijaan
	B$text<-iconv(B$text, 'UTF-8', 'ASCII')
	entityscores1<-sentimentalanalysis(B$text,textLoc)
	finalsum1 <- sum(entityscores1$score)
	return(finalsum1)
}
extractme = function(moviename,start_date=NULL,end_date=NULL)
{
	search.string <- paste("#",moviename," movie -RT", sep="")
	no.of.tweets <- 1000
	tweets <- searchTwitter(search.string, n=no.of.tweets, lang="en",since=start_date,until=end_date)
	tweety <- do.call("rbind", lapply(tweets, as.data.frame))
	if(is.null(tweety))
		return(NULL)
	f <- "1.csv"
	cnt <- 1
	while(file.exists(f))
	{
		cnt <- cnt + 1
		f <- toString(cnt)
		f <- paste(f, "csv", sep=".")
	}
	file.create(f)
	if(is.null(tweety))
		return(NULL)
	write.csv(tweety,file=f)
	if(file.size(f)<=1000)
		return(NULL)
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
	cnt <- 1
	while(file.exists(paste(cnt,"-corpy/1.txt",sep=""))) cnt <- cnt + 1
	cat(paste("corpus load : : ",toString(cnt),"/30 "))
	fd <- paste(cnt,"corpy",sep="-")
	dir.create(fd)
	setwd(fd)
	writeCorpus(tweets.text.corpus)
	setwd('..')
	cat("successful\n")
	return(0)
}
review = function(moviename,start_date=NULL,end_date=NULL,textLoc)
{
	dir.create(file.path(".","MovieCorpus",moviename), showWarnings = FALSE,recursive=TRUE)
	print(file.path(".","MovieCorpus",moviename))
	setwd(file.path(".","MovieCorpus",moviename))
	while(!file.exists('30.csv'))
	{
		chul <- extractme(moviename,start_date,end_date)
		if(is.null(chul))
		{
			return(data.frame("0","0"))
		}
	}
	setwd('1-corpy')
	x <- length(ls())
	setwd('..')
	setwd('..')
	y <- analyze(moviename,textLoc)
	scoring <- data.frame(x,y)
	return(scoring)
	#print(paste(scoring," is the score of the moviename ",moviename,sep=" "))
}
main_review = function(moviename,start_date=NULL,end_date=NULL)
{
	mainPath <- getwd()
	r <- review(moviename,start_date,end_date,mainPath)
	setwd(mainPath)
	return(r)
}
collect_info = function()
{
dfr <- data.frame(moviename=character(),review_value=integer())
vinod <- read.csv(file="dcsv.csv",header=TRUE,sep=",")
vin <- vinod
i <- sapply(vinod, is.factor)
vin[i] <- lapply(vinod[i], as.character)
#r <- do.call(rbind,apply(vin,1,function(x) main_review(x[1])))
sfr <- data.frame(moviename=vin[,1],review_value=do.call(rbind,apply(vin,1,function(x) main_review(x[1]))))
dfr <- rbind(dfr,sfr)
write.csv(dfr,file="reviewMe.csv")
#main(vin[1,1],vin[1,3],vin[1,2])
#main(vin[1,1],vin[1,2],vin[1,5])
#main(vin[1,1],vin[1,5],vin[1,6])
#main(vin[1,1],vin[1,6],vin[1,7])
#main(vin[1,1],vin[1,7],vin[1,8])
#main(vin[1,1],vin[1,8],vin[1,9])
#main(vin[1,1],vin[1,9],vin[1,10])
}


