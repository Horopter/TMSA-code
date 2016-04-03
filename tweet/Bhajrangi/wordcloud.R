library("plyr")
library("twitteR")
library("ROAuth")
library("bitops")
library("NLP")
library("RCurl")
library("devtools")
library("proto")
library("gsubfn")
library("tm")
library("RColorBrewer")
library("wordcloud")
library("ggplot2")
library("gridExtra")
cnt <- 1
f <- "1-corpy"
while(file.exists(paste(f,"1400.txt",sep='/')))
{
	corpus <- 0
	source <- DirSource(f)
	YourCorpus <- Corpus(source, readerControl=list(reader=readPlain))
	if(cnt ==1)
		corpus <- YourCorpus
	else
	corpus <- c(corpus,YourCorpus,recursive=T)
	cnt <- cnt + 1
	f <- toString(cnt)
	f <- paste(f, "corpy", sep="-")
	cat(c("corpus successful ",cnt - 1,"\n"))
}
#wordcloud(corpus,min.freq = 10, scale=c(7,0.5),colors=brewer.pal(8, "Dark2"),  random.color= TRUE, random.order = FALSE, max.words = 150)
cat("successful execution, see word cloud\n")
