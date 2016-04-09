#source('auth.R')
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
library("stringr")

CleanTweets<-function(tweets)
  {
    # Remove redundant spaces
    tweets <- str_replace_all(tweets," "," ")
    # Get rid of URLs
    tweets <- str_replace_all(tweets, "http://t.co/[a-zA-Z0-9]*","")
    # Take out retweet header, there is only one
    tweets <- str_replace(tweets,"RT @[a-zA-Z]*: ","")
    # Get rid of hashtags
    tweets <- str_replace_all(tweets,"#[a-zA-Z]*","")
    # Get rid of references to other screennames
    tweets <- str_replace_all(tweets,"@[a-zA-Z]*","")
    return(tweets)
    
  }

  score.sentiment = function(sentences,pos.words, neg.words)
  {
       
    # we got a vector of sentences. plyr will handle a list
    # or a vector as an "l" for us
    # we want a simple array ("a") of scores back, so we use 
    # "l" + "a" + "ply" = "laply":
    scores = laply(sentences, function(sentence, pos.words, neg.words) {
      
      # clean up sentences with R's regex-driven global substitute, gsub():
      sentence = gsub('[[:punct:]]', '', sentence)
      sentence = gsub('[[:cntrl:]]', '', sentence)
      sentence = gsub('\\d+', '', sentence)
      # and convert to lower case:
      sentence = tolower(sentence)
      
      # split into words. str_split is in the stringr package
      word.list = str_split(sentence, '\\s+')
      # sometimes a list() is one level of hierarchy too much
      words = unlist(word.list)
      
      # compare our words to the dictionaries of positive & negative terms
      pos.matches = match(words, pos.words)
      neg.matches = match(words, neg.words)
      
      # match() returns the position of the matched term or NA
      # we just want a TRUE/FALSE:
      pos.matches = !is.na(pos.matches)
      neg.matches = !is.na(neg.matches)
      # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
      score = sum(pos.matches) - sum(neg.matches)
      
      return(score)
    }, pos.words, neg.words)
    
    scores.df = data.frame(score=scores, text=sentences, size=seq(length(scores)))
    return(scores.df)
  }
  
sentimentalanalysis<-function(entitytext,textLoc){
   positivewords=readLines(file.path(textLoc,"positive.txt"))
   negativewords=readLines(file.path(textLoc,"negative.txt"))
   entityscore = score.sentiment(CleanTweets(entitytext),positivewords,negativewords)
}   

