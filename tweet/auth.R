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
ck <- '9CrXVQ7bm6ITEimbiXZH3REWq'
cs <- 'D3rBzhtyJT7AbKxR3Dr1IoKw9sqrP1Yp7coXy498iioiab2KAV'
ak <- '136316586-BCnBH2odRZZcIEImEELesDcxgUJfIcyGUS3JQDhd'
as <- 'Tpksq4lIvqggpqesOPE1YaduqL3RajldiNQdbwm57YUWd'
setup_twitter_oauth(ck,cs,ak,as)
