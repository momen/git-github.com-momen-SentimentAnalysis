twitter.corpus <- as.data.frame(unclean.corpus$tweet)


twitter.corpus <- sapply(twitter.corpus, Ar.removeStopWords)