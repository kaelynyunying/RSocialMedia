
######### Using tweets we perform sume text analytics

#Search twitter and print tweet and who tweeted it
messiTweets = searchTwitter("Messi")

for (i in 1:length(messiTweets)) {
  print(paste("Tweet = ", messiTweets[[i]]$getText()  ))
  print(paste("User = ",  messiTweets[[i]]$getScreenName() ))
}

#Get followers for a user
foxSports = getUser("FoxSports")
str(foxSports)
followList=foxSports$getFollowers(10)

for (i in 1:length(followList)) {
  print(paste("Screen Name = ", followList[[i]]$getScreenName() ,
              " Followers Count = ",  
              followList[[i]]$getFollowersCount() ))
}

foxSports$getFollowerIDs(10)

#Get trends by location
availableTrendLocations()
getTrends(23424948)  # for Singapore

################################# extracting and cleaning tweets ########################

library(twitterR)
tweets=userTimeline("Obama", n=3200)   # can change to diff topic

# analyse data
n.tweet=length(tweets)
tweets[1:5]   # see first 5 tweets

# convert to dataframe

#tweets.df=do.call("rbind", lapply(tweets, as.data.frame))

tweets.df=twListtoDF(tweets)
dim(tweets.df)


# build a corpus
library(tm)
myCorpus = Corpus(VectorSource(tweets.df$text))
myCorpus = tm_map(myCorpus, tolower)   # tm_map(myCorpus, content_transofrmer(tolower))
myCorpus = tm_map(myCorpus, removePunctuation)
myCorpus = tm_map(myCorpus, removeNumbers)
myCorpus = tm_map(myCorpus, removeURL)
myStopWords = c(stopwords("english"), "available", "via")  # removing these words
myStopWords=setdiff(myStopWords, c("r", "big"))
myCorpus=tm_map(myCorpus, removeWords, myStopwords)
myCorpusCopy=myCorpus
myCorpus=tm_map(myCorpus, stemDocument)

# inspect the first 5 tweets
for (i in 1:5) {
                cat(paste("[[", i, "]]", sep=""))
                writeLines(myCorpus[[i]])
}

#stem completion
myCorpus=tm_map(myCorpus, stemCompletion, dictionary=myCorpusCopy)

# count frequency of word 'mining'
miningCases=tm_map(myCorpusCopy, grep, pattern="\\<mining")
sum(unlist(miningCases))

# replace 'mining' with 'miners'
myCopus=tm_map(myCorpus, gsub, pattern="mining", replacement="miners")

# term document matrix
tdm=TermDocumentMatrix(myCorpus, control=list(wordLengths=c(1, Inf)))
tdm


# frequent words and associations

# term document matix (6 terms, 10 documents)
idx=which(dimnames(tdm)$Terms == "r")
inspect(tdm[idx + (0:5), 101:110])

freq.terms=findFreqTerms(tdm, lowfreq=15)

term.freq=rowSums(as.matrix(tdm))
term.freq=subset(term.freq, term.freq>=15)
df=data.frame(term=names(term.freq), freq=term.freq)


############################### Visualizations #############################

library(ggplot2)
ggplot(df, aes(x=term, y=freq)) + geom_bar(stat="identity") + xlab("Terms") + ylab("Count") + coord_flip()


# find words assiciate with 'mining'
findAssocs(tdm, "mining", 0.25)

library(graph)
library(Rgraphviz)
plot(tdm, term=freq.terms, corThreshold=0.12, weighting=T)


library(wordcloud)
m=as.matrix(tdm)
word.freq=sort(rowSums(m), decreasing=T)
wordcloud(words=names(word.freq), freq=word.freq, min.freq=3, random.order=F)



################################ Topic Modelling #################################

dtm=as.DocumentTermMatrix(tdm)

library(topicmodels)
lda=LDA(dtm, k=8)
term=terms(lda,4)
term
term=apply(term, MARGIN=2, paste, collapse=", ")

topic=topics(lda, 1)
topics=data.frame(date=as.Date(tweets.df$created), topic)
qplot(date, ..count.., data=topics, geom="density", fill=term[topic], position="stack")


#################################################################################
#                                 Sentiment Analysis
#################################################################################

# to learn if text words hint >> positive, negative or nuetral

# import our list of positive and negative words to check against >>
pos=readLines("C://Users//user//Desktop//RcodeProg//{Ranalytics}//Rsocialmedia_anly//bagofwords//Positive-Words.txt")
neg=readLines("C://Users//user//Desktop//RcodeProg//{Ranalytics}//Rsocialmedia_anly//bagofwords//Negative-Words.txt")


########################## this is a function for sentiment analysis #############################

score.sentiment = function(sentences, pos.words, neg.words)
{
  require(stringr)
  require(plyr)
  scores = laply(sentences,
                 function(sentence, pos.words, neg.words)
                 {
                   sentence = gsub("[[:punct:]]", "", sentence)
                   sentence = gsub("[[:cntrl:]]", "", sentence)
                   sentence = gsub('\\d+', '', sentence)
                   tryTolower = function(x)
                   {
                     y = NA
                     try_error = tryCatch(tolower(x), error=function(e) e)
                     if (!inherits(try_error, "error"))
                       y = tolower(x)
                     return(y)
                   }
                   sentence = sapply(sentence, tryTolower)
                   word.list = str_split(sentence, "\\s+")
                   words = unlist(word.list)
                   pos.matches = match(words, pos.words)
                   neg.matches = match(words, neg.words)
                   pos.matches = !is.na(pos.matches)
                   neg.matches = !is.na(neg.matches)
                   score = sum(pos.matches) - sum(neg.matches)
                   return(score)
                 }, pos.words, neg.words)
  scores.df = data.frame(text=sentences, score=scores)
  return(scores.df)
}



dwitest1=c("Great you are here", "Dwight is awesome experience",
           "Dwight is an idiot", "Dwight loves candy")


testsentiment=score.sentiment(dwitest1, pos, neg)

############################## view -ve sentiment ###############################

data$Negative = as.factor(tweets$score <=-1)
table(data$Negative)


################ tweet sentiment analysis by country ###############3

sporetweets=searchTwitter("singapore", n=1000, lang="en")
spore_txt=sapply(sporetweets, function(x) x$getText()) # convert to text
length(spore_txt)  # see how many tweets
score_spore=score.sentiment(spore_txt, pos, neg)


score_spore$very.pos=as.numeric(score_spore$score >= 2)
score_spore$very.neg=as.numeric(score_spore$score <= -2)

num.pos_spore=sum(score_spore$very.pos)
num.neg_spore=sum(score_spore$very.neg)

head(score_spore)

######## graphical representative

boxplot(score_spore[,2:4])

library(lattice)
histogram(score_spore$score)




