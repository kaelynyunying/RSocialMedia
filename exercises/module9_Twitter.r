library(twitteR)
library(ROAuth)
library(RCurl)
library(httr)
library(SocialMediaLab)
library(magrittr)
library(igraph)

#######################################################################
#                                Authentication
#######################################################################

key="Mblrycq3sF4JTIEfBDvzbSefr"
secret="sVSnam6T7fOCRDlig8tcbJTBMb1sHfYTL94Dfp5soFpbA4fxD8"
tktoken="446052007-HX8Q2RIYvd4YTYInwMvyDOxK41hZKBaMLTlFEl5e"
tksecret="Bt7Wkuxwj1qWFL0nFOp8GA25Rnw5iCjs892EVcBvt86qC"
cacert="C://Users//user//Desktop//RcodeProg//{Rsocialmedia}//httrOauth//cacert.pem"

setup_twitter_oauth(key,secret,tktoken,tksecret)


########################################################################
#			       User Objects
########################################################################


# get my user object
myUser=getUser("DwightFonseka")
str(myUser)
myUser$getScreenName()
myUser$getFriends()  # who i subscribed to
myUser$getFollowers()
myUser$getLocation()


# get my timeline (tweets i receive)
myTimeline=homeTimeline()
length(myTimeline)
str(myTimeline[1])

#print each tweet in myTimeline
for(i in 1:length(myTimeline)){
  print(myTimeline[[i]]$getText())
}


# get another user's timeline
another=userTimeline("myname2")


#print tweets, retweet count, hastags and user mentions
for (i in 1:length(myTimeline)){
  print(paste("Tweet=", myTimeline[[i]]$getText()))
  print(paste("Retweet Count=", myTimeline[[i]]$getRetweetCount()))
 # print(paste("Hashtags=", getHashTags(foxTweets[[i]]$getText())))
 # print(paste("user Mentions=", getUserMentions(foxTweets[[i]]$getText())))
}


#Get followers for a user
followMe=myUser$getFollowers(10)

for (i in 1:length(followMe)) {
  print(paste("Screen Name = ", followMe[[i]]$getScreenName() ,
              " Followers Count = ",  
              followMe[[i]]$getFollowersCount() ))
}

################################################################################
#			Searching Tweets
################################################################################

# search latest 10 tweets
myTweets=searchTwitter("myname", n=10)  #can specify any length

class(myTweets)
length(myTweets)
head(myTweets)

#Search twitter and print tweet and who tweeted it
ronaldoTweets = searchTwitter("Ronaldo", n=20)

for (i in 1:length(ronaldoTweets)) {
  print(paste("Tweet = ", ronaldoTweets[[i]]$getText()  ))
  print(paste("User = ",  ronaldoTweets[[i]]$getScreenName() ))
}


#################################################################################
#                           Graph of friends and followers
###################################################################################

library(igraph)

start<-getUser('DwightFonseka') 

friends.object<-lookupUsers(start$getFriendIDs())
follower.object<-lookupUsers(start$getFollowerIDs())

# Retrieve the names of your friends and followers from the friend
# and follower objects. You can limit the number of friends and followers by adjusting the 
# size of the selected data with [1:n], where n is the number of followers/friends 
# that you want to visualize. If you do not put in the expression the maximum number of 
# friends and/or followers will be visualized.

n<-10 
friends <- sapply(friends.object[1:n],name)
followers <- sapply(follower.object[1:n],name)

# Create a data frame that relates friends and followers to you for expression in the graph
relations <- merge(data.frame(User='yourname', Follower=friends), 
                   data.frame(User=followers, Follower='yourname'), all=T)

# Create graph from relations.
g <- graph.data.frame(relations, directed = T)

# Assign labels to the graph (=people's names)
V(g)$label <- V(g)$name
E(g)$arrow.size<-0.2
# Plot the graph using plot() or tkplot().

plot(g)

tkplot(g)

rglplot(g)


##########################################################################################
#                 Collecting networks using Social Media Lab
##########################################################################################

# It is currently possible to create 3 different types of networks using Twitter data collected with SocialMediaLab.
# These are (1) actor networks; (2) bimodal networks; and (3) semantic networks. 
# we will focus just on actor networks.

myTwitterData=Authenticate("twitter", apiKey=key,
                            apiSecret=secret,
			                      accessToken=tktoken,
                            accessTokenSecret=tksecret)%>%
Collect(searchTerm="Messi", numTweets=150, writeToFile=F, verbose=T)

# note the data is saved as csv file in your working directory if change
# writeToFile=T

View(myTwitterData)

# errors possibly related to the text of the tweets,
# converting the tweet text to UTF-8 character en
myTwitterData$text <- iconv(myTwitterData$text, to = 'utf-8')


# we will create a semantic network. In this network nodes represent unique concepts (in this case                                                                                    unique terms/words extracted from a set of 150 tweets), and edges represent the co-occurrence of terms for all
# observations in the data set. For example, for this Twitter semantic network, nodes represent either hashtags
# (e.g. “#auspol”) or single terms (“politics”). If there are 150 tweets in the data set (i.e. 150 observations),
# and the term #auspol and the term politics appear together in every tweet, then this would be represented
# by an edge with weight equal to 150.

g_twitter_semantic <- myTwitterData %>% Create("Semantic")
g_twitter_semantic  #igraph graph object

plot(g_twitter_semantic,vertex.shape="none",edge.width=1.5,edge.curved = .5,edge.arrow.size=0.5,asp=9/16,margin=-0.1)



# we will create an actor network. In this actor network, edges represent interactions between Twitter
# users. An interaction is defined as a ‘mention’ or ‘reply’ or ‘retweet’ from user i to user j, given ‘tweet’ m. In
# a nutshell, a Twitter actor network shows us who is interacting with who in relation to a particular hashtag
# or search term.


### this will generate error

#g_twitter_actor <- myTwitterData %>% Create("Actor")
#g_twitter_actor  #igraph graph object

#plot(g_twitter_actor,vertex.shape="none",edge.width=1.5,edge.curved = .5,edge.arrow.size=0.5,asp=9/16,margin=-0.1)




