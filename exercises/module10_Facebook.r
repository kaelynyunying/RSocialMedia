library(Rfacebook)
library(Rook)
require(RCurl)
require(rjson)
library(jsonlite)
library(httpuv)
library(RColorBrewer)
library(SocialMediaLab)
library(magrittr)
library(igraph)


#Get facebook for handle through this website : http://findmyfbid.com/

# option 1
# this key can expire > but can regenerate
APIkey="EAACEdEose0cBAHZBXvsbnFbrRj88aZCjanIE34kSqaATaLJu0k6yt0TNbGcXCvTNNZAfxzN4nK12XVWJSPV4MWF3XzvVCPyeOYoIYDDO4uREvRPXJnI1cEyZB1DhHFIKeXuMZBdZAMXqt26Hz2NOZB5tlYwBd6QJT6dMFzz3VJ8ZCPETjGCImeEi"


# option 2
app_id="236845553454666"
app_secret="7d452777f8c0c97546f9c09f75ef7381"
fb_oauth=fbOAuth(app_id="236845553454666", app_secret="7d452777f8c0c97546f9c09f75ef7381", extended_permissions = TRUE)
# a message will appear > copy the URL and go to setting of the facebook app. click
## settings tab on left side and choose "add platform" (at bottom)
# add the url in the fiel "site url" and save
save(fb_oauth, file="C://Users//user//Desktop//fb_oauth")
load("C://Users//user//Desktop//fb_oauth")



myDetails=getUsers("733992754", token=APIkey, private_info=TRUE)
str(myDetails)

myDetails$name
myDetails$id
myDetails$hometown


likes=getLikes('me', n=500, token=APIkey)


#Get info about my friends
myFriends=getFriends(token=APIkey, simplify=TRUE)
str(myFriends)
#only friends who are using the application that you used to generate the 
#token to query the API will be returned.

for (i in 1:nrow(myFriends)) {
  print(paste( "Name=", myFriends$name[i],
               "Gender=", myFriends$gender[i],
               "locale=", myFriends$locale[i],
               "relationship=", myFriends$relationship_status[i]))
}


table(myFriends$relationship_status)


pie(table(myFriends$relationship_status),col=brewer.pal(5, "Set1"))
pie(table(myFriends$location),col=brewer.pal(20, "Greens"))
pie(table(myFriends$locale),col=brewer.pal(4, "Blues"))
pie(table(myFriends$gender),col=brewer.pal(3, "Oranges"))


myNetwork1=getNetwork(token=APIkey, format = "edgelist", verbose = TRUE)
myNetwork1

myNetwork2 = getNetwork(token=APIkey, format = "adj.matrix", verbose = TRUE)
myNetwork2

#Using REST API directly and building query with Graph API explorer
# use FacebookID:733992754

# get DwightFonseka details
                           # can change to another user > just find the FaceBookID

baseURL = "https://graph.facebook.com/v2.8/733992754"
queryFields="id,name,about,age_range,birthday"
fullURL=paste0(baseURL,"?fields=",queryFields,"&access_token=",APIkey)

dwightDetails=fromJSON(fullURL)
for ( i in 1:length(dwightDetails$data)) {
  print(dwightDetails$data$name)
  print(dwightDetails$data$age_range)
  print(dwightDetails$data$birthday)
}

dwightDetails

queryFields="id,name,about,events.limit(10).fields(attending_count,start_time,description)"
# copy from field in Graph API page
fullURL=paste0(baseURL,"?fields=",queryFields,"&access_token=",APIkey)


dwightEvents=fromJSON(fullURL)
for ( i in 1:length(dwightEvents$events$data$id)) {
  print(dwightEvents$events$data$description[i])
  print(dwightEvents$events$data$attending_count[i] )
}

dwightEvents

# for streaming newsfeed
newsFeed=getNewsfeed(token=fb_oauth, n=1)


########################## analysing network of friends ############################

access_token <- APIkey

facebook <-  function( path = "me", access_token = token, options){
  if( !missing(options) ){
    options <- sprintf( "?%s", paste( names(options), "=", unlist(options), collapse = "&", sep = "" ) )
  } else {
    options <- ""
  }
  url <- sprintf( "https://graph.facebook.com/%s%s&access_token=%s", path, options, access_token )
  # print(url)
  data <- getURL( url )
  fromJSON( data )
}


# scrape the list of friends

myself <- facebook(path="me", access_token=access_token, options=list("fields"="id,name,location,hometown,gender,friends.fields(location,hometown,name,gender)"))
myname <- myself$name
friends <- myself$friends

# extract Facebook IDs

friends.id <- sapply(friends[1], function(x) x$id)
# extract names 
friends.name <- unlist(sapply(friends[1], function(x)  iconv(x$name,to='ASCII',sub="")))
friends.gender <- sapply(friends[1], function(x) unlist(x$gender))
friends.location.id <- unlist(sapply(friends[1], function(x) x$location$id))
friends.location.name <- unlist(sapply(friends[1], function(x) {if (is.null(x$location$id)) "none" else iconv(x$location$name,to='ASCII',sub="")}))
friends.hometown.name <- unlist(sapply(friends[1], function(x) {if (is.null(x$hometown$id)) "none" else iconv(x$hometown$name,to='ASCII',sub="")}))
friendlists <- facebook( path="me/friendlists", access_token=access_token)
friendlists.id <- sapply(friendlists$data, function(x) x$id)
friendlists.name <- sapply(friendlists$data, function(x) x$name)


# friendship relation matrix

friendships <- function() {
    print("Generating friendship matrix")
    N <- length(friends.id)
    friendship.matrix <- matrix(0,N,N)
  
    for (i in 1:N) {
          print(paste(i,friends.name[i]))
          tmp <- facebook( path=paste("me/mutualfriends", friends.id[i], sep="/") , access_token=access_token)
          mutualfriends <- sapply(tmp$data, function(x) x$id)
          friendship.matrix[i,friends.id %in% mutualfriends] <- 1
}
# Create a vector of 1's for my connections with my friends
     myfriends <- c(1:N)
     myfriends[] <-1
# Add this vector as a column to the friendship matrix
friendship.matrix <- cbind(friendship.matrix,myfriends)
# Append my friendship with myself (0 in this case), and add it as a row
friendship.matrix <- rbind(friendship.matrix,append(myfriends,0))
print(paste("Added",myname))
rownames(friendship.matrix) <- append(friends.name,myname)
colnames(friendship.matrix) <- append(friends.name,myname)
return (friendship.matrix)
}


frienddata <- function() {
       friend_data <- data.frame("name"=friends.name)
       friend_data["location"] <- friends.location.name
       friend_data["hometown"] <- friends.hometown.name
       return(friend_data)
}




friendgraph <- function(friendship.matrix) {
        friendship.graph <- graph.adjacency(friendship.matrix, mode=c("undirected"), weighted=NULL)
        V(friendship.graph)$gender <- append(friends.gender, myself$gender)
        V(friendship.graph)$location <- append(friends.location.name, myself$location$name)
        V(friendship.graph)$hometown <- append(friends.hometown.name, myself$hometown$name)
        V(friendship.graph)$Label <- V(friendship.graph)$name
        V(friendship.graph)$kind <- "friend"
        return(friendship.graph)
}


friendship.matrix=friendships()
g=friendgraph(friendship.matrix)
class(g)

##########################################################################################################
#                       Collecting Network using Social Media Lab
###########################################################################################################

#Note: for Facebook, SocialMediaLab currently only supports creating bimodal and dynamic networks.

# We will create a bimodal network where edges represent relationships between nodes of two different types. 
# For example, in our bimodal Facebook network, nodes represent Facebook users or Facebook posts, and edges 
# represent whether a user has commented or ?liked? a post. Edges are directed and weighted 
# (e.g. if user i has commented n times on post j, then the weight of this directed edge equals n).


g_bimodal_facebook_nike <- Authenticate("Facebook",
                                       appID=app_id, appSecret=app_secret) %>%
                                        SaveCredential("FBCredential.RDS") %>%
                                        Collect(pageName="StarWars", rangeFrom="2015-05-01",rangeTo="2016-06-03") %>%
                                        Create("Bimodal")


# note the data is saved as csv file in your working directory if change
# writeToFile=T


### We can save the graph object as a graphml file (so can be visualised using software such as Gephi) or for later use in R.

write.graph(g_bimodal_facebook_nike, "g_bimodal_facebook_nike.graphml",format="graphml")

# This can be read back into R as follows (note: to do this, you may have to compile R with XML support):

g <- read.graph("g_bimodal_facebook_nike.graphml", format="graphml")

################ common formats ###############
# edgelist > simple text file with one edge per line
# pajek > popular windows program for network analysis
# gml > graph modelling language is a common text based open format
# graphml > graph markup language is an XML based open format
# dot > format used by GraphViz

# Gephi > to export to GEXF fformat use the rgexf package
#################################################



##################### analysing a facebook page ########################3

myfacebookpage="nike"

page=getPage(myfacebookpage, APIkey, n=5000)  # set n to high value to caputre all posts

page2=getPage(myfacebookpage, APIkey, n=5000, since='2015/01/01', until='2015/12/31')


# convert Facebook data metrics

format.facebook.date=function(datestring) {
                      date=as.POSIXct(datestring, format="%Y-%m-%dT%H:%M:%S+0000", tz="GMT")
}

aggregate.metrics=function(metric){
                     m=aggregate(page[[paste0(metric, "_count")]], list(month=page$month),mean)
                     m$month=as.Date(paste0(m$month, "-15"))
                     m$metric=metric
                     return(m)
}

# create a data frame
page$datetime=format.facebook.date(page$created_time)
page$month=format(page$datetime, "%Y-%m")
df.list=lapply(c("likes","comments","shares"), aggregate.metrics)
df=do.call(rbind, df.list)

# visualize results
library(ggplot2)
library(scales)
ggplot(df, aes(x=month, y=x, group=metric)) + geom_line(aes(color=metric)) + scale_x_date(date_breaks="years", labels=date_format("%Y")) + scale_y_log10("Avearge count per post", breaks=c(10,100,1000,10000,50000))+ theme_bw() + theme(axis.title.x= element_blank())


############################ analysing posts #################################

post_id=head(page$id, n=1)  # get ID of most recent post
post=getPost(post_id, token, n=1000, likes=TRUE, comments=FALSE)
# this collects a list of 1000 users who like the most recent post > gender, language, country


# unless the users have public profiles, most of their information will be returned as NA
page_posts <- getPage(page="nike", token=APIkey, n=20, feed=TRUE)
other_users <- getUsers(page_posts$from_id, token=APIkey, private_info=TRUE)


###### scapping comments


# if you wish to find the data and focus on particlar topic or keyword
subset_data=subset(page, grepl("Sports", page$message))

#### function to scrape all the comments on a page

test= list()

for (i in 1:length(page$id)){
  test[[i]]=getPost(post=page$id[i], token = APIkey, comments = TRUE, likes = FALSE)
  if (nrow(test[[i]][["comments"]]) > 0) {
  write.csv(test[[i]], file = paste0("test", i, ".csv"), row.names = F)
   }
}

test.all <- do.call(rbind.data.frame, test)