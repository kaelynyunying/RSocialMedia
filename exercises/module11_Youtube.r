library(SocialMediaLab)
library(magrittr)
library(igraph)
library(bitops)
library(RCurl)
library(plusser)

options(RCurlOptions = 
          list(cainfo = 
                 system.file("CurlSSL", "cacert.pem", 
                             package = "RCurl")))

setAPIkey('AIzaSyAAZjeRlRIyGKqW1aLQBlDgsKmWKM7cjDo')

id=c("SSfactF_SDo")  # starwars: empire strikes back - modern trailer 2

APIkey='AIzaSyAAZjeRlRIyGKqW1aLQBlDgsKmWKM7cjDo'


################## video stats #####################################

library(jsonlite)
getStats <- function(id,APIkey){
  url=paste0("https://www.googleapis.com/youtube/v3/videos?id=",id,"&key=",APIkey,"&part=snippet,contentDetails,statistics,status")
  raw.data <- readLines(url, warn="F") 
  rd  <- fromJSON(raw.data)
  dop  <- rd$items$snippet["publishedAt"]
  title <- rd$items$snippet["title"]
  favs <- rd$items$statistics["favoriteCount"]
  views <- rd$items$statistics["viewCount"]
  dislikes <- rd$items$statistics["dislikeCount"]
  likes <- rd$items$statistics["likeCount"]
  return(data.frame(dop,title,favs,views,dislikes,likes))
}


getStats(id,APIkey)

############################# comments #########################################


base_url <- "https://www.googleapis.com/youtube/v3/commentThreads/"
api_opts <- list(
  part = "snippet",
  maxResults = 100,
  textFormat = "plainText",
  videoId = id,  
  key = APIkey,
  fields = "items,nextPageToken",
  orderBy = "published")

init_results <- httr::content(httr::GET(base_url, query = api_opts))
n=length(init_results[[1]])
n=n-1

ytcomments=data.frame()
for(i in 1:n){ a=init_results$items[[i]]$snippet$topLevelComment$snippet$authorDisplayName
                b=init_results$items[[i]]$snippet$topLevelComment$snippet$textDisplay
                results=data.frame(a,b)
                ytcomments=rbind(ytcomments,results)
}

names(ytcomments)=c("username","comment")
head(ytcomments)

############################################################################
#			Collecting network using social media lab
############################################################################

g_youtube_actor <- Authenticate("youtube", apiKey= APIkey) %>%
                   Collect(videoIDs =id, writeToFile=F) %>%
                   Create("Actor")

#A description of the network:
g_youtube_actor

# A plot of the network:
plot(g_youtube_actor,edge.width=1.5,edge.curved=.5,edge.arrow.size=0.5)
