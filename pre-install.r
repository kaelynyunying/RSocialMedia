
install.packages(c("devtools","rjson","bitops","plusser","bit64",
                   "httr","RCurl","ROAuth","jsonlite","ggplot2","scales",
                   "igraph","semver","stringr","jsonlite","tm","SnowballC",
                   "graph","Rgraphviz","wordcloud","arules","fpc","caret",
                   "lattice","devtools","igraphdata","sna","statnet",
                   "rgl","linkcomm","magittr","SocialMediaLab"), dependencies=T)


Sys.sleep(250)

library(devtools)
install_github("geoffjentry/twitteR")
Sys.sleep(50)


install_github("pablobarbera/Rfacebook/Rfacebook")