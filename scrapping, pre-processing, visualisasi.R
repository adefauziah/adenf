library(rvest)
library(xml2)
alamatweb<-'https://www.imdb.com/search/title/?count=250&release_date=2015,2019&title_type=feature'
lamanweb<-read_html(alamatweb)
lamanweb
rank_data_html <- html_nodes(lamanweb,'.text-primary')
rank_data <- html_text(rank_data_html)
rank_data
rank_data<-as.numeric(rank_data)

title_data_html <- html_nodes(lamanweb,'.lister-item-header a')
title_data <- html_text(title_data_html)
title_data

runtime_data_html <- html_nodes(lamanweb,'.runtime')
runtime_data <- html_text(runtime_data_html)
runtime_data
runtime_data<-gsub(" min","",runtime_data)
runtime_data<-as.numeric(runtime_data)
runtime_data

genre_data_laman<-html_nodes(lamanweb,'.genre')
genre_data_laman
genre_data<-html_text(genre_data_laman)
genre_data
genre_data<-gsub("\n","",genre_data)
genre_data<-gsub(" ","",genre_data)
genre_data<-gsub(",.*","",genre_data)
genre_data
genre_data<-as.factor(genre_data)

rating_data_laman<-html_nodes(lamanweb,'.ratings-imdb-rating strong')
rating_data<-html_text(rating_data_laman)
rating_data
rating_data<-as.numeric(rating_data)

metascore_data_html <- html_nodes(lamanweb,'.metascore')
metascore_data <- html_text(metascore_data_html)
metascore_data
metascore_data<-gsub(" ","",metascore_data)
metascore_data<-as.numeric(metascore_data)
for (i in c(118,122,132,134,155,161,196,206)){
  a<-metascore_data[1:(i-1)]
  b<-metascore_data[i:length(metascore_data)]
  metascore_data<-append(a,list("NA"))
  metascore_data<-append(metascore_data,b)
}
metascore_data<-as.numeric(metascore_data)

votes_html <- html_nodes(lamanweb,'.sort-num_votes-visible span:nth-child(2)')
votes<- html_text(votes_html)
votes
votes<-gsub(",","",votes)
votes<-as.numeric(votes)

gross_data_laman<-html_nodes(lamanweb,'.ghost~ .text-muted+ span')
gross_data<-html_text(gross_data_laman)
gross_data
gross_data<-gsub("M","",gross_data)
gross_data<-substring(gross_data,2,7)
gross_data<-as.numeric(gross_data)
gross_data<-prepend(gross_data,"NA")

for (i in c(2,5:7,14,16,22,40,44,57,61,67,71,75,78,93,96,97,98,108,109,112,118,120,122,127,129,132:134,137,152,155,161:163,166:168,181,184,189,195,196,202,206,215,221,227,228,230:232,236,241,242,246,248,249)){
  a<-gross_data[1:(i-1)]
    b<-gross_data[i:length(gross_data)]
    gross_data<-append(a,list("NA"))
    gross_data<-append(gross_data,b)
  }
gross_data<-as.numeric(gross_data)

directors_data_html <- html_nodes(lamanweb,'.text-muted+ p a:nth-child(1)')
directors_data <- html_text(directors_data_html)
head(directors_data)
directors_data<-as.factor(directors_data)
head(directors_data)

actors_data_html <- html_nodes(lamanweb,'.lister-item-content .ghost+ a')
actors_data <- html_text(actors_data_html)
head(actors_data)
actors_data<-as.factor(actors_data)
head(actors_data)

data=data.frame(title=title_data,rank=rank_data,run_time=runtime_data,genre=genre_data,gross=gross_data,rating=rating_data,metascore=metascore_data,votes=votes,directors=directors_data,actor=actors_data)
str(data)
write.csv(data,"F:/easba.csv",row.names = FALSE)

#preprocess
data$genre<-as.factor(data$genre)
data$gross<-as.numeric(data$gross)
data$rating<-as.numeric(data$rating)
data$directors<-as.factor(data$directors)
levels(data$genre)
data$genre <- factor(data$genre, levels = c('Action','Adventure','Animation','Biography','Comedy','Crime','Drama','Family','Fantasy','Horror'),  labels = c(1:10))
str(data)
library(dplyr)
data %>% group_by(data$genre) %>% dplyr::summarize(Mean=mean(data$gross,na.rm=TRUE))
data %>% group_by(data$genre) %>% summarise_at(vars(data$gross),list(name=mean))
data$gross<-ave(data$gross,data$genre,FUN=function(x) 
  ifelse(is.na(x), mean(x,na.rm=TRUE), x))
data$metascore<-ave(data$metascore,data$genre,FUN=function(x) 
  ifelse(is.na(x), mean(x,na.rm=TRUE), x))
data
library(tidyverse)
data$gross <- ifelse(is.na(data$gross),mean(data$gross,na.rm=TRUE),data$gross) #khusus genre fantasy
write.csv(data,"F:/data hasil preprocess.csv",row.names = FALSE)

data=read.csv("F:/data hasil preprocess 1.csv",sep=",",dec=".")

library(corrplot)
colnames(data)=c("title","rank","run time","genre","gross","rating","meta score","votes")
correlation <- cor(data[,c(3,5,6,7,8)])
corrplot(correlation, method = "number", type = "lower", diag = TRUE,tl.cex=1,tl.col = "black")

library(ggplot2)
g1=ggplot(data,aes(genre,metascore,fill=genre))+geom_boxplot(varwidth=T,show.legend = FALSE)+labs(y="Metascore")+theme(legend.key = element_blank())+theme_classic();g1
g2=ggplot(data,aes(genre,rating,fill=genre))+geom_boxplot(varwidth=T,show.legend = FALSE)+labs(y="Rating")+theme_classic();g2
g2=ggplot(data,aes(genre,gross,fill=genre))+geom_boxplot(varwidth=T,show.legend = FALSE)+labs(y="Gross")+theme_classic();g2
g2=ggplot(data,aes(genre,votes,fill=genre))+geom_boxplot(varwidth=T,show.legend = FALSE)+labs(y="Votes")+theme_classic();g2
g2=ggplot(data,aes(genre,run_time,fill=genre))+geom_boxplot(varwidth=T,show.legend = FALSE)+labs(y="Run time")+theme_classic();g2

str(data)
data$genre=as.factor(data$genre)
panel.cor <- function(x, y)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- round(cor(x, y), digits=2)
  txt <- paste0("R = ", r)
  cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}
# Customize upper panel
upper.panel<-function(x, y)
{
  points(x,y, pch = 19, col = data[data$genre])
}
# Create the plots
pairs(data[,c(3,5,6,7,8)], 
      lower.panel = panel.cor,
      upper.panel = upper.panel)


# Customize upper panel
lower.panel<-function(x, y)
{
  points(x,y, pch=19, col=data$genre)
  r <- round(cor(x, y), digits=2)
  txt <- paste0("R = ", r)
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  text(0.5, 0.9, txt)
}
pairs(data[,c(3,5,6,7,8)], lower.panel = lower.panel, 
      upper.panel = NULL,cex.labels = 1.5)

pairs(data[,c(3,5,6,7,8)],col=data$genre,cex.labels = 1.5,upper.panel = NULL)





