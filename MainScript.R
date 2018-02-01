# to do list:
# 1) calculate percentage for each block separately
# 2) get agency score
# 3) get experience scores
# 4) @Aziz - check out labels

#### Library ####
library(plyr)
library(car)

#### Data Readin ####
# agency questions:
# experience questions:
df <- read.csv("makreebles data.csv")
df=df[-1,]

#### Tidying up data frame: learning task ####
id=NaN 
block=NaN
agent=NaN
trial=NaN
answ=NaN
df2=data.frame(id,block,agent,trial,answ)
for (id in unique(df$ResponseId)){
  
  # Learning Task
  for (name in names(df)[24:123]){
    block=as.integer(substr(name,2,2))
    agent=substr(name,4,4)
    if (is.na(as.integer(substr(name,6,6)))) {
      trial=0
    }
    if (!is.na(as.integer(substr(name,6,6)))) {
      trial=as.integer(substr(name,6,6))
    }
    answ=df[which(df$ResponseId==id),which(names(df)==name)]
    df2=rbind(df2,c(id,block,agent,trial,as.character(answ)))
  }
  
}
df2=df2[-1,]
df2$cor=as.numeric(df2$agent==df2$answ)

#### Tidying up data frame: ratings ####
id=NaN 
agent=NaN
preexposed=NaN
quest=NaN
scale=NaN
answ=NaN
df3=data.frame(id,agent,preexposed,quest,scale,answ)
for (id in unique(df$ResponseId)){
  
  # Ratings
  for (name in names(df)[124:303]){
    agent=substr(name,1,4)
    if (as.integer(substr(name,2,2))==5){
      preexposed=1
    }
    if (!as.integer(substr(name,2,2))==5) {
      preexposed=0
    }
    quest=substr(name,6,nchar(name))
    if (is.element(quest,c("fear","hunger","joy","pain","pleasure","desire",
                           "personality","conscious","rage","embarrass","pride"))) {
      scale="experience"
    }
    if (is.element(quest,c("communication","morality","emoRecog","planning","selfCont",
                           "thought", "memory" ))) {
      scale="agency"
    }
    answ=df[which(df$ResponseId==id),which(names(df)==name)]
    df3=rbind(df3,c(id,agent,preexposed,quest,scale,as.character(answ)))
  }
}
df3=df3[-1,]
df3$answ=as.numeric(recode(df3$answ,"'Extremely unlikely'=1;'Moderately unlikely'=2;'Slightly unlikely'=3;'Neither likely nor unlikely'=4;'Slightly likely'=5;'Moderately likely'=6;'Extremely likely'=7"))

#### data exploration ####
df2.id.block=ddply(df2,.(id,block),summarise,
                   cor.pct=mean(cor))
df2.block.ga=ddply(df2.id.block,.(block),summarise,
                   cor.pct=mean(cor.pct))
df3.preexposed.scale=ddply(df3,.(preexposed,scale),summarise,
                           rat=mean(answ,na.rm=T))


######## Graphs
library(ggplot2)

ggplot(df2.id.block, aes(x=block, y=cor.pct, fill=block))+
  geom_bar(stat="summary", fun.y="mean")+
  geom_point(size=2)+
  geom_jitter()
