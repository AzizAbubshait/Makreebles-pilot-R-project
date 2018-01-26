# to do list:
# 1) calculate percentage for each block separately
# 2) get agency score
# 3) get experience scores

#### Library ####
library(plyr)

#### Data Readin ####
# agency questions:
# experience questions:
df <- read.csv("makreebles data.csv")
df=df[-1,]
df <- df[df$Progress==100, ]

#### Tidying up data frame: learning task ####
id=NaN 
block=NaN
agent=NaN
trial=NaN
answ=NaN
df2=data.frame(id,block,agent,trial,answ)
for (id in unique(df$ResponseId)){
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
df2.id.block=ddply(df2,.(id,block),summarise,
      cor.pct=mean(cor))
df2.block.ga=ddply(df2.id.block,.(block),summarise,
                   cor.pct=mean(cor.pct))


######## Graphs
library(ggplot2)

ggplot(df2.id.block, aes(x=block, y=cor.pct, fill=block))+
  geom_bar(stat="summary", fun.y="mean")+
  geom_point(size=2)
