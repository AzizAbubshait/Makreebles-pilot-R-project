# to do list:
# 1) calculate percentage for each block separately
# 2) get agency score
# 3) get experience scores
# 4) @Aziz - check out labels

#### Library ####
library(plyr)
library(car)
library(ggplot2)

#### Data Readin ####
# agency questions:
# experience questions:
df <- read.csv("makreebles data.csv")
df=df[-1,]
df$Duration..in.seconds.=as.numeric(as.character(df$Duration..in.seconds.))

#### Subject Exclusion ####
# exclude subjects that did not finish the study
n_aborted=length(which(df$Finished==F))
n_finished=length(which(df$Finished==T))
df=df[-which(df$Finished==F),]
# exclude subjects that needed less then A and more than B minutes to complete the study
sd_time=sd(df$Duration..in.seconds.)
m_time=mean(df$Duration..in.seconds.)
range_time=range(df$Duration..in.seconds.)
A=720
B=2700
# B=m_time+2*sd_time
n_timeoutlier=length(which(df$Duration..in.seconds.<A | df$Duration..in.seconds.>B))
df=df[-which(df$Duration..in.seconds.<A | df$Duration..in.seconds.>B),]
hist(df$Duration..in.seconds.,breaks=seq(500,2700,250))

#### Tidying up data frame: learning task ####
id=NaN 
block=NaN
agent=NaN
trial=NaN
answ=NaN
df_learn=data.frame(id,block,agent,trial,answ)
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
    df_learn=rbind(df_learn,c(id,block,agent,trial,as.character(answ)))
  }
  
}
df_learn=df_learn[-1,]
df_learn$cor=as.numeric(df_learn$agent==df_learn$answ)

#### Tidying up data frame: ratings ####
id=NaN 
agent=NaN
preexposed=NaN
quest=NaN
scale=NaN
answ=NaN
df_rating=data.frame(id,agent,preexposed,quest,scale,answ)
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
    df_rating=rbind(df_rating,c(id,agent,preexposed,quest,scale,as.character(answ)))
  }
}
df_rating=df_rating[-1,]
df_rating$answ=as.numeric(recode(df_rating$answ,"'Extremely unlikely'=1;'Moderately unlikely'=2;'Slightly unlikely'=3;'Neither likely nor unlikely'=4;'Slightly likely'=5;'Moderately likely'=6;'Extremely likely'=7"))
df_rating=df_rating[-which(df_rating$agent=="F1B2"),]

#### data exploration ####
df_learn_id_block=ddply(df_learn,.(id,block),summarise,
                        cor=mean(cor))

# add binary variably splitting the population in good and bad individuators (if are at least at the 50th percentile in block 5, thez are good learners!)
hist(subset(df_learn_id_block,block==5)$cor,breaks=seq(0,1,.05))
mean_cor_b5=mean(subset(df_learn_id_block,block==5)$cor)
pct_cor_b5=quantile(subset(df_learn_id_block,block==5)$cor,.85)  # top 15%

df_learn_id_block$goodlearner=NA
for (ID in unique(df_learn_id_block$id)){
df_learn_id_block$goodlearner[df_learn_id_block$id==ID]=df_learn_id_block$cor[df_learn_id_block$id==ID & df_learn_id_block$block==5]>=mean_cor_b5
df_learn_id_block$amazinglearner[df_learn_id_block$id==ID]=df_learn_id_block$cor[df_learn_id_block$id==ID & df_learn_id_block$block==5]>=pct_cor_b5
}

df_learn_id=ddply(df_learn_id_block,.(id),summarise,
                  cor=mean(cor),
                  goodlearner=unique(goodlearner),
                  amazinglearner=unique(amazinglearner))

df_learn_block_ga=ddply(df_learn_id_block,.(block),summarise,
                   cor=mean(cor))

df_rating_id_scale_pre=ddply(df_rating,.(id,scale,preexposed),summarise,
                         rat_n=sum(!is.na(answ)),
                         rat_m=mean(answ,na.rm=T))

# merge data frames
df_id_scale_pre=merge(df_learn_id,df_rating_id_scale_pre,by.x="id")

df_scale_pre_gl=ddply(df_id_scale_pre,.(preexposed,scale,goodlearner),summarise,
                      rat_n=sum(!is.na(rat_m)), 
                      rat_sd=sd(rat_m,na.rm=T),
                      rat_se=rat_sd/sqrt(rat_n),
                      rat_m=mean(rat_m,na.rm=T))
df_scale_pre_al=ddply(df_id_scale_pre,.(preexposed,scale,amazinglearner),summarise,
                      rat_n=sum(!is.na(rat_m)), 
                      rat_sd=sd(rat_m,na.rm=T),
                      rat_se=rat_sd/sqrt(rat_n),
                      rat_m=mean(rat_m,na.rm=T))


######## Graphs

ggplot(df_learn_id_block, aes(x=block, y=cor, fill=block))+
  geom_bar(stat="summary", fun.y="mean")+
  geom_point(size=2)+
  geom_jitter()

ggplot(df_scale_pre_gl,aes(scale,rat_m,fill=preexposed))+
  geom_bar(stat='identity',position = position_dodge())+
  geom_errorbar(aes(ymin=rat_m-rat_se,ymax=rat_m+rat_se),position = position_dodge())+
  facet_grid(goodlearner~.,labeller=label_both)
ggplot(df_scale_pre_al,aes(scale,rat_m,fill=preexposed))+
  geom_bar(stat='identity',position = position_dodge())+
  geom_errorbar(aes(ymin=rat_m-rat_se,ymax=rat_m+rat_se),position = position_dodge())+
  facet_grid(amazinglearner~.,labeller=label_both)




df_rating_fix_excel=ddply(df_rating,.(agent,quest),summarise,
                          n=sum(!is.na(answ)))

       