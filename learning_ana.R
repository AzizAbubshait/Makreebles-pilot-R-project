# to do list:
# 1) calculate percentage for each block separately
# 2) get agency score
# 3) get experience scores

#### Library ####
library(plyr)
library(car)
library(ggplot2)

#### Settings ####
exp_mat=c("maki","hum_w","hum_b")
only_white=0
only_black=1

for (exp in exp_mat){
#### Data Readin ####
  if (exp=="maki"){
    df <- read.csv("dat_maki_edit.csv")
    }
  if (exp=="hum_w"){
    df <- read.csv("dat_white_edit_2.csv")
    }
  if (exp=="hum_b"){
    df <- read.csv("dat_black_edit_2.csv")
    }
df=df[-1,]
df$Duration..in.seconds.=as.numeric(as.character(df$Duration..in.seconds.))

#### Subject Exclusion ####
# exclude subjects that did not finish the study
n_aborted=length(which(df$Finished=="False" | df$Finished==F))
n_finished=length(which(df$Finished=="True" | df$Finished==T))
df=df[-which(df$Finished=="False" | df$Finished==F),]
# exclude subjects that needed less then A and more than B minutes to complete the study
sd_time=sd(df$Duration..in.seconds.)
m_time=mean(df$Duration..in.seconds.)
range_time=range(df$Duration..in.seconds.)
# A=600
# B=1200
A=m_time-2*sd_time
B=m_time+2*sd_time
n_timeoutlier=length(which(df$Duration..in.seconds.<A | df$Duration..in.seconds.>B))
df=df[-which(df$Duration..in.seconds.<A | df$Duration..in.seconds.>B),]
hist(df$Duration..in.seconds.,breaks=seq(400,3600,100))

#### Tidying up data frame: learning task ####
id=NaN 
block=NaN
agent=NaN
trial=NaN
answ=NaN
eth=NaN
df_learn=data.frame(id,block,agent,trial,answ,eth)
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
    eth=df$Ethnicity[which(df$ResponseId==id)]
    df_learn=rbind(df_learn,c(id,block,agent,trial,as.character(answ),as.character(eth)))
  }
  
}
df_learn=df_learn[-1,]
df_learn$cor=as.numeric(df_learn$agent==df_learn$answ)
df_learn$exp=exp

# combine experiments
if (exists("df_learn_all")){
  df_learn_all=rbind(df_learn_all,df_learn)
}
if (!exists("df_learn_all")){
df_learn_all=df_learn
}

}

if (only_white==1){
  df_learn_all=subset(df_learn_all,eth=="White")
}
if (only_black==1){
  df_learn_all=subset(df_learn_all,eth=="Black or African American")
}

#### functions ####
agg_n <- function(x)
  sum(!is.na(x))
agg_sd <- function(x)
  sd(x,na.rm=T)
agg_se <- function(x)
  agg_sd(x)/sqrt(agg_n(x))
agg_m <- function(x)
  mean(x,na.rm=T)

#### data exploration ####
df_learn_id_block_exp=ddply(df_learn_all,.(id,block,exp),summarise,
                            cor_n=agg_n(cor),
                            cor_sd=agg_sd(cor),
                            cor_se=agg_se(cor),
                            cor_m=agg_m(cor))
df_learn_block_exp=ddply(df_learn_id_block_exp,.(block,exp),summarise,
                         cor_n=agg_n(cor_m),
                         cor_sd=agg_sd(cor_m),
                         cor_se=agg_se(cor_m),
                         cor_m=agg_m(cor_m))

#### Graphs ####

ggplot(df_learn_id_block_exp, aes(x=block, y=cor_m, fill=block))+
  geom_bar(stat="summary", fun.y="mean")+
  geom_point(size=2)+
  geom_jitter()+
  facet_grid(exp~.)

ggplot(df_learn_block_exp,aes(block,cor_m),group=exp)+
  geom_point(aes(color=exp,shape=exp),size=2,position=position_dodge(0.5))+
  geom_errorbar(aes(ymin=cor_m-cor_se,ymax=cor_m+cor_se,color=exp),position=position_dodge(0.5))+
  scale_y_continuous(name="Individuation Performance [%]")+
  scale_x_discrete(name="Block [#]")
ggsave("block_x_perf_x_exp.jpg")

                   