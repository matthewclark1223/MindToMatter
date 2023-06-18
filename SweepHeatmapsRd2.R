library(dplyr)
library(ggplot2)
#dat<-read.csv("~/DempsSocialLearningABM/DempsSocialLearningABM/Data/ParameterSweeps/PopVariationSweepCaseStudy.csv") # mu of 0.001
#dat<-read.csv("~/DempsSocialLearningABM/DempsSocialLearningABM/Data/ParameterSweeps/PopVariationSweepCaseStudyHighPressureb2.csv") # mu of 0.001
dat<-read.csv("~/DempsSocialLearningABM/DempsSocialLearningABM/Data/ParameterSweeps/PopVariationSweepCaseStudyHighPressureb3.csv") # mu of 0.001
#dat<-read.csv("PopVariationSweep_005Mu.csv") #mu of 0.005

#dat<-FillDat

dat%>%mutate(IndMinusContent=Independant-Content)%>%
  group_by(PopSize,EnvVar)%>%summarize(IndMinusContent=mean(IndMinusContent))%>%

ggplot(.,aes(x=as.factor(EnvVar),y=as.factor(PopSize),fill=IndMinusContent))+
  geom_tile()+
 
  scale_fill_viridis_c(name="IndMinusContent" )+theme_bw()+
  theme(panel.grid = element_blank(),axis.text = element_text(size=14,color="black"),
        axis.title = element_text(color="black",size=18),
        legend.text = element_text(size=12,color="black"),
        legend.title = element_text(size=14,color="black"))+
  ylab("Population size")+xlab("Environmental variation")


dat%>%mutate(SocialLearn=Success+Content+Unbiased)%>%
  mutate(IndMinusSocial=Independant-SocialLearn)%>%
  group_by(PopSize,EnvVar)%>%summarize(IndMinusSocial=mean(IndMinusSocial))%>%
  
  ggplot(.,aes(x=as.factor(EnvVar),y=as.factor(PopSize),fill=IndMinusSocial))+
  geom_tile()+
  
  scale_fill_viridis_c(name="Difference in individual\nand social learning" )+theme_bw()+
  theme(panel.grid = element_blank(),axis.text = element_text(size=14,color="black"),
        axis.title = element_text(color="black",size=18),
        legend.text = element_text(size=12,color="black"),
        legend.title = element_text(size=14,color="black"))+
  ylab("Population size")+xlab("Rate of environmental change")



#look at variability

dat%>%mutate(SocialLearn=Success+Content+Unbiased)%>%
  mutate(IndMinusSocial=Independant-SocialLearn)%>%
  ggplot(.,aes(x=PopSize,y=IndMinusSocial))+
  geom_point(aes(color=as.factor(Run)))+facet_wrap(~as.factor(EnvVar))
  
  
  
  
dat%>%mutate(SocialLearn=Success+Content+Unbiased)%>%
  mutate(IndMinusSocial=Independant-SocialLearn)%>%
  group_by(PopSize,EnvVar)%>%summarize(OutcomeVar=sd(IndMinusSocial))%>%
  
  ggplot(.,aes(x=EnvVar,y=as.factor(PopSize),fill=OutcomeVar))+
  geom_tile()+
  
  scale_fill_viridis_c(name="OutcomeVar" )+theme_bw()+
  theme(panel.grid = element_blank(),axis.text = element_text(size=14,color="black"),
        axis.title = element_text(color="black",size=18),
        legend.text = element_text(size=12,color="black"),
        legend.title = element_text(size=14,color="black"))+
  ylab("Population size")+xlab("Environmental variation")
