library(tidyverse)
#load data
dsimplt<-read.csv("~/DempsSocialLearningABM/DempsSocialLearningABM/Data/LinePlotData/StartingProportionsNoFavor/PropStart_08.csv")
dsimplt<-read.csv("~/DempsSocialLearningABM/DempsSocialLearningABM/Data/LinePlotData/datSINGLE.csv")

#make the median line df
dsimpltMed<-dsimplt%>%group_by(LearningType,EnvironmentalChangeRate)%>%summarise(medProp=median(PropPop))%>%
  ungroup()%>%mutate(run=rep("1",40))

#Old plot style
#ggplot(dsimplt,aes(x=EnvironmentalChangeRate,y=PropPop, fill=as.factor(run)  ))+
#  geom_line(aes(color=LearningType),size=2,alpha=0.6)+#geom_point(aes(shape=LearningType),size=4)+
#  geom_line(data=dsimpltMed,aes(x=EnvironmentalChangeRate,group=LearningType,y=medProp),linetype=2,size=2)+
#  theme_classic()+mytheme+#scale_color_manual(values=cols)
#  ylim(0,1)+
#  ylab("Proportion of population")+xlab("Rate of environmental change")+
#  scale_colour_viridis_d( labels=c("Content biased copying","Individual learning", 
 #                                  "Unbiased copying" ,
  #                                 "Kin biased copying",
      #                             "Success biased copying"),name="Learning type")



ggplot(dsimplt,aes(x=EnvironmentalChangeRate,y=PropPop  ))+
  geom_point(aes(color=LearningType,fill=LearningType),size=4,shape=21,stroke=2)+#geom_point(aes(shape=LearningType),size=4)+
  geom_line(data=dsimpltMed,aes(x=EnvironmentalChangeRate,color=LearningType,y=medProp),alpha=0.75,linetype=2,size=2,show.legend = FALSE )+
  theme_classic()+mytheme+#scale_color_manual(values=cols)
  ylim(0,1)+
  ylab("Proportion of population")+xlab("Rate of environmental change")+
  scale_color_manual(values=palette.colors(n = 5,palette = "R4",alpha=0.85),
                     labels=c("Content biased copying","Individual learning","Unbiased copying" ,
                      "Kin biased copying","Success biased copying"),name="Learning type")+
scale_fill_manual(values=palette.colors(n = 5,palette = "R4",alpha=0.25),
                  labels=c("Content biased copying","Individual learning","Unbiased copying" ,
                           "Kin biased copying","Success biased copying"),name="Learning type")
  
