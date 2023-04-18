library(tidyverse)

Simplot<-function(dsim){
  dsimplt<-dsim%>%mutate(EnvironmentalChangeRate=u,
                         Independant=p,
                         UnbiasedCopying= (1-p)*p2,
                         ContentBiasedCopying = (1-p)*(1-p2)*p3,
                         KinBiasedCopying = (1-p)*(1-p2)*(1-p3)*(1-p4),
                         SuccessBiasedCopying = (1-p)*(1-p2)*(1-p3)*p4 )%>%
    select(EnvironmentalChangeRate,Independant,UnbiasedCopying,ContentBiasedCopying,
           KinBiasedCopying,SuccessBiasedCopying)%>%
    tidyr::pivot_longer(Independant:SuccessBiasedCopying,names_to = "LearningType",values_to = "PropPop")
  
  cols<-c("Independant" = "black", 
          "Unbiased Copying" = "#a6cee3",
          "Content Biased Copying" = "#1f78b4",
          "Kin Biased Copying"= "#b2df8a",
          "Success Biased Copying"="#33a02c")
  
  mytheme<- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  plot.title = element_text( size=18, color="black",face="bold"),
                  axis.title.x = element_text( size=18),
                  axis.title.y = element_text( size=18),
                  axis.text=(element_text(color="black", size=14)),
                  legend.title = element_text(colour="black", size=18),
                  legend.text = element_text( size = 14))
  
  ggplot(dsimplt,aes(x=EnvironmentalChangeRate,y=PropPop))+
    geom_line(aes(color=LearningType ),size=3)+#geom_point(aes(shape=LearningType),size=4)+
    theme_classic()+mytheme+#scale_color_manual(values=cols)
    ylab("Proportion of Population")+xlab("Rate of Environmental Change")+
    scale_colour_viridis_d( labels=c("Content Biased Copying","Independent", 
                                     "Unbiased Copying" ,
                                     "Kin Biased Copying",
                                     "Success Biased Copying"),name="Learning Type")
}
