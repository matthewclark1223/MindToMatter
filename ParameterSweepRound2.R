#source("PlotFunction.r")
# katie d basic social learning model
set.seed(111)
fclip <- function(x,lo=0,hi=1) ifelse( x < lo , lo , ifelse( x > hi , hi , x ) ) #constrains to 0:1

# steady state mean p for this model is supposed to be E(p) = u(s-k)/(k(1-u))
simrogers <- function( tmax=1000 , b=1 , k=0.75 , u=.01, w0=5 , n=100 , 
                       mu=0.001 , s=0.01 , j=0.9, m=0.01, l=0.9, f=0.9, apt=0.9, 
                       pw = 0, qs = 10, plot=TRUE, sv=0.2 ) {
  # tmax : number of generations to simulate
  # b : benefit of adaptive behavior
  # k : cost of individual learning, as proportion of b
  # u : chance environment changes
  # W0: Baseline fitness
  # n : population size
  # mu: mutation size
  # s : success rate of individual learning
  # l : cost of random copying
  # j : cost of evaluating content
  # m : cost of success bias
  # f : cost of copying parent
  #apt: Probability that success biased learning leads to appropriate variant
  # pw: Scales probability that successful individuals copied  (leave at 0)
  # qs: sample size of individuals observable for content bias
  # sv: starting variance of the initialized population. Matt changed from using mutation size
  
  # init history variables
  ph <- {}
  p2h <-{}
  p3h <-{}
  p4h <-{}
  qh <- {}
  wh <- {}
  
  # initialize population
  # p is heritable prob of individual learning
  # p2 is heritable prob of unbiased copying
  # p3 is heritable prob of using content bias
  # p4 is heritable prob of using success or kin bias
  # q is 0,1 adaptive value of behavior
  p <- rnorm( n , 0.5 , sv ) #Mean of each is 0.5, variance is the mutation size (we might consider changing that to be a seperate parameter)
  p <- fclip(p) # All values between 0 and 1
  p2 <- rnorm( n, 0.5, sv )
  p2 <- fclip(p2)
  p3 <- rnorm( n, 0.5, sv )
  p3 <- fclip(p3)
  p4 <- rnorm( n, 0.5, sv )
  p4 <- fclip(p4)
  q <- rep(0,n) # all initially wrong culture  
  w <- rep(w0,n)
  qparent<-rep(0,n)
  
  for ( i in 1:tmax ) {
    # see if environment changes
    ut <- sample(c(0,1),1,prob=c(1-u,u)) #0 or 1. Does or doesn't
    ut <- rep(ut,n) #if it changes, it changes for everyone
    q <- ifelse( ut==1 , 0 , q ) #if it changes, everyone is wrong culture
    
    # record history
    ph[i] <- mean(p)  #each time step, index is heritable probability of indiv learning
    p2h[i] <- mean(p2) #each time step, index is heritable probability of unbiased copying
    p3h[i] <- mean(p3) #each time step, index is heritable probability of content bias
    p4h[i] <- mean(p4) #each time step, index is heritable probability of success bias
    qh[i] <- mean(q) ##each time step, average of wrong or right culture
    
    
    # learn
    # first roll dice to see if successfully learn
    learned <- ifelse( runif(n) < s , 1 , 0 )
    
    # then record a random copy from previous q, as if socially learned
    
    copied <- sample(q,n, replace=TRUE) #they just totally randomly sample from right or wrong culture
    
    
    #then sample multiple q and pick the right one
    content <- rep(0,n) 
    for ( i3 in 1:n ) {
      qsample <- sample(q,size=qs, replace = TRUE) #try some variants
      qsum <- sum(qsample)
      content[i3] <- ifelse( qsum>0 , 1 , 0 ) #If ANY of them are correct, go with that
    }
    
    #now add a success bias
    success <- rep(0,n)
    for (i4 in 1:n){  
      success [i4]<-sample (q, size=1, prob = exp(pw*w)) #Sample strategies
      if(success [i4]==1){ 
        appropriate<-ifelse(runif(1)<apt,success[i4],0) #The copied strategy is appropriate?
        success[i4]<- appropriate #Get it right if so
      }else{success[i4]<-0} #If none of the observed strategies are successful and appropriate, 0
    }
    
    #finally add kin
    kin<-qparent #success of parents
    
    # now decide which each agent actually does. THESE ARE HEIERARCHICAL
    behaveI <- ifelse( runif(n) < p , 1, 0) #individual learning
    behaveII <- ifelse( runif(n) < p2, 1, 0) # unbiased imitation
    behaveIII <- ifelse( runif(n) < p3, 1, 0) # content bias
    behaveIV <- ifelse( runif(n) < p4, 1, 0) # success or kin, p4 is success
    
    qnew <- ifelse( behaveI==1 , learned , ifelse (behaveII==1, copied, ifelse(behaveIII==1,content, ifelse(behaveIV==1, success, kin) )))
    q <- qnew #set strategy as sucessful or not
    
    # assign fitness values
    w <- w0 + b*q - b*k*behaveI - b*l*behaveII*(1-behaveI) - b*j*(behaveIII)*(1-behaveI)*(1-behaveII) - b*m*(behaveIV)*(1-behaveI)*(1-behaveII)*(1-behaveIII) - b*f*(1-behaveIV)*(1-behaveI)*(1-behaveII)*(1-behaveIII)
    
    
    
    #record fitness history
    wh[i] <- mean(w)
    
    # populate next generation, weighted by fitness
    parentIndices <- sample( 1:n , size=n , replace=TRUE , prob=w ) #how many kids each person has
    pnew <- p[parentIndices]
    p<-pnew
    p2new <- p2[parentIndices]
    p2<-p2new
    p3new <- p3[parentIndices]
    p3<-p3new
    p4new <- p4[parentIndices]
    p4<-p4new
    # store parent culture for later kin bias learning
    qparent <- q[parentIndices]
    
    
    # mutate p
    p <- rnorm( n , p , mu )
    p <- fclip(p)
    p2 <- rnorm(n, p2, mu)
    p2 <- fclip(p2)
    p3 <- rnorm(n, p3, mu)
    p3 <- fclip(p3)
    p4 <- rnorm(n, p4, mu)
    
    p4 <- fclip(p4)
    
  }
  list(p=ph, p2=p2h, p3=p3h, p4=p4h, q=qh, w=wh)
}





#function for geographic mean
basefit <- function(w) {sum(log(w))/200}





# tmax : number of generations to simulate
# b : benefit of adaptive behavior
# k : cost of individual learning, as proportion of b
# u : chance environment changes
# W0: Baseline fitness
# n : population size
# mu: mutation size
# s : success rate of individual learning
# l : cost of random copying
# j : cost of evaluating content
# m : cost of success bias
# f : cost of copying parent
#apt: Probability that success biased learning leads to appropriate variant
# pw: Scales probability that successful individuals copied  (leave at 0)
# qs: sample size of individuals observable for content bias



FillDat<-data.frame(expand.grid(PopSize=c(500,1000,2000,3000,4000),Run=1:4,EnvVar=c(.1,.2,.3,.4,.5)),
                    Success=rep(NA),Independant=rep(NA),Content=rep(NA),Unbiased=rep(NA) )




for(i in 1:nrow(FillDat)){
  
  dsim <- data.frame(u=0,p=0,p2=0,p3=0,p4=0, w=0)
    x <- simrogers(tmax=5000, b=1 , k=0.5 , u=FillDat[i,]$EnvVar, w0=5 , n=FillDat[i,]$PopSize , mu=0.005 , s=0.5 , j=0.5, m=0.5, l=0.5, f=0.5, apt=0.5, pw = 0, qs = 3,sv=0.1)
    Ep <- mean( x$p[4800:5000] ) #just show final levels
    Ep2 <- mean( x$p2[4800:5000] )
    Ep3 <- mean( x$p3[4800:5000] )
    Ep4 <- mean( x$p4[4800:5000] )
    Ew <- basefit( x$w[4800:5000])
    dsim[1,] <- c(k1,Ep, Ep2, Ep3, Ep4, Ew)
    print( paste0(i,"%") )
    
  
  
  
    FillDat[i,]$Independant=dsim$p
    FillDat[i,]$Unbiased= (1-dsim$p)*dsim$p2
    FillDat[i,]$Success = (1-dsim$p)*(1-dsim$p2)*(1-dsim$p3)*dsim$p4
    FillDat[i,]$Content = (1-dsim$p)*(1-dsim$p2)*dsim$p3
  
}


write.csv(FillDat,"PopVariationSweep_01Mu.csv")

FillDat<-read.csv("PopVariationSweep.csv")
FillDat%>%
  group_by(PopSize,SelPress)%>%
  summarise(MedDif=median(IndVsCopyDif))%>%
  ggplot(.,aes(x=PopSize,y=SelPress,fill=MedDif))+
  geom_tile()+
  scale_x_log10()+
  scale_fill_viridis_b(name="Difference in\ndifference" )+theme_bw()+
  theme(panel.grid = element_blank(),axis.text = element_text(size=14,color="black"),
        axis.title = element_text(color="black",size=18),
        legend.text = element_text(size=12,color="black"),
        legend.title = element_text(size=14,color="black"))+
  ylab("Selection pressure")+xlab("Population size")





FillDat%>%
  group_by(PopSize,SelPress)%>%
  #summarise(SDDif=sd(IndVsCopyDif))%>%
  summarise(SDDif=range(IndVsCopyDif)[2]-range(IndVsCopyDif)[1])%>%
  ggplot(.,aes(x=PopSize,y=SelPress,fill=SDDif))+
  scale_x_log10()+
  geom_tile()+
  scale_fill_viridis_b(name="Range\nin outcome")+theme_bw()+
  theme(panel.grid = element_blank(),axis.text = element_text(size=14,color="black"),
        axis.title = element_text(color="black",size=18),
        legend.text = element_text(size=12,color="black"),
        legend.title = element_text(size=14,color="black"))+
  ylab("Selection pressure")+xlab("Population size")
