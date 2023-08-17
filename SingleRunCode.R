# Note: this code is not used in the study as we run all combinations of parameters settings for multiple runs.
#Thus the code used here is for interested readers to quickly explore the outputs of single runs of various parameter settings.


source("PlotFunction.r")
# katie d basic social learning model



################## THIS CODE IS FOR THE SINGLE RUN VERSION OF THE MODEL #########

############# SCROLL DOWN FOR MULTI RUN VERSION #############################
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
  ph <- {rep(NA,tmax)}
  p2h <-{rep(NA,tmax)}
  p3h <-{rep(NA,tmax)}
  p4h <-{rep(NA,tmax)}
  qh <- {rep(NA,tmax)}
  wh <- {rep(NA,tmax)}
  
  # initialize population
  # p is heritable prob of individual learning
  # p2 is heritable prob of unbiased copying
  # p3 is heritable prob of using content bias
  # p4 is heritable prob of using success or kin bias
  # q is 0,1 adaptive value of behavior
  
  #This first 0.5 is the proportion of learners starting with individual learning!!!!
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
basefit <- function(w) {sum(log(w))/100}





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






#simulate across many values
set.seed(111)
dsim <- data.frame(u=0,p=0,p2=0,p3=0,p4=0, w=0)
k1list <- c(.01, .05, .1, .2, .3, .4, .5, .6) # k values to loop over
rownum <- 1

for ( k1 in k1list ) {
 x <- simrogers(
#favoring no specific learning bias
#tmax=5000, b=1 , k=0.5 , u=k1, w0=5 , n=1000 , mu=0.001 , s=0.5 , j=0.5, m=0.5, l=0.5, f=0.5, apt=.5, pw = 0, qs = 3,sv=0.1)
#favoring content bias
tmax=1000, b=1 , k=0.5 , u=k1, w0=5 , n=1000 , mu=0.001 , s=0.5 , j=0.1, m=0.5, l=0.5, f=0.5, apt=.1, pw = 0, qs = 3,sv=0.1)
  Ep <- mean( x$p[800:1000] ) #just show final levels
  Ep2 <- mean( x$p2[800:1000] )
  Ep3 <- mean( x$p3[800:1000] )
 Ep4 <- mean( x$p4[800:1000] )
 Ew <- basefit( x$w[800:1000])
 dsim[rownum,] <- c(k1,Ep, Ep2, Ep3, Ep4, Ew)
  print( dsim[rownum,] )
  rownum <- rownum + 1

}

Simplot(dsim)

