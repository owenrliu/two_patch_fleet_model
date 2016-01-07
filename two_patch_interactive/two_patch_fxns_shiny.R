## Two-patch model with fleet, but just the source functions, for use with Shiny.  Same pieces of code as in the Rmd
## file in this directory.  However, only includes the functions and code necessary for simulation, with
## none of the single-population sensitivity analysis. For explanations and representations of these
## functions and how they work, read the two_patch_with_fleet html or .Rmd file.

#### Libraries and Dependencies ####
library(RColorBrewer)
library(ggplot2)
library(reshape2)
library(popbio)

#### Multiplot function ####
# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#### Fishery and PopDy Equations ####

## Von Bertalanffy Growth
vb.growth <- function(age.vec,vb)  { 
  ## function that takes an age vector and a vector of V-B params, c(Linf, K, t0)
  ## returns a vector of lengths
  lengths <- sapply(age.vec, function(x) vb[1]*(1-exp(-vb[2]*(x-vb[3]))))
  return(lengths)
}

## Selectivity at Length/Age
selectivity.at.length <- function(vb,age.vec, L50, L95) {
  l.vec <- vb.growth(age.vec=age.vec,vb=vb)
  sel.vec <- 1/(1+exp(-1*log(19)*(l.vec-L50)/(L95-L50)))  
  return(sel.vec)
}

## Survival function, including natural mortality and fishing
surv.fxn <- function(fish, q.vec, mort) {
  p.vec <- exp(-fish*q.vec-mort)
  # where fish is the fishing intensity, q.vec is the selectivity vector, and mort is natural mortality
  return(p.vec)
}

## Fertility/Fecundity
fert <- function(age.vec, vb, repr.age, fert1,fert2) {
  ## function that builds on VB equation above, taking three more parameters to calculate fertility
  ## as a function of age.
  ## From Armsworth (2002) and Sadovy (1996)
  l.vec <- vb.growth(age.vec=age.vec,vb=vb)
  f <- sapply(l.vec, function(x) fert1*x^fert2)
  f[1:repr.age] <- 0
  return(f)
}

## Weight at Length/Age
weight.at.age <- function(vb,age.vec,lw.a,lw.b) {
  # first, age to length
  l.vec <- vb.growth(age.vec=age.vec,vb=vb)
  #then, weights-by-length (divide by 1000 for g to kg)
  w.vec <- lw.a*l.vec^(lw.b)/1000
  return(w.vec)
}

## Spawning Stock Biomass (weight times numbers times percent mature)
ssb <- function(w.vec,n.vec,mature.vec) sum(w.vec*n.vec*mature.vec)

## Fisheries yield
yield <- function(fish,q.vec,mort,w.vec,n.vec) {
  expl.rate<-(fish*q.vec)/(fish*q.vec+mort) # exploitation rate is equal to the ratio of fishing 
  # mortality (F=f*q) to total mortality (F+M). This equation returns an age-specific exploitation rate
  y.vec <- expl.rate*w.vec*n.vec # yield by age 
  return(sum(y.vec)) # total yield
}

## Net Reproductive Rate
net.R <- function(fert.vec,p.vec,max.age) {
  p.cum <- numeric()
  for (i in 1:(max.age+1)) { #cumulative survival (i.e. vector of prob. surviving age 0:x)
    p.cum[i] <- prod(p.vec[1:i])
  }
  R <- sum(fert.vec*p.cum)
  return(R)
}

#### Population Projection Matrix Construction ####
M.di <- function (max.age,a, gamma, fert.vec, p.vec) {
  M=matrix(nrow=max.age+1,ncol=max.age+1)
  r1 <- fert.vec*a*gamma #first row, allowing for a partially open population and settler mortality in year 1
  M[1,] <-r1
  for (i in 2:(max.age+1)) { #rows 2 to max age
    row <- numeric(max.age+1)
    row[i-1] <- p.vec[i]
    M[i,] <- row
  }
  return(M)
}

#### Stochasticity/Randomness Estimator Functions ####
# Beta Distribution parameters for a given mean and CV
estBetaParams <- function(mu, cv) { # estimates parameters for a beta distribution, for a desired mean and cv
  var = (cv*mu)^2
  alpha <- ((1 - mu) / var - 1 / mu) * mu ^ 2
  beta <- alpha * (1 / mu - 1)
  return(params = list(alpha = alpha, beta = beta))
}
# Lognormal parameters for a given mean and CV
estlnormParams <- function(mean,cv) { #estimates location and scale parameters for a lognormal 
  # distribution for a desired mean and cv, for use in R's rlnorm function
  var = (cv*mean)^2
  scale <- sqrt(log(1+var/mean^2))
  loc <- log(mean)-scale/2
  return(params=list(meanlog=loc, sdlog=scale))
}

#### Single Population Simulation ####
## Calls the functions above to do a population simulation
sim.dipop <- function(nyears=200,max.age=14,Linf=52.2,K=0.354,t0 = -0.766, surv=0.863, mort=1-0.863, 
                      repr.age=2, gamma=0.125,fert1=0.0129,fert2=3.03, fish=0, L50=25,L95=40, lw.a=0.0105, 
                      lw.b=3.11, a.mu=0.001,a.cv=0.5, ext.mean=1000, ext.cv=0.5) {
  ## age vector
  age.vec <- 0:max.age
  
  ## lengths vector
  vb <- c(Linf,K,t0)
  l.vec <- vb.growth(age.vec=age.vec,vb=vb)
  
  ## fertility vector
  fert.vec <- fert(age.vec=age.vec,vb=vb,repr.age=repr.age,fert1=fert1,fert2=fert2)
  
  ## selectivity-at-age
  q.vec <- selectivity.at.length(vb=vb,age.vec=0:max.age, L50=L50, L95=L95)
  
  ## survival vector
  p.vec <- surv.fxn(fish=fish, q.vec=q.vec,mort=mort)
  
  ## maturity-at-age
  mature.vec <- c(rep(0,repr.age-1),rep(1,max.age+2-repr.age))
  
  ## weight-at-age
  w.vec <- weight.at.age(age.vec=age.vec,vb=vb,lw.a=lw.a,lw.b=lw.b)
  
  ## allows external larvae to affect the matrix
  e.vec <- c(1,rep(0,(max.age)))
  
  ## stable age distribution (for seeding simulation)
  stableage <- Re(eigen(M.di(max.age=max.age,a=a.mu,gamma=gamma,fert.vec=fert.vec,p.vec=p.vec))$vector)[,1]
  stableage <- stableage/sum(stableage)
  
  ## parameters and vector for beta-distributed a (self-recruitment rate)
  a.alpha <- estBetaParams(mu=a.mu,cv=a.cv)[[1]]
  a.beta <- estBetaParams(mu=a.mu,cv=a.cv)[[2]]
  a.rand <- rbeta(n=nyears,shape1=a.alpha,shape2=a.beta)
  
  ## parameters and vector for log-norm-distributed ext (number of external larvae)
  ext.meanlog <- estlnormParams(mean=ext.mean,cv=ext.cv)[[1]]
  ext.sdlog <- estlnormParams(mean=ext.mean,cv=ext.cv)[[2]]
  ext.rand <- rlnorm(n=nyears,meanlog=ext.meanlog,sdlog=ext.sdlog)
  
  ## the simulation output matrix
  d.i.sim <- matrix(nrow=(max.age+1), ncol=nyears)
  d.i.simtot <- numeric() # total population
  Ninit <- rep(250,15)*stableage # starting abundance
  d.i.sim[,1] <- Ninit
  d.i.simtot <- sum(Ninit)
  eigens <- numeric() # for holding eigenvalues
  Radj <- numeric() # for holding "adjusted" R values (R*gamma*a in each year)
  SSB <- numeric() # for holding value of SSB in each year
  Yield <- numeric() #for holding value of yield in each year
  
  ## run the simulation
  for (i in 1:(nyears-1)) {
    demo.rand <- runif(1,min=0.875,max=1.125) # allows uniform random synchronous variation
    M <- M.di(max.age=max.age,a=a.rand[i], gamma=gamma*demo.rand, fert.vec=fert.vec*demo.rand, p.vec=p.vec*demo.rand)
    eigens[i] <- eigen(M)[[1]][1] #store the dominant eigen value
    Radj[i] <- net.R(fert.vec=fert.vec*demo.rand,p.vec=p.vec*demo.rand,max.age=max.age)*(gamma*demo.rand)*a.rand[i]
    N.vec <- M%*%d.i.sim[,i] + (gamma*ext.rand[i]*e.vec)
    d.i.sim[,i+1] <- N.vec
    d.i.simtot[i+1] <- sum(N.vec)
    SSB[i] <- ssb(w.vec=w.vec,n.vec=d.i.sim[,i],mature.vec=mature.vec)
    Yield[i] <- yield(fish=fish,q.vec=q.vec,mort=mort,w.vec=w.vec,n.vec=d.i.sim[,i])
  }
  ## for the last year
  demo.rand <- runif(1,min=0.875,max=1.125) # allows uniform random synchronous variation
  M <- M.di(max.age=max.age,a=a.rand[nyears], gamma=gamma*demo.rand, fert.vec=fert.vec*demo.rand, p.vec=p.vec*demo.rand)
  eigens[nyears] <- eigen(M)[[1]][1] #store the dominant eigen value
  Radj[nyears] <- net.R(fert.vec=fert.vec*demo.rand,p.vec=p.vec*demo.rand,max.age=max.age)*(gamma*demo.rand)*a.rand[nyears]
  SSB[nyears] <- ssb(w.vec=w.vec,n.vec=d.i.sim[,nyears],mature.vec=mature.vec)
  Yield[nyears] <- yield(fish=fish,q.vec=q.vec,mort=mort,w.vec=w.vec,n.vec=d.i.sim[,nyears])
  
  ## Returns a list with the last 100 years of the simulation, including population (every age class), total population summed over age classes, proportional population in each age class, eigenvalues of each year's matrix, and Reproductive Value for the average individual each year, adjusted for settler survival (gamma) and self-recruitment rate a.
  return(list(pop=d.i.sim[,(nyears-99):nyears],tot=d.i.simtot[(nyears-99):nyears],props=apply(d.i.sim,
                                                                                              MARGIN=2,FUN=function(x) x/sum(x))[,(nyears-99):nyears], eigens=Re(eigens[(nyears-99):nyears]),
              Radj=Radj[(nyears-99):nyears],SSB=SSB[(nyears-99):nyears],Yield=Yield[(nyears-99):nyears]))
}

#### Two-patch Movement Matrix ####
Q.fxn <- function (max.age,larvala, adulta, gamma, fert.vec,p.vec) {
  Q=matrix(nrow=max.age+1,ncol=max.age+1) # square matrix, with each dimension equal to the number of age classes
  r1 <- fert.vec*larvala*gamma #first row, which combines larval production in the outside population, proportional recruitment into the local population, and larval survival. IMPORTANT that the fert.vec used here is not for the local population (that will be accounted for in M), but for the outside population that is seeding the local population.
  Q[1,] <-r1
  for (i in 2:(max.age+1)) { #rows 2 to max age
    row <- numeric(max.age+1)
    row[i-1] <- adulta*p.vec[i]
    Q[i,] <- row
  }
  return(Q)
}

#### Two-patch simulation function ####
sim.2pops <- function(nyears=200,max.age=14,Linf.1=52.2,K.1=0.354,t0.1 = -0.766, surv.1=0.863, mort.1=1-0.863, 
                      repr.age.1=2, gamma.1=0.125,fert1.1=0.0129,fert2.1=3.03, fish.1=0, L50.1=25,L95.1=40, 
                      lw.a.1=0.0105, lw.b.1=3.11, a.mu.1=0.001,a.cv.1=0.5,Linf.2=52.2,K.2=0.354,t0.2 = -0.766, 
                      surv.2=0.863, mort.2=1-0.863, repr.age.2=2, gamma.2=0.125,fert1.2=0.0129,fert2.2=3.03, 
                      fish.2=0, L50.2=25,L95.2=40, lw.a.2=0.0105, lw.b.2=3.11, a.mu.2=0.001,a.cv.2=0.5, 
                      larvala.mu.1=0.0005,larvala.cv.1=0.5,adulta.mu.1=0,adulta.cv.1=0.5,larvala.mu.2=0.0005,
                      larvala.cv.2=0.5,adulta.mu.2=0,adulta.cv.2=0.5) {
  
  ### First population
  ## age vector
  age.vec <- 0:max.age
  
  ## lengths vector
  vb.1 <- c(Linf.1,K.1,t0.1)
  l.vec.1 <- vb.growth(age.vec=age.vec,vb=vb.1)
  
  ## fertility vector
  fert.vec.1 <- fert(age.vec=age.vec,vb=vb.1,repr.age=repr.age.1,fert1=fert1.1,fert2=fert2.1)
  
  ## selectivity-at-age
  q.vec.1 <- selectivity.at.length(vb=vb.1,age.vec=age.vec, L50=L50.1, L95=L95.1)
  
  ## survival vector
  p.vec.1 <- surv.fxn(fish=fish.1, q.vec=q.vec.1,mort=mort.1)
  
  ## maturity-at-age
  mature.vec.1 <- c(rep(0,repr.age.1-1),rep(1,max.age+2-repr.age.1))
  
  ## weight-at-age
  w.vec.1 <- weight.at.age(age.vec=age.vec,vb=vb.1,lw.a=lw.a.1,lw.b=lw.b.1)
  
  ## allows external larvae to affect the matrix
  e.vec <- c(1,rep(0,(max.age)))
  
  ## stable age distribution (for seeding simulation)
  stableage.1 <- Re(eigen(M.di(max.age=max.age,a=a.mu.1,gamma=gamma.1,fert.vec=fert.vec.1,p.vec=p.vec.1))$vector)[,1]
  stableage.1 <- stableage.1/sum(stableage.1)
  
  ## parameters and vector for beta-distributed a (self-recruitment rate)
  a.alpha.1 <- estBetaParams(mu=a.mu.1,cv=a.cv.1)[[1]]
  a.beta.1 <- estBetaParams(mu=a.mu.1,cv=a.cv.1)[[2]]
  a.rand.1 <- rbeta(n=nyears,shape1=a.alpha.1,shape2=a.beta.1)
  
  ### Second population
  ## age vector
  age.vec <- 0:max.age
  
  ## lengths vector
  vb.2 <- c(Linf.2,K.2,t0.2)
  l.vec.2 <- vb.growth(age.vec=age.vec,vb=vb.2)
  
  ## fertility vector
  fert.vec.2 <- fert(age.vec=age.vec,vb=vb.2,repr.age=repr.age.2,fert1=fert1.2,fert2=fert2.2)
  
  ## selectivity-at-age
  q.vec.2 <- selectivity.at.length(vb=vb.2,age.vec=age.vec, L50=L50.2, L95=L95.2)
  
  ## survival vector
  p.vec.2 <- surv.fxn(fish=fish.2, q.vec=q.vec.2,mort=mort.2)
  
  ## maturity-at-age
  mature.vec.2 <- c(rep(0,repr.age.2-1),rep(1,max.age+2-repr.age.2))
  
  ## weight-at-age
  w.vec.2 <- weight.at.age(age.vec=age.vec,vb=vb.2,lw.a=lw.a.2,lw.b=lw.b.2)
  
  ## allows external larvae to affect the matrix
  e.vec <- c(1,rep(0,(max.age)))
  
  ## stable age distribution (for seeding simulation)
  stableage.2 <- Re(eigen(M.di(max.age=max.age,a=a.mu.2,gamma=gamma.2,fert.vec=fert.vec.2,p.vec=p.vec.2))$vector)[,1]
  stableage.2 <- stableage.2/sum(stableage.2)
  
  ## parameters and vector for beta-distributed a (self-recruitment rate)
  a.alpha.2 <- estBetaParams(mu=a.mu.2,cv=a.cv.2)[[1]]
  a.beta.2 <- estBetaParams(mu=a.mu.2,cv=a.cv.2)[[2]]
  a.rand.2 <- rbeta(n=nyears,shape1=a.alpha.2,shape2=a.beta.2)
  
  ## Random, beta-distributed larval and adult connectivity, pop2 to pop1, based on mean larval connectivity larvala.mu.1, and mean adult connectivity adulta.mu.1
  larvala.alpha.12 <- estBetaParams(mu=larvala.mu.1,cv=larvala.cv.1)[[1]]
  larvala.beta.12 <- estBetaParams(mu=larvala.mu.1,cv=larvala.cv.1)[[2]]
  larvala.rand.12 <- rbeta(n=nyears,shape1=larvala.alpha.12,shape2=larvala.beta.12)
  if(larvala.mu.1==0) larvala.rand.12<-rep(0,nyears) # corrects for an error in rbeta() when params are zero (for zero connectivity)
  adulta.alpha.12 <- estBetaParams(mu=adulta.mu.1,cv=adulta.cv.1)[[1]]
  adulta.beta.12 <- estBetaParams(mu=adulta.mu.1,cv=adulta.cv.1)[[2]]
  adulta.rand.12 <- rbeta(n=nyears,shape1=adulta.alpha.12,shape2=adulta.beta.12)
  if(adulta.mu.1==0) adulta.rand.12<-rep(0,nyears)
  
  ## Random, beta-distributed larval and adult connectivity, pop1 to pop2 based on mean larval connectivity larvala.mu.2, and mean adult connectivity adulta.mu.2
  larvala.alpha.21 <- estBetaParams(mu=larvala.mu.2,cv=larvala.cv.2)[[1]]
  larvala.beta.21 <- estBetaParams(mu=larvala.mu.2,cv=larvala.cv.2)[[2]]
  larvala.rand.21 <- rbeta(n=nyears,shape1=larvala.alpha.21,shape2=larvala.beta.21)
  if(larvala.mu.2==0) larvala.rand.21<-rep(0,nyears) # corrects for an error in rbeta() when params are zero (for zero connectivity)
  adulta.alpha.21 <- estBetaParams(mu=adulta.mu.2,cv=adulta.cv.2)[[1]]
  adulta.beta.21 <- estBetaParams(mu=adulta.mu.2,cv=adulta.cv.2)[[2]]
  adulta.rand.21 <- rbeta(n=nyears,shape1=adulta.alpha.21,shape2=adulta.beta.21)
  if(adulta.mu.2==0) adulta.rand.21<-rep(0,nyears)
  
  ## the simulation output holders (pop 1)
  sim.1 <- matrix(nrow=(max.age+1), ncol=nyears)
  simtot.1 <- numeric() # total population
  Ninit.1 <- rep(250,15)*stableage.1 # starting abundance (subpop 1)
  sim.1[,1] <- Ninit.1
  simtot.1[1] <- sum(Ninit.1)
  eigens.1 <- numeric() # for holding eigenvalues
  Radj.1 <- numeric() # for holding "adjusted" R values (R*gamma*a in each year)
  SSB.1 <- numeric() # for holding value of SSB in each year
  Yield.1 <- numeric() #for holding value of yield in each year
  
  ## the simulation output holders (pop 2)
  sim.2 <- matrix(nrow=(max.age+1), ncol=nyears)
  simtot.2 <- numeric() # total population
  Ninit.2 <- rep(250,15)*stableage.2 # starting abundance (subpop 2)
  sim.2[,1] <- Ninit.2
  simtot.2[1] <- sum(Ninit.1)
  eigens.2 <- numeric() # for holding eigenvalues
  Radj.2 <- numeric() # for holding "adjusted" R values (R*gamma*a in each year)
  SSB.2 <- numeric() # for holding value of SSB in each year
  Yield.2 <- numeric() #for holding value of yield in each year
  
  ## the simulation output holders (total pop)
  sim.all <- matrix(nrow=2*(max.age+1), ncol=nyears)
  simtot.all <- numeric() # total population
  sim.all[,1] <- c(Ninit.1,Ninit.2)
  simtot.all[1] <- sum(Ninit.1,Ninit.2)
  eigens.all <- numeric() # for holding eigenvalues
  SSB.all <- numeric() # for holding value of SSB in each year
  Yield.all <- numeric() #for holding value of yield in each year
  
  ## Run the simulation
  for (i in 1:(nyears-1)) {
    ## Movement matrices Q12 (transitions and reproduction of individuals the move from patch 2 to patch 1) and Q21       (transitions and reproduction of individuals the move from patch 1 to patch 2). Remember, the fertility vector        corresponds to the outside population, and all other survival and recruitment-rate terms correspond to the local      population.
    Q.12 <- Q.fxn(max.age=max.age,larvala=larvala.rand.12[i], adulta=adulta.rand.12[i], gamma=gamma.1,fert.vec=fert.vec.2,p.vec=p.vec.1)
    Q.21 <- Q.fxn(max.age=max.age,larvala=larvala.rand.21[i], adulta=adulta.rand.21[i], gamma=gamma.2,fert.vec=fert.vec.1,p.vec=p.vec.2)
    
    ## Local transition matrices M1 and M2
    demo.rand <- runif(1,min=0.875,max=1.125) # allows uniform random synchronous variation in demographic rates
    M.1 <- M.di(max.age=max.age,a=a.rand.1[i], gamma=gamma.1*demo.rand, fert.vec=fert.vec.1*demo.rand, p.vec=p.vec.1*demo.rand)
    M.2 <- M.di(max.age=max.age,a=a.rand.2[i], gamma=gamma.2*demo.rand, fert.vec=fert.vec.2*demo.rand, p.vec=p.vec.2*demo.rand)
    
    #Projection matrix A, including M and Q
    A <- rbind(cbind(M.1,Q.12),cbind(M.2,Q.21))
    
    ## update all the variables we're tracking
    
    # total pop
    eigens.all[i] <- eigen(A)[[1]][1] 
    N.vec <- A%*%sim.all[,i]
    sim.all[,i+1] <- N.vec
    simtot.all[i+1] <- sum(N.vec)
    
    # pop 1
    eigens.1[i] <- eigen(M.1)[[1]][1]
    Radj.1[i] <- net.R(fert.vec=fert.vec.1*demo.rand,p.vec=p.vec.1*demo.rand,max.age=max.age)*(gamma.1*demo.rand)*a.rand.1[i]
    sim.1[,i+1] <- N.vec[1:(max.age+1)]
    simtot.1[i+1] <- sum(N.vec[1:(max.age+1)])
    SSB.1[i] <- ssb(w.vec=w.vec.1,n.vec=sim.1[,i],mature.vec=mature.vec.1)
    Yield.1[i] <- yield(fish=fish.1,q.vec=q.vec.1,mort=mort.1,w.vec=w.vec.1,n.vec=sim.1[,i])
    
    # pop 2
    eigens.2[i] <- eigen(M.2)[[1]][1]
    Radj.2[i] <- net.R(fert.vec=fert.vec.2*demo.rand,p.vec=p.vec.2*demo.rand,max.age=max.age)*(gamma.2*demo.rand)*a.rand.2[i]
    sim.2[,i+1] <- N.vec[(max.age+2):(2*(max.age+1))]
    simtot.2[i+1] <- sum(N.vec[1:(max.age+1)])
    SSB.2[i] <- ssb(w.vec=w.vec.2,n.vec=sim.2[,i],mature.vec=mature.vec.2)
    Yield.2[i] <- yield(fish=fish.2,q.vec=q.vec.2,mort=mort.2,w.vec=w.vec.2,n.vec=sim.2[,i])
    
    # total SSB and yield
    SSB.all[i] <- sum(SSB.1[i],SSB.2[i])
    Yield.all[i] <- sum(Yield.1[i],Yield.2[i])
    
  }
  ## FOR THE FINAL YEAR
  demo.rand <- runif(1,min=0.875,max=1.125) # allows uniform random synchronous variation in demographic rates
  M.1 <- M.di(max.age=max.age,a=a.rand.1[i], gamma=gamma.1*demo.rand, fert.vec=fert.vec.1*demo.rand, p.vec=p.vec.1*demo.rand)
  M.2 <- M.di(max.age=max.age,a=a.rand.2[i], gamma=gamma.2*demo.rand, fert.vec=fert.vec.2*demo.rand, p.vec=p.vec.2*demo.rand)
  
  #Projection matrix A, including M and Q
  A <- rbind(cbind(M.1,Q.12),cbind(M.2,Q.21))
  
  ## update all the variables we're tracking
  
  # total pop
  eigens.all[nyears] <- eigen(A)[[1]][1] 
  
  # pop 1
  eigens.1[nyears] <- eigen(M.1)[[1]][1]
  Radj.1[nyears] <- net.R(fert.vec=fert.vec.1*demo.rand,p.vec=p.vec.1*demo.rand,max.age=max.age)*(gamma.1*demo.rand)*a.rand.1[nyears]
  SSB.1[nyears] <- ssb(w.vec=w.vec.1,n.vec=sim.1[,nyears],mature.vec=mature.vec.1)
  Yield.1[nyears] <- yield(fish=fish.1,q.vec=q.vec.1,mort=mort.1,w.vec=w.vec.1,n.vec=sim.1[,nyears])
  
  # pop 2
  eigens.2[nyears] <- eigen(M.2)[[1]][1]
  Radj.2[nyears] <- net.R(fert.vec=fert.vec.2*demo.rand,p.vec=p.vec.2*demo.rand,max.age=max.age)*(gamma.2*demo.rand)*a.rand.2[nyears]
  SSB.2[nyears] <- ssb(w.vec=w.vec.2,n.vec=sim.2[,nyears],mature.vec=mature.vec.2)
  Yield.2[nyears] <- yield(fish=fish.2,q.vec=q.vec.2,mort=mort.2,w.vec=w.vec.2,n.vec=sim.2[,nyears])
  
  # total SSB and yield
  SSB.all[nyears] <- sum(SSB.1[nyears],SSB.2[nyears])
  Yield.all[nyears] <- sum(Yield.1[nyears],Yield.2[nyears])
  
  ## Returns a list of 3 lists of the simulation, including population (every age class), total population summed over age classes, proportional population in each age class, eigenvalues of each year's matrix, and Reproductive Value for the average individual each year, adjusted for settler survival (gamma) and self-recruitment rate a.  One list for each population separately, and one list for the entire population.
  simlist.1 <- list(pop=sim.1,tot=simtot.1,props=apply(sim.1,
                                                       MARGIN=2,FUN=function(x) x/sum(x)), eigens=Re(eigens.1),
                    Radj=Radj.1,SSB=SSB.1,Yield=Yield.1)
  simlist.2 <- list(pop=sim.2,tot=simtot.2,props=apply(sim.2,
                                                       MARGIN=2,FUN=function(x) x/sum(x)), eigens=Re(eigens.2),
                    Radj=Radj.2,SSB=SSB.2,Yield=Yield.2)
  simlist.all <- list(pop=sim.all,tot=simtot.all,props=apply(sim.all,
                                                             MARGIN=2,FUN=function(x) x/sum(x)), eigens=Re(eigens.all),
                      SSB=SSB.all,Yield=Yield.all)
  
  return(list(pop1=simlist.1,pop2=simlist.2,total=simlist.all))
}

#### Plotting the outputs from the simulation ####
two_pop_sim_plots <- function(params.list) {
  sim2test <- do.call(sim.2pops,params.list)
  #Radjs (horizontal lines at 1, tipping point betewen geometric growth and decline, and red line at the mean value)
  sim2Radj1.plot<-ggplot(melt(sim2test[[1]][['Radj']]),aes(x=1:200,y=value)) +geom_line() +geom_hline(aes(yintercept=1))+ggtitle('Population 1, Net Reproductive Value') +geom_hline(aes(yintercept=mean(value),color='red')) +xlab('time')+ylab('R')
  sim2Radj2.plot<-ggplot(melt(sim2test[[2]][['Radj']]),aes(x=1:200,y=value)) +geom_line() +geom_hline(aes(yintercept=1))+geom_hline(aes(yintercept=mean(value),color='red'))+ggtitle('Population 2, Net Reproductive Value') +xlab('time')+ylab('R')
  multiplot(sim2Radj1.plot,sim2Radj2.plot)
  #eigens (horizontal lines at 1, tipping point between geometric growth and decline,and red line at the mean value)
  ggplot(melt(sim2test[[3]][['eigens']]),aes(x=1:200,y=value)) +geom_line() +geom_hline(aes(yintercept=1))+geom_hline(aes(yintercept=mean(value),color='red')) +ggtitle('Entire Population, Eigen Values') +xlab('time')+ylab('Dominant Eigen Value')
  sim2eigens1.plot<-ggplot(melt(sim2test[[1]][['eigens']]),aes(x=1:200,y=value)) +geom_line() +geom_hline(aes(yintercept=1))+ggtitle('Population 1, Eigen Values') +geom_hline(aes(yintercept=mean(value),color='red')) +xlab('time')+ylab('Dominant Eigen Value')
  sim2eigens2.plot<-ggplot(melt(sim2test[[2]][['eigens']]),aes(x=1:200,y=value)) +geom_line() +geom_hline(aes(yintercept=1))+ggtitle('Population 2, Eigen Values') +geom_hline(aes(yintercept=mean(value),color='red')) +xlab('time')+ylab('Dominant Eigen Value')
  multiplot(sim2eigens1.plot,sim2eigens2.plot)
  #yield (horizontal lines at the mean value)
  ggplot(melt(sim2test[[3]][['Yield']]),aes(x=1:200,y=value)) +geom_line() +geom_hline(aes(yintercept=mean(value))) +ggtitle('Entire Population, Yield') +xlab('time')+ylab('Yield')
  sim2Yield1.plot<-ggplot(melt(sim2test[[1]][['Yield']]),aes(x=1:200,y=value)) +geom_line() +geom_hline(aes(yintercept=mean(value)))+ggtitle('Population 1, Yield') +xlab('time')+ylab('Yield')
  sim2Yield2.plot<-ggplot(melt(sim2test[[2]][['Yield']]),aes(x=1:200,y=value)) +geom_line() +geom_hline(aes(yintercept=mean(value)))+ggtitle('Population 2, Yield') +xlab('time')+ylab('Yield')
  multiplot(sim2Yield1.plot,sim2Yield2.plot)
  #abundance (horizontal line at the mean value)
  ggplot(melt(sim2test[[3]][['tot']]),aes(x=1:200,y=value)) +geom_line() +geom_hline(aes(yintercept=mean(value))) +ggtitle('Entire Population, Abundance') +xlab('time')+ylab('abundance')
  sim2pop1.plot<-ggplot(melt(sim2test[[1]][['tot']]),aes(x=1:200,y=value)) +geom_line() +geom_hline(aes(yintercept=mean(value)))+ggtitle('Population 1, Abundance') +xlab('time')+ylab('abundance')
  sim2pop2.plot<-ggplot(melt(sim2test[[2]][['tot']]),aes(x=1:200,y=value)) +geom_line() +geom_hline(aes(yintercept=mean(value)))+ggtitle('Population 2, Abundance') +xlab('time')+ylab('abundance')
  multiplot(sim2pop1.plot,sim2pop2.plot)
}
