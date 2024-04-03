#Stage based matrix model to estimate lambda and stable age
#distribution for boreal caribou simulations.

#John Boulanger, IER, March 8,2024


library(demogR)
library(popbio)

#input parameters

#sex ratio at birth and yearlings
sexratio<-0.5
#adult female survival
Saf<-0.85
#yearling survival
Sy<-0.85
#calf survival
Sc<-0.5
#fecundity 
Fa<-0.22
#product that estimates female calves produced  
SRSafFa<-sexratio*Saf*Fa

#approximate calf cow-without scaled survival estiamtes--assumes minimal change in calf cow from comp survey to end of biological year 
calfcow<-(Fa*Sc)/(Saf+0.5*Sy)

#DeCesare Recruitment.
recruitment<-calfcow*sexratio/(1+calfcow*sexratio)

#HB approximate lambda--should be semi-close to lambda from dominant eigenvalue of matrix model.
HBlambda<-Saf/(1-recruitment)
#1.1
 
#make a 3x3 stage based matrix model-this should be close to what is used in simulations?

A <- matrix(c(0,  0,  SRSafFa,
              Sc, 0,  0,
              0,  Sy, Saf), nrow=3,byrow=TRUE)

#get the eigenvalues
ev <- eigen(A)

#lambda is dominant eigenvalue-close to HB estimate
lmax <- which.max(Re(ev$values))
Re(ev$values)[lmax]
#[1] 1.113713

#stable stage distribution (calves-yearlings-adults)
eigen.analysis(A)$stable.stage
#[1] 0.2466623 0.1107387 0.6425989
