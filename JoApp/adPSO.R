# PSO with adaptive weight 
# Based on the book Fundamentals of Particle Swarm Optimisation by A. Engelbrecht 

# Learning experience with teaching learning based optimization or LETLBO Zou et al. (2015)
AD_PSO=function(pop,complexes,dim,xmin,xmax,fitness,gen){
  source("multimodal.R")
  source("unimodal.R")
  # initialise Learners
  fit_func<-function(x){
    switch(fitness,
           "Sphere"=return(sphere(x)),
           "Rastrigin"=return(rastrigin(x)),
           "Rosenbrock"=return(rosenbrock(x))      
    )
  }
  error<- switch(fitness,
                 "Sphere"=1e-8,
                 "Rastrigin"=1e-8,
                 "Rosenbrock"=1e-8      
  )
  
  # Initialise particles 
  vmax=c(0.1*(xmax-xmin),0.1*(xmax-xmin))
  swarm=list()
  vel=list()
  pbest_loc=list()
  for(j in 1:complexes){
    swarm[[j]]=matrix(ncol=dim,nrow=pop)
    vel[[j]]=matrix(ncol=dim,nrow=pop)
    pbest_loc[[j]]=matrix(ncol=dim,nrow=pop)
    for(i in 1:dim){
      swarm[[j]][,i]=runif(pop,xmin,xmax) # assuming that no number will be generated twice
      vel[[j]][,i]=0
      pbest_loc[[j]][,i]=swarm[[j]][,i]
    }
  }
  result=matrix(ncol=complexes,nrow=pop)
  pbest=matrix(ncol=complexes,nrow=pop)
  gbest=c()
  gbest_loc=matrix(ncol=dim,nrow=complexes)

  # evaluate swarm and define first gbest
  for(j in 1:complexes){
    for(i in 1:pop){
      x=swarm[[j]][i,]
      result[i,j]=fit_func(x)
      pbest[i,j]=result[i,j]
    }
    gbest[j]=min(result[,j])
    pos=which.min(result[,j])
    gbest_loc[j,]=swarm[[j]][pos,]
  }
  
  # optimisation algorithm update 
  k=1
  results=list()
  while(k<gen && gbest>error){
    w=(0.9-0.2)*(gen-k)/gen+0.2 # based on Suganthan, Roshida and yoshida et al. 
    vmax=c((0.1*(xmax-xmin)-0.5)*(gen-k)/gen+0.5,(0.1*(xmax-xmin)-0.5)*(gen-k)/gen+0.5)
    for(j in 1:complexes){
      for(i in 1:pop){
          c2=0.5+(2.5-0.5)*k/gen
          c1=0.1+(1.5-0.1)*(gen-k)/gen
          vel[[j]][i,]=w*vel[[j]][i,]+c1*runif(2)*(pbest_loc[[j]][i,]-swarm[[j]][i,])+c2*runif(2)*(gbest_loc[j,]-swarm[[j]][i,])
          
          for(m in 1:dim){
            if(vel[[j]][i,m] > vmax[m]){
              vel[[j]][i,m]=vmax[m]}
            else if(vel[[j]][i,m] < -vmax[m]){
              vel[[j]][i,m]=-vmax[m]
            }
          }

          swarm[[j]][i,]=swarm[[j]][i,]+vel[[j]][i,]
          for(m in 1:dim){
            if(swarm[[j]][i,m] > xmax){
              swarm[[j]][i,m] = xmax}
            else if(swarm[[j]][i,m] < xmin)
              {swarm[[j]][i,m] = xmin}
          }
          result[i,j]=fit_func(swarm[[j]][i,])
          if(result[i,j]<pbest[i,j]){
            pbest[i,j]=result[i,j]
          }
      }
      if(gbest[j]>min(result[,j])){
        gbest[j]=min(result[,j])
        pos=which.min(result[,j])
        gbest_loc[j,]=swarm[[j]][pos,]
      }
    }
    results[[k]]=cbind(swarm[[1]],result[,1])
    k=k+1  
  }
  results[[k]]=cbind(gbest_loc,gbest[1])
  
  return(results)
}