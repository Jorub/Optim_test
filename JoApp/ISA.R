# Interior search algorithm (ISA): global optimization. Gandomi (2014)

# Learning experience with teaching learning based optimization or LETLBO Zou et al. (2015)
ISA=function(elements,rooms,dim,xmin,xmax,fitness,gen){
  source("multimodal.R")
  source("unimodal.R")
  # initialise Learners
  fit_func<-function(z){
    switch(fitness,
           "Sphere"=return(sphere(z)),
           "Rastrigin"=return(rastrigin(z)),
           "Rosenbrock"=return(rosenbrock(z))      
    )
  }
  error<- switch(fitness,
                 "Sphere"=1e-8,
                 "Rastrigin"=1e-8,
                 "Rosenbrock"=1e-8      
  )
  
  # Initialise learners 
  room_old=list()
  for(j in 1:rooms){
    room_old[[j]]=matrix(ncol=dim,nrow=elements)
    for(i in 1:dim){
      room_old[[j]][,i]=runif(elements,xmin,xmax) # assuming that no number will be generated twice
    }
  }
  result_old=matrix(ncol=rooms,nrow=elements)
  gbest=c()
  gbest_loc=matrix(ncol=dim,nrow=rooms)
  pos=c()
  # evaluate elements and define first gbest
  for(j in 1:rooms){
    for(i in 1:elements){
      z=room_old[[j]][i,]
      result_old[i,j]=fit_func(z)
    }
    gbest[j]=min(result_old[,j])
    pos[j]=which.min(result_old[,j])
    gbest_loc[j,]=room_old[[j]][pos[j],]
  }
  
  result_new=matrix(ncol=rooms,nrow=elements)
  room_new=list()
  
  # optimisation algorithm update 
  k=1
  results=list()
  mins=c()
  maxs=c()
  lamda=c()
  alpha=0.2 # can be tuned
  while((mean(result_old)>error) && (k<gen)){
    for(j in 1:rooms){
      room_new[[j]]=matrix(ncol=dim,nrow=elements)
      for(n in 1:dim){
        mins[n]=min(room_old[[j]])
        maxs[n]=max(room_old[[j]])
        lamda[n]=0.01*(maxs[n]-mins[n])
      }
      for(i in 1:elements){
        if(i == pos[j]){
          room_new[[j]][i,]=gbest_loc[j,]+runif(1)*lamda
        }else if(runif(1)<alpha){
          rm=runif(1)
          mirror=rm*room_old[[j]][i,]+(1-rm)*gbest_loc[j,]
          room_new[[j]][i,]=2*mirror-room_old[[j]][i,]
        }else{
          room_new[[j]][i,]=mins+(maxs-mins)*runif(1)
        }
        result_new[i,j]=fit_func(room_new[[j]][i,])
        if(result_new[i,j]<result_old[i,j]){
          result_old[i,j]=result_new[i,j]
          room_old[[j]][i,]=room_new[[j]][i,]
        }
      }
    
      gbest[j]=min(result_old[,j])
      pos[j]=which.min(result_old[,j])
      gbest_loc[j,]=room_old[[j]][pos[j],]
    }
    results[[k]]=cbind(room_old[[1]],result_old[,1])
    k=k+1
  }
  results[[k]]=cbind(gbest_loc[1,],gbest[1])
  return(results)
}