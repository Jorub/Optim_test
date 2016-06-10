# Learning experience with teaching learning based optimization or LETLBO Zou et al. (2015)
TLBO=function(class_size,classes,dim,xmin,xmax,fitness,gen){
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
  school_old=list()
  for(j in 1:classes){
    school_old[[j]]=matrix(ncol=dim,nrow=class_size)
    for(i in 1:dim){
      school_old[[j]][,i]=runif(class_size,xmin,xmax) # assuming that no number will be generated twice
    }
  }
  result_old=matrix(ncol=classes,nrow=class_size)
  teacher=c()
  means=c()
  teacher_loc=matrix(ncol=dim,nrow=classes)
  mean_loc=matrix(ncol=dim,nrow=classes)
  # evaluate learners and define first teacher
  for(j in 1:classes){
    for(i in 1:class_size){
      z=school_old[[j]][i,]
      result_old[i,j]=fit_func(z)
    }
    teacher[j]=min(result_old[,j])
    pos=which.min(result_old[,j])
    teacher_loc[j,]=school_old[[j]][pos,]
    mean_loc[j,]=cbind(mean(school_old[[j]][,1]),mean(school_old[[j]][,2]))
  }
  
  result_new=matrix(ncol=classes,nrow=class_size)
  school_new=list()
  
  # optimisation algorithm update 
  k=1
  results=list()
while((mean(result_old)>error) && (k<gen)){
  for(j in 1:classes){
    school_new[[j]]=matrix(ncol=dim,nrow=class_size)
    for(i in 1:class_size){
      a=runif(1);b=runif(1)
      if(a<b){
        TF=round(1+runif(1))
        school_new[[j]][i,]=school_old[[j]][i,]+runif(1)*(teacher_loc[j,]-TF*mean_loc[j,])
      }else{
        other=sample.int(class_size,1)
        while(other == i){
          other=sample.int(class_size,1)
        }
        if(result_old[i,j]>result_old[other,j]){
          school_new[[j]][i,]=school_old[[j]][i,]+runif(1)*(teacher_loc[j,]-school_old[[j]][other,])
          } else{
          school_new[[j]][i,]=school_old[[j]][i,]+runif(1)*(teacher_loc[j,]-school_old[[j]][i,])
          }
      }
      result_new[i,j]=fit_func(school_new[[j]][i,])
      if(result_new[i,j]<result_old[i,j]){
        result_old[i,j]=result_new[i,j]
        school_old[[j]][i,]=school_new[[j]][i,]
      }
    }
  }
  
  for(j in 1:classes){
    for(i in 1:class_size){
      a=runif(1);b=runif(1)
      if(a<b){
        other=sample.int(class_size,1)
        while(other == i){
          other=sample.int(class_size,1)
          }
        if(result_old[i,j]<result_old[other,j]){
          school_new[[j]][i,]=school_old[[j]][i,]+runif(1)*(school_old[[j]][i,]-school_old[[j]][other,])
          } else{
          school_new[[j]][i,]=school_old[[j]][i,]+runif(1)*(school_old[[j]][other,]-school_old[[j]][i,])
        } 
      } else{
        other=sample.int(class_size,2)
        while(any(other == i)){
          other=sample.int(class_size,2)
        }
        if(result_old[other[1],j]<result_old[other[2],j]){
          school_new[[j]][i,]=school_old[[j]][i,]+runif(1)*(school_old[[j]][other[1],]-school_old[[j]][other[2],])
        } else{
          school_new[[j]][i,]=school_old[[j]][i,]+runif(1)*(school_old[[j]][other[2],]-school_old[[j]][other[1],])
        }  
      }
      result_new[i,j]=fit_func(school_new[[j]][i,])
      if(result_new[i,j]<result_old[i,j]){
        result_old[i,j]=result_new[i,j]
        school_old[[j]][i,]=school_new[[j]][i,]
      }
    }
    teacher[j]=min(result_old[,j])
   
    pos=which.min(result_old[,j])
    teacher_loc[j,]=school_old[[j]][pos,]
    mean_loc[j,]=cbind(mean(school_old[[j]][,1]),mean(school_old[[j]][,2]))
  }
  
  
  results[[k]]=cbind(school_old[[1]],result_old[,1])
  k=k+1
}
  results[[k]]=cbind(teacher_loc[1,],teacher[1])
  return(results)
}