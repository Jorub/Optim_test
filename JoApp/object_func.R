RMSE=function(sim,real){
  result=0
  for(i in 1:length(real)){
    result=result+(sim[i]-real[i])^2
  }
  result=sqrt(result/length(real))
  return(result)
}

NS=function(sim, real){
  var_sim=0
  var_data=0
  mean_real=mean(real)
  for(i in 1:length(real)){
    var_sim=var_sim+(real[i]-sim[i])^2
    var_data=var_data+(real[i]-mean_real)^2
  }
  result=1-var_sim/var_data
  return(result)
}