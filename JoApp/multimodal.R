# multimodal functions

rastrigin=function(z){
	y=0
	for(i in 1:length(z)){
		y=y+(5.12/100*z[i])^2-10*cos(2*pi*(z[i]*5.12/100))
	}
	y=y+20
	return(y)
}

rosenbrock=function(z){
	y=0
	for(i in 1:(length(z)-1)){
		y=y+100*(2.048/100*z[i+1]-(2.048/100*z[i])^2)^2+100*(1-2.048/100*z[i])^2
	}
	return(y)
}

