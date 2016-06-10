# Sphere function
# optimum at -1400


sphere=function(z){
	y=0
	for(i in 1:length(z))
	{
		y=y+z[i]^2
	}
	y=y 
	return(y)
}

rotated_discus=function(z){
	y=0
	for(i in 1:length(z)){
		y=y+z[i]^2
	}
	y=y+10^6*z[1]^2-1100
	return(y)
}
