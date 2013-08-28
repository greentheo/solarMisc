reflectSim = function(r, theta, focus, sunangle=pi/2, w=1, offset=0){
	r = c(-rev(r), r)+offset
	theta = c(rev(pi-theta), theta)
	last=function(x){
		return(x[length(x)])
	}
	first=function(x){
		return(x[1])
	}
	#horizontal fresnel, vertical sun
	
	#r = seq(1,30, by=10)
	#theta = seq(.3, .6, by=.1)
	#focus = 100
	#sunangle=pi/2
	#draw the lines representing the mirrors
	
	#for a given r find the corresponding slope and y intercept.
	# y = mx+b
	# x = (y-b) / m ->  x = y/m - b/m
	# at y=0, x=r => r=-b/m
	# m = tan(theta)
	# b = -r*tan(theta)
	#focus location is a function of the sun angle

	plot(cos(pi-sunangle)*focus, sin(pi-sunangle)*focus, type="p", lwd=10,main="Reflective Fresnel Sim", xlim=c(cos(pi/8)*focus,cos(7*pi/8)*focus), ylim=c(-1, focus+1+w))
	abline(h=focus, col="grey")
	abline(h=0, col="red", lwd=3)
	abline(v=0, col="grey")
	for(ri in 1:length(r)){
			abline(-r[ri]*tan(theta[ri]), tan(theta[ri]), col="blue")
	}
	
	#now plot the vertical lines
	
	vlines = seq(min(r)+1, max(r), length.out=20)
	
	for(vi in 1:length(vlines)){
		m = tan(sunangle)
		b = -m*vlines[vi]
		abline(b, m, col="grey")
	}
	
	#now for each vline, find which mirror it inersects, calculate the corresponding
	#reflection point and draw the line
	
	for(vi in 1:length(vlines)){
		#find which mirror it intersects with
		if(vlines[vi]<0){	
			i = first(which(vlines[vi]<r))
		}else{
			i = last(which(vlines[vi]>r))
		}

		#where on the miror does it intersect (y value)?
		mb = -r[i]*tan(theta[i])
		mm = tan(theta[i])
	
		yintm = mm*vlines[vi]+mb
		
		#reflection slope
		reft = 2*theta[i]-sunangle
		refm = tan(reft)
		b=yintm-refm*vlines[vi]
		abline(b, refm)
		
	}
}
