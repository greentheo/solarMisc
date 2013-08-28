nmax=10

revstart = function(xend, yend, n){
	#calculate the height of the equilateral triangle
	sh = sqrt(xend^2+(yend)^2)
	s = sqrt(4/3*sh^2)
	rads=s/(2*tan(pi/3))
	for(i in 4:n){
		rads = c(rads, s/(2*tan(pi/i)))
	}
	ns = c(3:n)[which.min(abs(rads-xend))]
	scalc=xend*2*tan(pi/n)
	c=yend/(xend^2)
	focus = 1/(4*c)

	return(list(ns=ns, s=s,scalc=scalc, sh=sh, rad=abs(rads)[which.min(abs(rads-xend))], focus=focus, c=c))
			
}

revmid = function(xstart, xend, ystart, yend, sstart, nstart){
	#start with a layer of triangles that fits on the previous base
	#then calculate how best to expand it
	#find the radius out to the corner of the bottom layer
	rad = sstart/(2*tan(pi/nstart))
	sc = sqrt((.5*sstart)^2+(rad)^2)
	
	shtopbase = sqrt(sc^2+(yend-ystart)^2)
	
	#now find the upside down layer of triangles basewidth given the number of triangles needed
	stopbase = xend*2*tan(pi/nstart)

	radtop = stopbase/(2*tan(pi/nstart))
	sctop = sqrt((.5*sc)^2+radtop^2)
	shbotbase = sqrt(sctop^2+(yend-ystart)^2)
	return(list(stopbase=stopbase, shtopbase=shtopbase, sbotbase=sstart, shbotbase=shbotbase))

}
fwdmid = function(xstart, xend, ystart, yend, n){
	
	s = xend*2*tan(pi/n)
	sbottom = xstart*2*tan(pi/n)
	sbottomheight = sqrt((xend-xstart)^2+(yend-ystart)^2)
	return(list(sh=sbottomheight, sbottom=sbottom, s=s))	
}

x = c(1,2,3)
y = x^2*.5

start = revstart(x[1],y[1],10)

nextlevel = fwdmid(x[1],x[2],y[1],y[2],12)

thirdlevel = fwdmid(x[2],x[3],y[2],y[3],14)

nextlevel = revmid(x[1],x[2],y[1],y[2],start$s,start$ns)

thirdlevel = revmid(x[2],x[3],y[2],y[3],start$s,start$ns)