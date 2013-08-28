source('fresnelSim.r')
to8 = function(x){
	whole = floor(x)
	frac = round((x-whole)*8)
	frac
}
to10 = function(x){
	whole = floor(x)
	frac = round((x-whole)*20)/2
	frac
}

f=360 #focus length
w=12  #width of reflecting plane... 
w=sqrt(2)*6
w=12
## note about setup ##
# the reflection off the inner radius of the reflector is use to point to the focus
#the top edge of the mirror would reflect approximately w higher than the focus

r1 = seq(2, 48-w, by=w)
r1 = seq(0, 120-w, by=w)
r1 = seq(w/2,36, by=w)

#the sets of mirrors that we would like to calculate

eps = .01
tol =.05 #tolerance for height of mirror to be within 1/8 inch
#the smallest incrememnt to move the next mirror back by

col_eff = .6 #efficiency of collector tube
gen_eff = .6#efficiency of generator to electricity

r2 = r1+w
theta = rep(0, length(r1))
alpha = theta


#ths function finds the angle for the mirror to be tilted up at.
thetafind = function(f, x, w=0){
	theta = (pi/2-atan(f/(x+w)))/2
}

for(r in 1:(length(r1)-1)){
	theta[r] = thetafind(f, r1[r]+w/2)
	alpha[r] = 2*pi*(1-(2*pi*(cos(theta[r])*w+r1[r]))/(2*pi*(r1[r]+w)))
	#now adjust the next r so that it doesn't collide with the mirror in fornt of it
	rgood=F
	i=1
	while(!rgood){
		r1[r+1] = r1[r]+w+i*eps
		y2 = sin(theta[r])*w
		thetanew = thetafind(f, r1[r+1]+w/2)#pi/2-atan(f/r1[r+1])
		reflect = atan(f/r1[r+1])
		lengthr1 = cos(theta[r])*w
		lengthr2r1 = r1[r+1] - r1[r] - lengthr1
		lengthbaseback = sqrt(lengthr2r1^2+y2^2)

		#check to see that if at back side of the first reflector any of the light from the second
		#will run into it... and also that the height is really close to being a multiple of 1/8 (for easier measuring)

		rgood=ifelse(lengthbaseback>(y2/sin(reflect)), T, F)& ifelse((abs(sin(thetanew)*w/(1/20)-(sin(thetanew)*w)%/%(1/20)))<tol, T, F)

		i=i+1
	}
	
}
fresmat = cbind(r1=r1, theta=theta/2/pi*360, alpha=alpha/2/pi*360, height=sin(theta)*w, 
	tenths=to10(sin(theta)*w))
fresmat = fresmat[1:(nrow(fresmat)-1), ]
print(fresmat)

cat('collection area ', sum(pi*((cos(theta)*(r1+w)))^2-r1^2), ' if in^2 then m^2: ', sum(pi*((cos(theta)*(r1+w)))^2-r1^2)/144/10.7, '\n')
cat('onto a focus point ', w^2, '\n')
cat('approximate concentration ',sum(pi*((cos(theta)*(r1+w)))^2-r1^2)/w^2, ' \n')
cat('collecting about', sum(pi*((cos(theta)*(r1+w)))^2-r1^2)/144/10.7*1000, ' W, producing: ', sum(pi*((cos(theta)*(r1+w)))^2-r1^2)/144/10.7*1000*col_eff*gen_eff, ' w \n')


for(pa in seq(pi/2+3*pi/8, pi/2-3*pi/8, length.out = 100)){
#for(pa in pi/4){
	Sys.sleep(.1)
	reflectSim(c(0,r1[-length(r1)]), c(0,theta[-length(r1)]), focus=f, sunangle=pa, w=12, offset=0)


}

