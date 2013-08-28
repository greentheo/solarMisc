##calculate the length, height and etc. of a parabolic shape.

x = seq(0, 6, by=.05)

c = .25
a = .465
height = c*x[length(x)]^2
height = cosh(a*x[length(x)])-1

cat('parabola is ', height, 'tall \n')

lPar = sqrt(diff(c*x^2)^2+diff(x)^2)
lPar = sqrt(diff(cosh(a*x)-1)^2+diff(x)^2)

cat('parabola is ', sum(lPar), ' long (per side) \n')

fx = c*x^2
fx = cosh(a*x)-1

rad2Deg = function(x){
  return(x/2/pi*360)
}

dec2FootIn = function(x){
  return(floor(x)+ round((x-floor(x))*12)/100) 
}
##use this to figure out the angles between bends of a pipe to make a parabola.
a0 = atan(fx[2]/x[2])
cat('@x ',  x[1], ' @pipe ', cumsum(lPar)[1],rad2Deg(a0), ' \n')
apa0 = 0
ap = a0
angles = c(rad2Deg(a0))
xs = c(x[1])
fxs = c(fx[1])
is = c(1)
i=3
i1=2
xi1 = x[2]
fxi1 = fx[2]
while(i<length(x)){
  while(apa0<10 & x[i]-xi1<1 ){
    if(i>(length(x)-1)){
      break
    }
    ap = atan((fx[i]-fxi1)/(x[i]-xi1))
    apa0 = rad2Deg(ap)-rad2Deg(a0)
#     cat(apa0, ' ')
    i=i+1
  }
  cat('\n @x ', x[i1], ' @pipe ', dec2FootIn(cumsum(lPar)[i1]), ' @angle ', rad2Deg(ap)-rad2Deg(a0), '\n')
  angles = c(angles, rad2Deg(ap)-rad2Deg(a0))
  xs=c(xs, xi1)
  fxs=c(fxs, fxi1)
  is=c(is, i)
  a0 = ap
  apa0 = 0
  ap = 0
  cat(paste(a0, apa0, ap, sep=' '))
  xi1=x[i-1]
  fxi1=fx[i-1]
  i1 = i-1
}

xs = c(xs, xi1)
fxs = c(fxs, fxi1)

plot(x,fx, type="l", col="red")
segments(xs[1:(length(xs)-1)], fxs[1:(length(xs)-1)], xs[2:length(xs)], fxs[2:length(xs)], lty=1, lwd=2)