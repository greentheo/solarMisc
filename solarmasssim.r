#define the substance

volume = 5000 #gallons
weight = 8.8 #lbs
mass = volume*weight

#surface area of container
area = volume/750*(3*3^3 - 4*9)
#R value of insulation around container
R = 10 

#surface area of radiator 
areaRad = .25*1000
#R value of radiator
RRad = .5

#surface area of house
sqftHouse = 1100
areaHouse = sqftHouse*2+sqrt(sqftHouse)*8*4 #4 8 foot high walls, floor and ceiling
RHouse = 20 
massH = 50*sqftHouse

x = seq(0, 2*pi, length.out=(365*24*60))
tempfun = (88+15)/2+15*sin(x)+18*sin(365*x)
tempfunin = (88+15)/2+5*sin(x)+5*sin(365*x-pi/2) #inside temperature is phase shifted

solarParea = 64 #sq. ft
efficiency = .35
btuwatts = .293

#solarHouse Area
solarHarea = sqrt(sqftHouse)*8
efficiencyH = .05

solarkwh = rnorm(5.5/24/60, .1/24/60)
smin = 3.6
smax = 6.9
xday = seq(0, pi, length.out=365)
insolation = (smax+smin)/2+(smax-(smax+smin)/2)*sin(xday)
stochasticinsolation = NULL
day = seq(0, 2*pi, length.out=24*60)
for(i in 1:length(xday)){
	daysim = ifelse(sin(day)>0, rnorm(1, insolation[i], .1)*sin(day)/2, 0)*(pi/12)
	stochasticinsolation = c(stochasticinsolation, daysim)
}

watertemp = tempfun
solargain = stochasticinsolation
solargainH = solargain
heata = tempfun
heatb = tempfun #this would be syphoning off the heat for use in the house
heaters = heata
#simulate the temperature at every step

#heat gain/loss from ambient temperature
for(i in 1:(length(tempfun)-1)){
	heata[i] = area*(tempfun[i]-watertemp[i])/R
	heatb[i] = ifelse(tempfunin[i]<55, areaRad*(tempfunin[i]-watertemp[i])/RRad, 0)	
	solargain[i] = ifelse(watertemp[i]<180, solarParea/10.7*stochasticinsolation[i]*efficiency*1000*(1/btuwatts), 0)
	solargainH[i] = solarHarea/10.7*stochasticinsolation[i]*efficiencyH*1000*(1/btuwatts)
	watertemp[i+1] = watertemp[i]+(solargain[i]+heata[i])/mass+heatb[i]/mass
	heaters[i] = ifelse(tempfunin[i]<35, -4*3000, 0)
	tempfunin[i+1] = tempfunin[i]+solargainH[i]/massH+areaHouse*(tempfun[i]-tempfunin[i])/RHouse/massH - heatb[i]/massH - heaters[i]/massH

}
cat('spent ',-sum(heaters/60/btuwatts/1000*.12), ' heating the house \n')
cat('generated ', -sum(heatb/60/btuwatts/1000*.12), ' of heat \n') 

par(mfrow=c(2,1))
plot(tempfun[1:(5*24*60)], type="l", col = "red", ylim=c(min(tempfun, watertemp), max(tempfun, watertemp)))
lines(watertemp[1:(5*24*60)], col="blue")
lines(tempfunin[1:(5*24*60)], col="grey")
legend("topright", legend=c("amb", "water", "inside"), col=c("red", "blue", "grey"), lty=1)
plot(watertemp, type="l",col="blue",ylim=c(min(tempfun, watertemp), max(tempfun, watertemp)))
lines(tempfun, col="red")
lines(tempfunin, col="grey")
abline(h=63)
legend("topright", legend=c("amb", "water", "inside"), col=c("red", "blue", "grey"), lty=1)
