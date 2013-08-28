#import the solar data

library(xts)


r = read.csv('data/cabin_solar.csv', quote="")
#remove anything over 1000 w/m^2
r = r[-which(r[,2]>1000 | r[,2]<=0), ]
solar = xts(r[,2], order.by=as.POSIXct(format(r[,1], format="%Y-%m-%d %H:%i:%s")))

r = read.csv('data/ECO_wind.csv', quote="")
#wind = xts(.5*1.2*(r[,2]*.44)^3*.6, order.by=as.POSIXct(format(r[,1], format="%Y-%m-%d %H:%i:%s")))
r = r[-which(r[,2]>5000),]
wind = xts(r[,2], order.by=as.POSIXct(format(r[,1], format="%Y-%m-%d %H:%i:%s")))


r = read.csv('data/cabin_temp2.csv', quote="")
r = r[-which(r[,2]>175 | r[,2]<0), ]
box = xts(r[,2], order.by=as.POSIXct(format(r[,1], format="%Y-%m-%d %H:%i:%s")))

r = read.csv('data/cabin_temp0.csv', quote="")
r = r[-which(r[,2]<85), ]
water = xts(r[,2], order.by=as.POSIXct(format(r[,1], format="%Y-%m-%d %H:%i:%s")))

#make a couple of plots

mat = cbind(solar=solar, box=box, water=water, wind=wind)
mat = na.locf(mat, na.rm=T)

#first a plot comparing solar to box and water temp

pdf('SWH_analysis.pdf')

plot(density(as.numeric(solar[solar>100])), main="Distribution of Hourly Potential Solar Energy ", sub=paste("mean solar power of ", mean(as.numeric(solar[solar>100])), " w/m^2", sep=""))
abline(v=mean(as.numeric(solar[solar>100])))

#solar kwH by day.
kwh = apply.daily(solar, sum)/1000
par(mfrow=c(2,1))

plot(kwh, lty=1, main="Potential Solar Energy by day (KwH/m^2)", sub=paste("Mean potential ", mean(kwh), " (KwH/m^2) ", sep=""))
abline(v=mean(as.numeric(kwh)))

plot(density(as.numeric(kwh)), "Distribution of Daily Solar Potential (KwH/m^2)", sub=paste("Mean potential ", mean(kwh), " (KwH/m^2) ", sep=""))
abline(h=mean(as.numeric(kwh)))

par(mfrow=c(1,1))

#wind potential
wkwh = apply.daily(wind,sum)/1000

plot(density(as.numeric(wind)), main="Distribution of Hourly Potential Wind Energy ", sub=paste("mean wind power of ", mean(as.numeric(wind)), " w/m^2", sep=""))
abline(v=mean(as.numeric(wind)))


par(mfrow=c(2,1))

plot(wkwh, lty=1, main="Potential Wind Energy by day (KwH/m^2)", sub=paste("Mean potential ", mean(wkwh), " (KwH/m^2) ", sep=""))
abline(v=mean(as.numeric(wkwh)))

plot(density(as.numeric(wkwh)), "Distribution of Daily Wind Potential (KwH/m^2)", sub=paste("Mean potential ", mean(wkwh), " (KwH/m^2) ", sep=""))
abline(h=mean(as.numeric(wkwh)))

par(mfrow=c(1,1))

#sort solar values
ind = sort(as.numeric(mat[,"solar"]), index.return=T, decreasing=T)$ix
plot(x=as.numeric(mat[ind,"solar"]), y=as.numeric(mat[ind, "box"]), col="blue", pch=1, main="Temperature of SHW panel and water vs. solar", ylab="deg. F", xlab="solar w/m^2")
points(x=as.numeric(mat[ind,"solar"]), y=as.numeric(mat[ind, "water"]), col="red", pch=2)
legend("topleft", pch=c(1,2), legend=c("panel", "water"), col=c("blue", "red"))

plot(x=as.numeric(mat[ind,"box"]), y=as.numeric(mat[ind, "water"]), pch=1, main="Temperature of water  vs. SHW panel", ylab="deg. F Water", xlab="deg. F Panel")

plot(x=as.numeric(mat[ind,"solar"]), y=as.numeric(mat[ind, "water"]), pch=1, main="Temperature of water  vs. Solar Potential", ylab="deg. F water", xlab="w/m^2")


#a histogram of solar potential





#difference plots
#take the difference in water temperature vs. solar irradiation
boxdiff = diff(mat[,"box"])
waterdiff = diff(mat[,"water"])
solardiff = diff(mat[,"solar"])
plot(x=as.numeric(solardiff[ind]), y=as.numeric(boxdiff[ind]), pch=1, main="1 hour temp difference of SHW panel vs. 1 hour diff in solar level", ylab="deg. F", xlab="solar w/m^2")
abline(h=0)
abline(v=0)

plot(x=as.numeric(solardiff[ind]), y=as.numeric(waterdiff[ind]), pch=1, main="1 hour temp difference of water vs. 1 hour diff in solar level", ylab="deg. F", xlab="solar w/m^2")
abline(h=0)
abline(v=0)

heatbox = array(0, c(30,30))
heatwater = array(0, c(30,30))
solarbins = c(0,seq(300, 1000, length.out=30))
solardiffbins = c(-700,seq(-350, 400, length.out=30))
for(i in 2:length(solarbins)){
	for(j in 2:length(solardiffbins)){
		indbins = intersect(which(solardiff<solardiffbins[j] & solardiff >=solardiffbins[j-1]), 
			which(mat[,"solar"]<solarbins[i] &mat[,"solar"]>=solarbins[i-1]))
		heatbox[i-1,j-1] = mean(as.numeric(boxdiff[indbins]))
		heatwater[i-1,j-1] = mean(as.numeric(waterdiff[indbins]))
	}

}
heatbox[is.nan(heatbox)] = 0
heatwater[is.nan(heatwater)] = 0

heatmap(heatbox, Rowv=NA, Colv=NA,col=heat.colors(100),main="Panel Temp Diff vs Solar Potential and 1 hour SP diff.", ylab="Solar Potential w/m^2", xlab="1 hour solar diff", labRow=round(solarbins), labCol=round(solardiffbins))

heatmap(heatwater, Rowv=NA, Colv=NA,col=heat.colors(100),main="Water Temp Diff vs Solar Potential and 1 hour SP diff.", ylab="Solar Potential w/m^2", xlab="1 hour solar diff", labRow=round(solarbins), labCol=round(solardiffbins))

dev.off()
