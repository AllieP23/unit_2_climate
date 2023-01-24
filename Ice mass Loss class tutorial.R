#Allie 
#1-24-23
#class notes
#Ice mass loss over poles


ant_ice_loss= read.table(file="data/antarctica_mass_200204_202209.txt", skip=31, sep="", header=FALSE, col.names=c("decimal_date", "mass_Gt", "sigma_Gt"))
typeof(ant_ice_loss)
class(ant_ice_loss)
dim(ant_ice_loss)
grn_ice_loss= read.table(file="data/greenland_mass_200204_202209.txt", skip=31, sep="", header=FALSE, col.names=c("decimal_date", "mass_Gt", "sigma_Gt"))
head(grn_ice_loss) #gives you a look at the first 6 columns in the data set
tail(grn_ice_loss) #gives you a look at the last 6 columns in the data set
summary(ant_ice_loss) #gives you a summary of the data

#plot it!!
plot(mass_Gt ~ decimal_date, data=ant_ice_loss, ylab="Antarctica Mass Loss (Gt)")
plot(mass_Gt ~ decimal_date, data=grn_ice_loss, ylab="Greenland Mass Loss(Gt)")

#Plot both lines together
plot(mass_Gt ~ decimal_date, data=ant_ice_loss, ylab="Ice Sheet Mass Loss (Gt)", type="l")
line(mass_Gt ~ decimal_date, data=grn_ice_loss, col='red')
plot(mass_Gt ~ decimal_date, data=ant_ice_loss, ylab="Antarctica Mass Loss (Gt)", type='l', ylim=range(grn_ice_loss$mass_Gt)) + lines(mass_Gt ~decimal_date, data=grn_ice_loss, type='l', col='red')

#create a data.frame with an NA between the GRACE missions
data_break = data.frame(decimal_date=2018.0, mass_Gt=NA, sigma_Gt=NA)
data_break

#Add NA data point to the Antarctica ice trends data frame 
ant_ice_loss_with_NA = rbind(ant_ice_loss, data_break) #Merge ant_ice_loss data frame with our NA point
tail(ant_ice_loss_with_NA)
order(ant_ice_loss_with_NA$decimal_date)
ant_ice_loss_with_NA = ant_ice_loss_with_NA[order(ant_ice_loss_with_NA$decimal_date),]


#Repeat with Greenland data.frame
grn_ice_loss_with_NA = rbind(grn_ice_loss, data_break)
grn_ice_loss_with_NA = grn_ice_loss_with_NA[order(ant_ice_loss_with_NA$decimal_date),]

plot(mass_Gt ~ decimal_date, data=ant_ice_loss_with_NA, ylab="Antarctica Mass Loss (Gt)", type='l', ylim=range(grn_ice_loss_with_NA$mass_Gt, na.rm=TRUE)) +
  lines(mass_Gt ~ decimal_date, data=grn_ice_loss_with_NA, type='l', col='red')



#NASA provided sigma of 68 percent 
head(ant_ice_loss_with_NA)
plot(mass_Gt ~ decimal_date, data=ant_ice_loss_with_NA, ylab="Antarctica Mass Loss (Gt)", xlab="Year", type='l') +
  lines((mass_Gt+2*sigma_Gt) ~ decimal_date, data=ant_ice_loss_with_NA, type='l', lty='dashed') +
  lines((mass_Gt+2*sigma_Gt) ~ decimal_date, data=ant_ice_loss_with_NA, type='l', lty='dashed')


#Saving plots to hard drive
pdf('figures/ice_mass_trends.pdf', width=7, height=7)
plot(mass_Gt ~ decimal_date, data=ant_ice_loss_with_NA, ylab="Ice Sheet Mass Loss (Gt)", xlab="Year", type='l', ylim=range(grn_ice_loss_with_NA$mass_Gt, na.rm=TRUE), lwd=2) + 
  lines((mass_Gt+2*sigma_Gt) ~ decimal_date, data=ant_ice_loss_with_NA, type='l', lty='dashed') + 
  lines((mass_Gt-2*sigma_Gt) ~ decimal_date, data=ant_ice_loss_with_NA, type='l', lty='dashed') +
  lines(mass_Gt ~ decimal_date, data=grn_ice_loss_with_NA, type='l', col='red', lwd=2) +
  lines((mass_Gt+2*sigma_Gt) ~ decimal_date, data=grn_ice_loss_with_NA, type='l', lty='dashed', col="red") +
  lines((mass_Gt-2*sigma_Gt) ~ decimal_date, data=grn_ice_loss_with_NA, type='l', lty='dashed', col="red")
dev.off()      

#making a barplot

#largest observed decrease in ice mass in Ant
min(ant_ice_loss$mass_Gt)

#Barplot of largest observed decrease in ice loss in Ant
barplot(height=c(min(ant_ice_loss$mass_Gt), min(grn_ice_loss$mass_Gt)))

#Flip to negative to positive, add x axis labels

barplot(height=c(min(ant_ice_loss$mass_Gt)*(-1), min(grn_ice_loss$mass_Gt)*(-1)), names.arg=c("Antarctica","Greenland"), ylim=c(0,5000), ylab="Ice loss in Gt")

        