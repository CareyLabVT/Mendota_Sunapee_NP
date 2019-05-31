# Mapping Mendota & Sunapee

#install.packages('pacman')
pacman::p_load(maps)

# lat, long for Mendota, Sunapee
data <- data.frame(Lat = c(43, 43.395), 
                   Long = c(-89.41, -72.053))

# Lakes in contiguous USA
par(mar = c(0,0,0,0),mgp=c(0,0,0))
maps::map('state', col='black', wrap=T,lwd=2) 
maps::map('state', region=c('wisconsin','new hampshire'), add = T, lwd=2,
    col=c('dodgerblue', 'tomato'), fill=T) 
points(data$Long,data$Lat,col='black', pch=16, cex=1,lwd=1)

# Regional map
region_states <- c('minnesota','wisconsin','iowa', 'illinois','michigan',
                   'indiana','ohio','pennsylvania','west virginia','new jersey',
                   'new york','vermont','new hampshire','maine','massachusetts',
                   'rhode island','connecticut','maryland', 'missouri',
                   'kentucky','virginia')


# small US with region in gray, then blowup
par(mar = c(0,0,0,0),mgp=c(0,0,0))
map('state', col='black', wrap=T,lwd=2) 
map("state", region= region_states, add=T,
    lty = 1, border='black', lwd=2, col = 'gray80', fill=T)


# Region only
par(oma=c(2,2,1,1), mar = c(0,0,0,0),mgp=c(0,0,0))
map("state", region= region_states,
    lty = 1, border='black', lwd=2, col = 'gray92', fill=T)
map('state', region=c('wisconsin','new hampshire'), add = T,
    col=c('dodgerblue', 'tomato'), fill=T, lwd=2) 
points(data$Long,data$Lat,col='black', pch=16, cex=1.5,lwd=1)
axis(1, at=seq(-95,-70,5), las=1, padj=1.5, cex.axis=1.25)
mtext("Longitude", 1, line=2.5, cex=1.25)
axis(2, at=c(36,40,44,48), labels=c('36','40','44','48'), las=1, hadj=1.75, cex.axis=1.25)
mtext("Latitude", 2, line=2.5, cex=1.25)