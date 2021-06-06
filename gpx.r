library(XML)
library(OpenStreetMap)
library(lubridate)
## 
## Attaching package: 'lubridate'
## The following object is masked from 'package:base':
## 
##     date
library(ggmap)
## Loading required package: ggplot2
library(ggplot2)
library(raster)
## Loading required package: sp
library(sp)
  


  shift.vec <- function (vec, shift) {
    if(length(vec) <= abs(shift)) {
      rep(NA ,length(vec))
    }else{
      if (shift >= 0) {
        c(rep(NA, shift), vec[1:(length(vec)-shift)]) }
      else {
        c(vec[(abs(shift)+1):length(vec)], rep(NA, abs(shift))) } } }
  
  options(digits=10)
  # Parse the GPX file
  pfile <- htmlTreeParse(file = "./voor de storm.gpx", error = function(...) {
  }, useInternalNodes = T)
  
  # Get all elevations, times and coordinates via the respective xpath
  elevations <- as.numeric(xpathSApply(pfile, path = "//trkpt/ele", xmlValue))
  times <- xpathSApply(pfile, path = "//trkpt/time", xmlValue)
  coords <- xpathSApply(pfile, path = "//trkpt", xmlAttrs)
  
  str(coords)
  # Extract latitude and longitude from the coordinates
  lats <- as.numeric(coords["lat",])
  lons <- as.numeric(coords["lon",])

  # Put everything in a dataframe and get rid of old variables
  geodf <- data.frame(lat = lats, lon = lons, ele = elevations, time = times)
  rm(list=c("elevations", "lats", "lons", "pfile", "times", "coords"))
  head(geodf)
  
  # Shift vectors for lat and lon so that each row also contains the next position.
  geodf$lat.p1 <- shift.vec(geodf$lat, -1)
  geodf$lon.p1 <- shift.vec(geodf$lon, -1)
  head(geodf)
  
  
  geodf$dist.to.prev <- apply(geodf, 1, FUN = function (row) {
    pointDistance(c(as.numeric(row["lat.p1"]),
                    as.numeric(row["lon.p1"])),
                  c(as.numeric(row["lat"]), as.numeric(row["lon"])),
                  lonlat = T)
  })
  
  head(geodf$dist.to.prev)
  
  # Calculate distances (in metres) using the function pointDistance from the 'raster' package.
  # Parameter 'lonlat' has to be TRUE!
  td <- sum(geodf$dist.to.prev, na.rm=TRUE)
  print(paste("The distance run was ", td, " meters"))
  
  # Transform the column 'time' so that R knows how to interpret it.
  geodf$time <- strptime(geodf$time, format = "%Y-%m-%dT%H:%M:%OS")
  # Shift the time vector, too.
  geodf$time.p1 <- shift.vec(geodf$time, -1)
  # Calculate the number of seconds between two positions.
  geodf$time.diff.to.prev <- as.numeric(difftime(geodf$time.p1, geodf$time))
  
  head(geodf$time.diff.to.prev, n=15) 
  
  # Calculate metres per seconds, kilometres per hour and two LOWESS smoothers to get rid of some noise.
  geodf$speed.m.per.sec <- geodf$dist.to.prev / geodf$time.diff.to.prev
  geodf$speed.km.per.h <- geodf$speed.m.per.sec * 3.6
  geodf$speed.km.per.h <- ifelse(is.na(geodf$speed.km.per.h), 0, geodf$speed.km.per.h)
  geodf$lowess.speed <- lowess(geodf$speed.km.per.h, f = 0.2)$y
  geodf$lowess.ele <- lowess(geodf$ele, f = 0.2)$y
  
  # Plot elevations and smoother
  plot(geodf$ele, type = "l", bty = "n", xaxt = "n", ylab = "Elevation", xlab = "", col = "grey40")
  lines(geodf$lowess.ele, col = "red", lwd = 3)
  legend(x="bottomright", legend = c("GPS elevation", "LOWESS elevation"),
         col = c("grey40", "red"), lwd = c(1,3), bty = "n")
  
  # Plot speeds and smoother
  plot(geodf$speed.km.per.h, type = "l", bty = "n", xaxt = "n", ylab = "Speed (km/h)", xlab = "",
       col = "grey40")
  lines(geodf$lowess.speed, col = "blue", lwd = 3)
  legend(x="bottom", legend = c("GPS speed", "LOWESS speed"),
         col = c("grey40", "blue"), lwd = c(1,3), bty = "n")
  abline(h = mean(geodf$speed.km.per.h), lty = 2, col = "blue")
  
  # Plot the track without any map, the shape of the track is already visible.
  plot(rev(geodf$lon), rev(geodf$lat), type = "l", col = "red", lwd = 3, bty = "n", ylab = "Latitude", xlab = "Longitude")
  
  
  plot(geodf[order(-geodf$speed.m.per.sec),"speed.km.per.h"])
  hist(geodf$speed.km.per.h)
  
  
  
  ggplot(data = geodf, mapping = aes(x=speed.km.per.h)) + 
    geom_histogram(aes(y=..density..),fill="bisque",color="white",alpha=0.7) + 
    geom_density() +
    geom_rug() +
    labs(x='speed') +
    theme_minimal()

    library(OneR)
  library(ggplot2)
b = bin(geodf$speed.km.per.h, nbins = 80, labels = NULL, method = c("length", "content",
                                                   "clusters"), na.omit = TRUE)

dfb = data.frame(b)
ggplot(dfb, aes(x = b, y = length(b))) + theme_bw() + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = -90))

dfbb = data.frame(table(dfb$b))
dfbb$ID <- seq.int(nrow(dfbb))
dfbb_rordered = dfbb[order(-dfbb$ID),]
cumsum(dfbb_rordered$Freq)
dfbb_rordered$cumsum = cumsum(dfbb_rordered$Freq) 
dfbb_rordered$percentile = round((dfbb_rordered$cumsum / sum(dfbb$Freq)) * 100,0)

p = ggplot(dfbb_rordered, aes(x = Var1, y = percentile)) + theme_bw() + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = -90))
p + geom_bar( aes(y=Freq), colour="Black") 
p

# A few constants
temperatureColor <- "#69b3a2"
priceColor <- rgb(0.2, 0.6, 0.9, 1)

adjustment = max(dfbb_rordered$Freq) / 100
p = ggplot(dfbb_rordered, aes(x=Var1, group=1)) 
p = p + geom_line( aes(y=percentile), colour="Black") 
p = p + geom_bar( aes(y=Freq/adjustment, group=1), stat="identity", size=.1, fill=temperatureColor, color="black", alpha=.4) + geom_text(aes(y=Freq/adjustment, group=1,label=percentile), vjust=1.6, color="black", size=3.5)
p = p + scale_y_continuous(
    # Features of the first axis
    name = "Percentile",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.* adjustment, name="No of measurement")
  )  +
  theme(
    axis.title.y = element_text(color = temperatureColor, size=13),
    axis.title.y.right = element_text(color = priceColor, size=13)
  ) +  ggtitle("Speeds and percentiles")
p = p + theme(axis.text.x = element_text(angle = -90))
p
