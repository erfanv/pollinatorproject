#Load packages
source("packages.R")
#install.packages("plotly")
library("plotly")

#install.packages("stringr")
library("stringr")

#install.packages("Taxonstand")
library("Taxonstand")

#Import the raw data
rawdata <- read.csv("/Volumes/GoogleDrive/My\ Drive/Projects/Southern\ Region\ Pollinator\ CS\ Project/r-analysis/raw_data.csv", header=TRUE)

#check import
head(rawdata)

#check data summary
summary(rawdata)

#Provide new easy-to-write names for each column. Note that flower colors have changed, as the 'value' associated with the user input was actually incorrect in the original survey. True color values are as follows (data value = user selected value): blue_purple = red, white_pink = Orange, yellow_orange = Yellow, red = Green, green = Blue/Cyan, purple_indigo_ = Purple/Indigo/Violet, white = White/Pink.
names(rawdata) <- c("start","end","un","did","student_status","firstobs","email","loc","loclat","loclong","localt","locprec","time","temp","latname","cultivar","bloom","red","orange","yellow","green","blue","purp","whitepink","largebee","mediumbee","smallblbee","smallgbee","nonbeewasp","hoverflies","butterflies","photo","feedback","version","metaid","id","uuid","subtime","tags","notes","versionx","duration","submittedby","totalmedia","mediacount","mediareceived","xformid")
names(rawdata)

#Total rows of data
dim(rawdata)[1]

#Let's remove any "test" rows. Test rows inputted by Erfan and Mike Merchant. Also, several of the first rows were test input.
#View(rawdata)

#Remove rows 1 - 8
rawdata <- rawdata[-c(1:8),]

#Check for data points where hostplantselec is anything other than n/a, as it should be.
rawdata[rawdata$hostplantselec!="n/a",]

#check dimensions of latnames, i.e. number of unique values typed in for latin names.
dim(table(rawdata$latname))
dim(table(rawdata$cultivar))

#convert all characters in latin name and cultivar to lower case
rawdata$latname <- tolower(rawdata$latname)
rawdata$cultivar <- tolower(rawdata$cultivar)

#attach dataframe
attach(rawdata)
rawdata[which(names(rawdata)=="red"):which(names(rawdata)=="whitepink")]

#Change data type for temperature
rawdata$temp <- as.numeric(as.character(rawdata$temp))

#Change data type for the bloom colors from false/true to 0/1.
col <- c("red","orange","yellow","green","blue","purp","whitepink")

rawdata[,col] <- apply(rawdata[,col], 2, function(x) as.integer(as.logical(x)))
apply(rawdata[,col], 2, function(x) sum(x, na.rm=TRUE))


#Change all pollinator counts to numeric
pol <- c("largebee","mediumbee","smallblbee","smallgbee","nonbeewasp","hoverflies","butterflies")

rawdata[,pol] <- apply(rawdata[,pol], 2, function(x) as.numeric(as.character(x), na.rm=TRUE))
apply(rawdata[,pol], 2, function(x) sum(x, na.rm=TRUE))

attach(rawdata)

#create table of latin names
dim(table(latname))
dim(table(rawdata$cultivar))
table(latname)
table(cultivar)

#If latname is unknown (i.e. latname == "unknown"), then simply make the latname the same as cultivar, since people sometimes put the common name under cultivar.
rawdata[rawdata == "unknown"] <- NA
rawdata$latname[is.na(rawdata$latname)] <- rawdata$cultivar[is.na(rawdata$latname)]

#To complete the location data for every row, we will first sort the data by start time, first obs, and device ID
raw_sort <- rawdata[order(rawdata$start,rawdata$firstobs,rawdata$did),]
raw_sort[raw_sort == "n/a"] <- NA

#Once sorted, we can assume (and/or hope) that any blanks in email, loc, loclat, loclong, time, and temp are related to the last completed entry, since first obs == yes would be at the top for any given device id.
raw_sorted <- fill(raw_sort, email)
raw_sorted <- fill(raw_sorted, loc)
raw_sorted <- fill(raw_sorted, loclat)
raw_sorted <- fill(raw_sorted, loclong)
raw_sorted <- fill(raw_sorted, locprec)
raw_sorted <- fill(raw_sorted, time)
raw_sorted <- fill(raw_sorted, temp)

#Adding State and County
#Convert coordinate data to numeric
raw_sorted[,9] <- as.numeric(as.character(raw_sorted[,9]))
raw_sorted[,10] <- as.numeric(as.character(raw_sorted[,10]))

#Some of the longitude data is positive (i.e. "E" instead of "W"), putting some of these locations in China rather than Texas... Need to change all positive longitude points to negative.

#Test sample
raw_sorted <- transform(raw_sorted, loclong=ifelse(raw_sorted$loclong > 0, raw_sorted$loclong*-1, raw_sorted$loclong*1))

#install.packages("sp")
#install.packages("maps")
#install.packages("maptools")
library(sp)
library(maps)
library(maptools)

# The single argument to this function, pointsDF, is a data.frame in which:
#   - column 1 contains the longitude in degrees (negative in the US)
#   - column 2 contains the latitude in degrees

latlong2county <- function(pointsDF) {
  # Prepare SpatialPolygons object with one SpatialPolygon
  # per county
  counties <- map('county', fill=TRUE, col="transparent", plot=FALSE)
  IDs <- sapply(strsplit(counties$names, ":"), function(x) x[1])
  counties_sp <- map2SpatialPolygons(counties, IDs=IDs,
                                     proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Convert pointsDF to a SpatialPoints object 
  pointsSP <- SpatialPoints(pointsDF, 
                            proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Use 'over' to get _indices_ of the Polygons object containing each point 
  indices <- over(pointsSP, counties_sp)
  
  # Return the county names of the Polygons object containing each point
  countyNames <- sapply(counties_sp@polygons, function(x) x@ID)
  countyNames[indices]
}

raw_sorted$statecounty <- latlong2county(raw_sorted[,10:9])

#Split State and County
raw_sorted <- separate(data=raw_sorted, col = statecounty, into = c("state","county"))

#Now let's verify all plant taxonomic information, and add the data from theplantlist.org
raw_sorted_fulltax <- TPL(raw_sorted$latname, corr = TRUE, diffchar = 2, max.distance = 1)

raw_sorted <- cbind(raw_sorted,raw_sorted_fulltax)

#raw_done <- raw_sorted[,-50]
#fulltax <- raw_sorted %>%
#  filter(tax.Taxonomic.status!="")

names(raw_sorted)
str(raw_sorted)

#Save the cleaned data file
write.csv(raw_sorted, "cleaned_data.csv", row.names=FALSE)

#Save data file after unnecessary columns are removed, including people's emails
raw_public <- raw_sorted %>%
  select(start,end,did,student_status,firstobs,loclat,loclong,time,temp,latname,cultivar,bloom,red,orange,yellow,green,blue,purp,whitepink,largebee,mediumbee,smallblbee,smallgbee,nonbeewasp,hoverflies,butterflies,feedback,state,county, Family, New.Genus, New.Species, Authority, Taxonomic.status)


#Create a quick pie chart
?pie()
pie(table(latname))
pie(table(cultivar))

#Pie chart names are all ontop of each other. Make text further away from circle, to try and make them slightly more legible!
pie2 <- function (x, labels = names(x), edges = 200, radius = 0.8, clockwise = FALSE, 
          init.angle = if (clockwise) 90 else 0, density = NULL, angle = 45, 
          col = NULL, border = NULL, lty = NULL, main = NULL, ...) 
{
  if (!is.numeric(x) || any(is.na(x) | x < 0)) 
    stop("'x' values must be positive.")
  if (is.null(labels)) 
    labels <- as.character(seq_along(x))
  else labels <- as.graphicsAnnot(labels)
  x <- c(0, cumsum(x)/sum(x))
  dx <- diff(x)
  nx <- length(dx)
  plot.new()
  pin <- par("pin")
  xlim <- ylim <- c(-1, 1)
  if (pin[1L] > pin[2L]) 
    xlim <- (pin[1L]/pin[2L]) * xlim
  else ylim <- (pin[2L]/pin[1L]) * ylim
  dev.hold()
  on.exit(dev.flush())
  plot.window(xlim, ylim, "", asp = 1)
  if (is.null(col)) 
    col <- if (is.null(density)) 
      c("white", "lightblue", "mistyrose", "lightcyan", 
        "lavender", "cornsilk")
  else par("fg")
  if (!is.null(col)) 
    col <- rep_len(col, nx)
  if (!is.null(border)) 
    border <- rep_len(border, nx)
  if (!is.null(lty)) 
    lty <- rep_len(lty, nx)
  angle <- rep(angle, nx)
  if (!is.null(density)) 
    density <- rep_len(density, nx)
  twopi <- if (clockwise) 
    -2 * pi
  else 2 * pi
  t2xy <- function(t) {
    t2p <- twopi * t + init.angle * pi/180
    list(x = radius * cos(t2p), y = radius * sin(t2p))
  }
  for (i in 1L:nx) {
    n <- max(2, floor(edges * dx[i]))
    P <- t2xy(seq.int(x[i], x[i + 1], length.out = n))
    polygon(c(P$x, 0), c(P$y, 0), density = density[i], angle = angle[i], 
            border = border[i], col = col[i], lty = lty[i])
    P <- t2xy(mean(x[i + 0:1]))
    lab <- as.character(labels[i])
    if (!is.na(lab) && nzchar(lab)) {
      lines(c(1, 1.35) * P$x, c(1, 1.35) * P$y)
      text(1.5 * P$x, 1.5 * P$y, labels[i], xpd = TRUE, 
           adj = ifelse(P$x < 0, 1, 0), ...)
    }
  }
  title(main = main, ...)
  invisible(NULL)
}

pie2(table(latname))

View(latname)

genusname <- word(latname, 1)
pie2(table(genusname))

#list all unique values of latin names
latnames <- unique(latname)
latnames <- sort(latnames)
View(latnames)

write.csv(latnames,"/Volumes/GoogleDrive/My\ Drive/Projects/Southern\ Region\ Pollinator\ CS\ Project/r-analysis/latnames.csv")
#fulltaxon <- read.csv("/Volumes/GoogleDrive/My\ Drive/Projects/Southern\ Region\ Pollinator\ CS\ Project/r-analysis/fulltax.csv")

fulltaxon <- TPL(rawdata$latname, corr = TRUE, diffchar = 2, max.distance = 1)

View(fulltaxon)

fulltax <- fulltaxon %>%
  filter(Taxonomic.status!="")

pie2(table(fulltax$New.Genus))


p <- fulltax[1:20] %>%
  group_by(Genus) %>%
  summarize(count = n()) %>%
  plot_ly(labels = ~Genus, values = ~count) %>%
  add_pie(hole = 0.6) %>%
  layout(title = "Pie chart of plant genus observations",  showlegend = F,
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

p



fulltaxtable <- sort(table(fulltax$New.Genus), decreasing = TRUE)
par(mar=c(15,4,4,2))
barplot(fulltaxtable[1:20], ylim=c(0,300), las=2)

pie2(fulltaxtable[1:20])

write.csv(fulltaxon,"/Volumes/GoogleDrive/My\ Drive/Projects/Southern\ Region\ Pollinator\ CS\ Project/r-analysis/fulltax.csv")
install.packages("knit2wp")
knit2wp('/Users/erfanv/Downloads/plotly\ test.Rmd', title = 'Your post title')


install.packages("devtools")
library("devtools")
if (!require('RWordPress')) {
  devtools::install_github(c("duncantl/XMLRPC", "duncantl/RWordPress"))
}
library(RWordPress)
