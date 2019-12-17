#Purpose of this document is to "append" new data to what we already have, in order to decrease load on TPL (theplantlist.org)
#load packages
source("packages.R")
library("Taxonstand")
library("stringr")

old_data <- read.csv("cleaned_data.csv", header=TRUE)

#Subset old data for the purpose of practice/demonstration
all_data <- read.csv("raw_data.csv",header=TRUE)

#Provide new easy-to-write names for each column. Note that flower colors have changed, as the 'value' associated with the user input was actually incorrect in the original survey. True color values are as follows (data value = user selected value): blue_purple = red, white_pink = Orange, yellow_orange = Yellow, red = Green, green = Blue/Cyan, purple_indigo_ = Purple/Indigo/Violet, white = White/Pink.
names(all_data) <- c("start","end","un","did","student_status","firstobs","email","loc","loclat","loclong","localt","locprec","time","temp","latname","cultivar","bloom","red","orange","yellow","green","blue","purp","whitepink","largebee","mediumbee","smallblbee","smallgbee","nonbeewasp","hoverflies","butterflies","photo","feedback","version","metaid","id","uuid","subtime","tags","notes","versionx","duration","submittedby","totalmedia","mediacount","mediareceived","xformid")

#Let's remove any "test" rows. Test rows inputted by Erfan and Mike Merchant. Also, several of the first rows were test input.
#View(all_data)

#Remove rows 1 - 8
all_data <- all_data[-c(1:8),]

#convert all characters in latin name and cultivar to lower case
all_data$latname <- tolower(all_data$latname)
all_data$cultivar <- tolower(all_data$cultivar)

#attach dataframe
attach(all_data)

#Change data type for temperature
all_data$temp <- as.numeric(as.character(all_data$temp))

#Change data type for the bloom colors from false/true to 0/1.
col <- c("red","orange","yellow","green","blue","purp","whitepink")

all_data[,col] <- apply(all_data[,col], 2, function(x) as.integer(as.logical(x)))
apply(all_data[,col], 2, function(x) sum(x, na.rm=TRUE))

#Change all pollinator counts to numeric
pol <- c("largebee","mediumbee","smallblbee","smallgbee","nonbeewasp","hoverflies","butterflies")

all_data[,pol] <- apply(all_data[,pol], 2, function(x) as.numeric(as.character(x), na.rm=TRUE))
apply(all_data[,pol], 2, function(x) sum(x, na.rm=TRUE))

attach(all_data)

#If latname is unknown (i.e. latname == "unknown"), then simply make the latname the same as cultivar, since people sometimes put the common name under cultivar.
all_data[all_data == "unknown"] <- NA
all_data$latname[is.na(all_data$latname)] <- all_data$cultivar[is.na(all_data$latname)]

#To complete the location data for every row, we will first sort the data by start time, first obs, and device ID
all_sort <- all_data[order(all_data$start,all_data$firstobs,all_data$did),]
all_sort[all_sort == "n/a"] <- NA

#Once sorted, we can assume (and/or hope) that any blanks in email, loc, loclat, loclong, time, and temp are related to the last completed entry, since first obs == yes would be at the top for any given device id.
all_sorted <- fill(all_sort, email)
all_sorted <- fill(all_sorted, loc)
all_sorted <- fill(all_sorted, loclat)
all_sorted <- fill(all_sorted, loclong)
all_sorted <- fill(all_sorted, locprec)
all_sorted <- fill(all_sorted, time)
all_sorted <- fill(all_sorted, temp)

#Remove any rows that aren't "New"
x <- nrow(old_data)+1
y <- nrow(all_sorted)

#Verify by using email and time of day that where we are "spitting" are similar between the two datasets
#old_data[x-1,c(1,7)]
#all_sorted[x-1,c(1,7)]

old_data[x-1,c(1)] == all_sorted[x-1,c(1)] && old_data[x-1,c(7)] == all_sorted[x-1,c(7)]

new_data <- all_sorted[x:y,]

#Adding State and County
#Convert coordinate data to numeric
new_data$loclat <- as.numeric(as.character(new_data$loclat))
new_data$loclong <- as.numeric(as.character(new_data$loclong))

#Some of the longitude data is positive (i.e. "E" instead of "W"), putting some of these locations in China rather than Texas... Need to change all positive longitude points to negative.

#Test sample
new_data <- transform(new_data, loclong=ifelse(new_data$loclong > 0, new_data$loclong*-1, new_data$loclong*1))

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

new_data$statecounty <- latlong2county(new_data[,10:9])

#Split State and County
new_data <- separate(data=new_data, col = statecounty, into = c("state","county"))

#Before checking and pulling information on all of the latnames from theplantlist.org database, lets use a manual list produced by Danielle Dunn to correct some of the latnames
corrected_latnames <- read.csv("corrected_latnames.csv", header=TRUE)
corrected_latnames$Incorrect <- as.character(corrected_latnames$Incorrect)
corrected_latnames$Correct <- as.character(corrected_latnames$Correct)

old_data$latname <- as.character(old_data$latname)
new_data$latname <- as.character(new_data$latname)

all <- vector()
for(i in 1:length(corrected_latnames$Incorrect)){
  y <- new_data$latname[new_data$latname %in% corrected_latnames$Incorrect[i]]
  all <- c(all,y)
}

#How many corrections were made?
length(all)

#Make corrections
for(i in 1:length(corrected_latnames$Incorrect)){
  new_data$latname[new_data$latname %in% corrected_latnames$Incorrect[i]] <- corrected_latnames$Correct[i]
}

#Now let's verify all plant taxonomic information, and add the data from theplantlist.org
all_sorted_fulltax <- TPL(new_data$latname, corr = TRUE, diffchar = 2, max.distance = 1)

new_data <- cbind(new_data,all_sorted_fulltax)

#Chagned from rbind to bind_rows from dplyr, due to slight differences in number of columns
complete_set <- bind_rows(new_data,old_data)

#Some of the data is only to genus level. We want to still collect Family and Genus-level information from those observations. We can do that by searching for any latnames with "sp." in them, then match the first word (genus) from that observation with a complete observation from the dataset and copy over the Family and Genus.

#complete_set <- new_data
complete_set$latname[str_detect(complete_set$latname, coll("sp."))]
length(complete_set$latname[str_detect(complete_set$latname, coll("sp."))])

#Make a dataframe of all the genus-only observations in the dataset
genus_only <- complete_set$latname[str_detect(complete_set$latname, coll("sp."))]
genus_only <- unique(genus_only)
genus_only <- genus_only[!is.na(genus_only)]
Family <- vector("character",length(genus_only))
New.Genus <- vector("character",length(genus_only))
New.Taxonomic.status <- vector("character",length(genus_only))

genus_only <- as.data.frame(cbind(genus_only, Family, New.Genus, New.Taxonomic.status))
genus_only$genus_only <- str_to_sentence(genus_only$genus_only)
genus_only$New.Genus <- word(genus_only$genus_only, 1)

full_tax_unique <- complete_set %>%
  select(New.Genus, Family, New.Taxonomic.status) %>%
  filter(New.Taxonomic.status=="Accepted") %>%
  unique(.)


#full_tax_unique$New.Genus[full_tax_unique$New.Genus==word(genus_only$genus_only[1], 1)]
#full_tax_unique$New.Genus[full_tax_unique$New.Genus==word(genus_only$genus_only[1], 1)]
#full_tax_unique$New.Genus[full_tax_unique$New.Genus==word(genus_only$genus_only[1], 1)]

#full_tax_unique[full_tax_unique$New.Genus=="lantana",]
merged_fam_genus <- merge(genus_only, full_tax_unique, by = "New.Genus", all.x=TRUE)
merged_fam_genus <- merged_fam_genus %>%
  select(New.Genus, Family.y, New.Taxonomic.status.y) %>%
  filter(New.Taxonomic.status.y=="Accepted")
names(merged_fam_genus) <- c("New.Genus","Family","New.Taxonomic.status")

#Total entries with lantana in the genus
length(complete_set$New.Genus[str_detect(complete_set$latname, coll("lantana"))])

#Total entries with lantana in the genus where the taxonomic status is listed as "Accepted".
length(complete_set$New.Genus[str_detect(complete_set$latname, coll("lantana")) & complete_set$Taxonomic.status=="Accepted"])

complete_set$genus_and_family <- "No"
complete_set$genus_and_family <- factor(complete_set$genus_and_family,
       levels = c("No","Yes"),
       labels = c("No","Yes"))

complete_set$genus_and_family[complete_set$New.Taxonomic.status=="Accepted"] <- "Yes"
old_correct <- length(complete_set$genus_and_family[complete_set$New.Taxonomic.status=="Accepted"])

#One major way to correct our data - use the full_tax_unique to populate any missing genus and family data from our complete dataset

#if genus_and_family==No, then merge fulltaxunique$family -> complete$family and fulltaxonunique$genus -> complete$genus and genus_and_Family<-"yes"
complete_set$New.Genus <- tolower(complete_set$New.Genus)
full_tax_unique$New.Genus <- tolower(full_tax_unique$New.Genus)
full_tax_unique$Family <- as.character(full_tax_unique$Family)
full_tax_unique$New.Taxonomic.status <- as.character(full_tax_unique$New.Taxonomic.status)


complete_set <- complete_set %>%
  left_join(full_tax_unique, by = "New.Genus")

complete_set$New.Taxonomic.status.y[is.na(complete_set$New.Taxonomic.status.y)] <- "Incomplete"

new_correct <- length(complete_set$genus_and_family[complete_set$New.Taxonomic.status.y=="Accepted"])

#Number of "corrections" made to taxonomic data, by adding in genus and family data.
new_correct - old_correct

complete_set$genus_and_family[complete_set$New.Taxonomic.status.y=="Accepted" & !is.na(complete_set$Family.y)] <- "Yes"

complete_set <- complete_set[,-which(names(complete_set) %in% c("New.Taxonomic.status.x","Family.x"))]

#List of all new plant families.
table(complete_set$Family.y)

names(complete_set)[names(complete_set)=="Family.y"] <- "Family"
names(complete_set)[names(complete_set)=="New.Taxonomic.status.y"] <- "New.Taxonomic.status"

names(new_data)


#A bit more data cleanup! Appears to be some bee counts that may be erroneous. In this case, we're removing any numbers above 60 or below 0 for pollinator counts.
#Also create a boxplot to quickly view any anomolies
boxplot(complete_set[25:31])
boxplot(complete_set$temp)
complete_set$mediumbee[complete_set$mediumbee>60] <- NA
complete_set$mediumbee[complete_set$mediumbee<0] <- NA
complete_set$largebee[complete_set$largebee>60] <- NA
complete_set$nonbeewasp[complete_set$nonbeewasp>60] <- NA
complete_set$hoverflies[complete_set$hoverflies<0] <- NA
complete_set$butterflies[complete_set$butterflies<0] <- NA
complete_set$temp[complete_set$temp>150] <- NA
complete_set$temp[complete_set$temp<50] <- NA

boxplot(complete_set[25:31])

#Let's split the "end" time down by year, month, date and hour
complete_set <- transform(complete_set, end_year = substr(end,1,4), end_month = substr(end,6,7), end_date = substr(end,9,10), end_time = substr(end,12,19))
complete_set <- transform(complete_set, end_full_date = as.Date(paste(end_year,end_month,end_date,sep="-")))
complete_set <- transform(complete_set, end_week_num = strftime(end_full_date, format = "%V"))

write.csv(complete_set, "cleaned_data.csv", row.names=FALSE)

#write an archived version as well, in case of overwrite
write.csv(complete_set, paste("archived/cleaned_data",format(Sys.time(), "%m-%d-%Y"),".csv",sep=""), row.names=FALSE)

names(complete_set)
#Save data file after unnecessary columns are removed, including people's emails
raw_public <- complete_set %>%
  select(end_year,end_month,end_date, end_time,end_week_num,did,student_status,firstobs,loclat,loclong,time,temp,latname,cultivar,bloom,red,orange,yellow,green,blue,purp,whitepink,largebee,mediumbee,smallblbee,smallgbee,nonbeewasp,hoverflies,butterflies,feedback,state,county, Family, New.Genus, New.Species, Authority, Taxonomic.status, genus_and_family)

write.csv(raw_public, "data.csv", row.names=FALSE)
