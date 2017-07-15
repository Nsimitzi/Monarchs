library(gstat)
library(maps)
library(maptools)


#Read in Zipcode Shapefile
ZipcodeFile <- readShapePoly("Zipcode Shapefiles(2015)/cb_2015_us_zcta510_500k.shp")


#Read Monarch data
dat1 = read.csv("Summer.csv",stringsAsFactors = F)
dat2 = read.csv("Winter.csv",stringsAsFactors = F)
dat3 = read.csv("overWintering.csv",stringsAsFactors = F)
dat14_15 = read.csv("2014_2015MHData.csv",stringsAsFactors = F)

d1 = cbind(dat1$Zipcode,dat1$Date.Collected,dat1$INF.status)
d2 = cbind(dat2$Zipcode,dat2$Date.Collected,dat2$INF.status)
d3 = cbind(dat3$Zipcode,dat3$Date.Collected,dat3$INF.status)
d4= cbind(dat14_15$Zipcode, dat14_15$Date_Collected,dat14_15$Infection.Status)

dat = rbind(d1,d2,d3,d4)
colnames(dat) = c("zip", "Date.Collected","status")
dat = as.data.frame(dat,stringsAsFactors = F)

#selects for valid dates
validDates = grep("[0-9]+/[0-9]+/[0-9]{4}", dat$Date.Collected)

#index rows with valid dates
dat = dat[validDates, ]

#presents in default format of xx/xx/xxxx
dat$Date.Collected = as.Date(dat$Date.Collected[validDates], "%m/%d/%Y")

#Finds unique zipcodes from the shapecode files
UniqueZipShapeCode = unique(dat$zip)

#Selects ZipCodes that are in both the shapecode and monarch data
SelectedZipCodes = ZipcodeFile$ZCTA5CE10 %in% UniqueZipShapeCode

#ZipcodeFile with selected zipcodes
updatedZipcodeFile = ZipcodeFile[SelectedZipCodes,]

#Selects valid zipcodes from monarch data
dat = dat[dat$zip %in% updatedZipcodeFile$ZCTA5CE10, ]

#Subset monarch dataset to only include relevant categories
monarch = dat
monarch$status = as.numeric(monarch$status)

#Organize monarch data by zipcode
monarchZip = data.frame(zip = sort(unique(monarch$zip)))


#plot per year by zipcode
yr = format(as.Date(monarch$Date.Collected),"%Y")
for(i.yr in 2011:2015){
  ind =  yr == i.yr
  
  mn2 = monarch[ind,]
  monarchZip = data.frame(zip = sort(unique(mn2$zip)) )
  
  #Calculate butterflies caught per zipcode
  monarchZip$Captured = aggregate(status ~ zip, data = mn2, FUN = length)[, 2]
  
  #Calculate infected proportion per zipcode
  monarchZip$Proportion = aggregate(status ~ zip, data = mn2, FUN = mean)[, 2]
  
  #Calculate number of infected per zipcode
  monarchZip$Infected = aggregate(status ~ zip, data = mn2, FUN = function(x) sum(x))[, 2]
  
  #Finds unique zipcodes from the shapecode files
  UniqueZipShapeCode = unique(mn2$zip)
  
  #Selects ZipCodes that are in both the shapecode and monarch data
  SelectedZipCodes = ZipcodeFile$ZCTA5CE10 %in% UniqueZipShapeCode
  
  #ZipcodeFile with selected zipcodes
  updatedZipcodeFile = ZipcodeFile[SelectedZipCodes,]

  pal = colorRampPalette(c("light gray", "red"))
  colors = pal(10)
  
  breaks = seq(0,max(monarchZip$Proportion),1/(length(colors)))
  
  val.levels = cut(monarchZip$Proportion, breaks,include.lowest = T)
  color.table = data.frame(level = levels(val.levels), color = I(colors))
  
  for(k in 1:length(updatedZipcodeFile$ZCTA5CE10)){
    updatedZipcodeFile@data$color[k] = color.table$color[val.levels[k] == color.table$level]
  }
  
  plot(updatedZipcodeFile,col=updatedZipcodeFile$color,border=updatedZipcodeFile$color)
  map("state", add=T, col="black")
  
}





