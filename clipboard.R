loadData <- function(env){
    bv <- read.csv("detroit-blight-violations-s.csv", stringsAsFactors = FALSE)        
    # return(bv)
    bvs <- extractFields(bv)
    assign("bvs",bvs,.GlobalEnv)    
}
extractFields <- function(bv){
    # Select a subset of the fields
    bvs <- data.frame(bv$TicketID, bv$ViolationCode, bv$ViolationAddress,
                      bv$ViolationStreetNumber,bv$ViolationStreetName)  
    names(bvs) <- c("ID","Code","Address","VStrNr","VStrName")
    return(bvs)
}
loadData <- function(env){
    bv <- read.csv("detroit-blight-violations-s.csv", stringsAsFactors = FALSE)        
    # return(bv)
    bvs <- extractFields(bv)
    assign("bvs",bvs,.GlobalEnv)    
}
isInDaHouse <- function(lon,lat,house){
    # if((coord[1] > x1 & coord[2] < x2) & coord[2] > y1 & coord[2] < y2){
    if(lon > house["lon"] & lon < (house["lon"] + 0.0005) 
       & lat > house["lat"] & lat < (house["lat"] + 0.0005)){
        return(TRUE)
    }
}


getCoord <- function(bvsRow) {
    # bvsRow is like 26288 2566 GRAND BLVD\nDetroit, MI\n(42.36318237000006, -83.09167672099994)
    coord <- unlist(strsplit(toString(bvsRow),split = "\n"))[3] 
    # Now we have something like (42.32544917300004, -83.06413908999997)
    coord <- gsub("[()]","",coord)
    # coord <- unlist(strsplit(coord,split=","))
    coord <- strsplit(coord,split=",")
    return(coord)
}

checkCoordinates <- function(bvRow,e) {
    houseNo <- get0("houseNo",envir=e)
    houseDf <- get0("houses",envir=e)
    houseMax <- nrow(houseDf)
    prevHouse <- houseDf[houseMax,]
    bvLon <- as.numeric(bvRow["Lon"])
    bvLat <- as.numeric(bvRow["Lat"])                                   
    h <- isInDaHouse(bvLon,bvLat,prevHouse["lon"],prevHouse["lat"])
    if(!(h)){
        newHouseNo <- houseMax + 1
        newRow <- c(newHouseNo,bvLon,bvLat,1) #register 1st incident to new house
        # We make a new house with the bvRow coordinates as centrepoint.
        # If aiming for perfection, we'd need to check if the corners of the new house
        # are inside an existing house. If yes, we'd move the new house so that it won't
        # overlap with an existing...
        houseDf[newHouseNo,] <- newRow
    } 
    else {
        houseDf[houseMax,]$incidentCount <- houseDf[houseMax,]$incidentCount + 1
    }
    # assign("houses",houseDf,envir=e)
}

checkCoordinates2 <- function(incidentRow,h){
    houseMax <- nrow(h)
    prevHouse <- h[houseMax,]
    incidentLon <- as.numeric(incidentRow["Lon"])
    incidentLat <- as.numeric(incidentRow["Lat"])   
    if(!(isInDaHouse(incidentLon,incidentLat,prevHouse["lon"],prevHouse["lat"]))){
        newHouseNo <- houseMax + 1
        newRow <- c(newHouseNo,incidentLon,incidentLat,1)
        h[newHouseNo,] <- newRow
        print(h[newHouseNo,])
    } else {
        h[houseMax,]$incidentCount <- h[houseMax,]$incidentCount + 1
        print(h[houseMax,])
    }
    houses <<- h
    
    
    # assign("houses",h,pos=sys.frame(which=-1))
}        


checkCoordinates2 <- function(incidentRow,h){
    maxHouseNo <- nrow(h)
    prevHouse <- h[maxHouseNo,]
    incidentLon <- as.numeric(incidentRow["Lon"])
    incidentLat <- as.numeric(incidentRow["Lat"])   
    if(!(isInDaHouse(incidentLon,incidentLat,prevHouse["lon"],prevHouse["lat"]))){
        newHouseNo <- maxHouseNo + 1
        newHouse <- c(newHouseNo,incidentLon,incidentLat,1)
        h[newHouseNo,] <- newHouse
        # print(newHouse)
    } else {
        prevHouse$incidentCount <- prevHouse$incidentCount + 1
        h[maxHouseNo,] <- prevHouse
        # print(prevHouse)
    }
    houses <<- h
}        

setHouses2 <- function(incidents) {
    incidents <- incidents[order(incidents$Lon, incidents$Lat), ]
    #set the first house
    houseNo <- 1
    incidentCount <- 0
    houses <- data.frame(houseNo,incidents$Lon[1],incidents$Lat[1],incidentCount)
    names(houses) <- c("houseNo","lon","lat","incidentCount")
    
    #For each entry in incidents, check if it's coordinates match the last created. 
    #If yes, add one more incident to the number of incidents of that house.
    #If not, create a new house (because incidents entries are sorted)
    houses <- apply(incidents,1,checkCoordinates2,houses)
    return(houses)
    
}


setHouses3 <- function(incidents) {
    incidents <- incidents[order(incidents$Lon, incidents$Lat), ]
    #set the first house
    houseNo <- 1
    incidentCount <- 0
    houses <- data.frame(incidents$Lon[1],incidents$Lat[1],incidentCount)
    names(houses) <- c("Lon","Lat","incidentCount")
    
    #For each entry in incidents, check if it's coordinates match the last created. 
    #If yes, add one more incident to the number of incidents of that house.
    #If not, create a new house (because incidents entries are sorted)
    # houses <- apply(incidents,1,checkCoordinates2,houses)
    i = 1
    prevInc <- incidents[1,]
    for (inc in incidents) {
        if(isInDaHouse(inc["Lon"], inc["Lat"], prevInc["Lon"], prevInc["Lat"])){
            houses[i]$incidentCount <-  houses[i]$incidentCount + 1
        } else {
            houses[i] <- c(inc["Lon"], inc["Lat"],1)
        }
        prevInc <- inc
    }
    return(houses)
    
}
checkCoordinates4 <- function(incidentRow,h,e){
    prevH <- get0("prevHouse",envir=e)
    incidentLon <- as.numeric(incidentRow["Lon"])
    incidentLat <- as.numeric(incidentRow["Lat"])   
    if(!(isInDaHouse(incidentLon,incidentLat,prevH["lon"],prevH["lat"]))){
        # New House, append it to houses
        newHouseNo <- as.numeric(prevH["houseNo"]) + 1
        newHouse <- c(newHouseNo, incidentLon, incidentLat, 1)
        rbind(h,newHouse)
        prevHouse <<- newHouse
    } else {
        prevH$incidentCount <- prevH$incidentCount + 1
        prevHouse <<- prevH
    }
    houses <<- h
}        

setHouses4 <- function(incidents) {
    incidents <- incidents[order(incidents$Lon, incidents$Lat), ]
    #set the first house
    houseNo <- 1
    incidentCount <- 0
    e <- environment()
    houses <- data.frame(houseNo,incidents$Lon[1],incidents$Lat[1],incidentCount)
    names(houses) <- c("houseNo","lon","lat","incidentCount")
    prevHouse <- houses[1,]
    #For each entry in incidents, check if it's coordinates match the last created. 
    #If yes, add one more incident to the number of incidents of that house.
    #If not, create a new house (because incidents entries are sorted)
    houses <- apply(incidents,1,checkCoordinates4,houses,e)
    return(houses)
    
}

main <- function(){
    # Something wrong with the environments. bvs <- main() won't work. Somehow getViolationCoordinates() sees bvs as null.
    mainEnv <- environment()
    bvs <- loadData(mainEnv)
    assign("bvs",bvs,.GlobalEnv)
    coo <- getViolationCoordinates()
    bvs["Lat"] <- as.numeric(coo[,1]) #For some reson, Lat and Lon still won't be numeric
    bvs["Lon"] <- as.numeric(coo[,2])
    rm(coo)
    return(bvs)
}

## start 14.5.2016

isInDaHouse <- function(lon,lat,houseLon, houseLat){
    # 0.00001 degrees = 1.11m. Assume the coordinates represent the centre of the house.
    # Then the house boundaries are 0.00005 degr off in both directions
    offset <- 0.00005
    if(lon > (houseLon - offset) && lon < (houseLon + offset) 
       && lat > (houseLat - offset) && lat < (houseLat + offset)){
        return(TRUE)
    }else{
        return(FALSE)
    }
}

roundCoordinates <- function(bvs) {
    # Round to 4 decimal places. 0.0001 degrees = 11.1m. We round everything 
    # from 0.00005 to 0.000149 to 0.0001, meaning an offset of max 0.00005 degr = 5,5 m. 
    LonR <- round(bvs$Lon,digits=4) 
    LatR <- round(bvs$Lat,digits=4) 
}
findDuplicates <- function(bvs){
    LonR <- round(bvs$Lon) 
    LatR <- round(bvs$Lat,digits=4) 
    mat <- data.frame(LonR, LatR)
    dup <- duplicated(mat)
    return(dup)
}

# with sapply
checkCoordinates5 <- function(incidentRow,e){
    prevH <- get0("prevHouse",envir=e)
    incidentLon <- as.numeric(incidentRow["Lon"])
    incidentLat <- as.numeric(incidentRow["Lat"])   
    if(!(isInDaHouse(incidentLon,incidentLat,prevH["lon"],prevH["lat"]))){
        # New House, append it to houses
        newHouseNo <- as.numeric(prevH["houseNo"]) + 1
        newHouse <- c(newHouseNo, incidentLon, incidentLat, 1)
        h <- get0("houses",envir=e)
        rbind(h,newHouse)
        prevHouse <<- newHouse
    } else {
        prevH$incidentCount <- as.numeric(prevH$incidentCount + 1)
        prevHouse <<- prevH
    }
    houses <<- h
}        

setHouses5 <- function(incidents) {
    incidents <- incidents[order(incidents$Lon, incidents$Lat), ]
    #set the first house
    houseNo <- 1
    incidentCount <- 0
    houses <- data.frame(houseNo,incidents$Lon[1],incidents$Lat[1],incidentCount)
    names(houses) <- c("houseNo","lon","lat","incidentCount")
    prevHouse <- houses[1,]
    #For each entry in incidents, check if it's coordinates match the last created. 
    #If yes, add one more incident to the number of incidents of that house.
    #If not, create a new house (because incidents entries are sorted)
    e <- environment()
    houses <- sapply(incidents,checkCoordinates5,e)
    return(houses)
    
}
## end 14.5.2016

loadData <- function(env){
    if(!is.null(bvs <- get0("bvs",envir=env))){
        message("No need to reload file.")
        return(bvs)
    } else {
        bv <- read.csv("detroit-blight-violations-s.csv", stringsAsFactors = FALSE)        
        # Should we add and examine ViolationStreetNumber, ViolationStreetName?
        bvs <- data.frame(bv$TicketID, bv$ViolationCode, bv$ViolationAddress)  
        bvh <- head(bv)
        assign("bvh",bvh,.GlobalEnv) 
        rm(bv)
        names(bvs) <- c("ID","Code","Address")
        return(bvs)
    }
}

# 22.5.2016
# For each row in coordinates, see if it matches any of the rows in houses

findCoord <- function(houses,LonV,LatV){
    coord <- data.frame(lon=LonV,lat=LatV)
    searchCoord <- function(houses,coord){
        apply(coord,1,findInHouses,houses)
    }
    findInHouses <- function(cRow, houses){
        lon <- cRow["lon"]
        lat <- cRow["lat"]
        lon = LonV[1]
        lat = LatV[1]
        isInDaHouse <- function(house,lon,lat){
            #     lon <- get0("lon",envir=parent.frame())
            #     lat <- get0("lat")
            if(lon > house["Lon"] & lon < (house["Lon"] + 0.0005) 
               & lat > house["Lat"] & lat < (house["Lat"] + 0.0005)){
                return(TRUE)
            } else {
                return(FALSE)
            }
        }  
        apply(houses,1,isInDaHouse,lon,lat)   
        
    }
    
}

# 11.6.2016
findInHouses <- function(cRow, houses){
    lon <- cRow["Lon"]
    lat <- cRow["Lat"]
    isInDaHouse <- function(house,lon,lat){
        #     lon <- get0("lon",envir=parent.frame())
        #     lat <- get0("lat")
        off <- 0.0005
        if(lon >= house["Lon"] & lon <= (house["Lon"] + off) 
           & lat >= house["Lat"] & lat <= (house["Lat"] + off)){
            return(TRUE)
        } else {
            return(FALSE)
        }
    }  
    # Build a boolean list that indicates if rows of houses match the coordinates
    matchInHouses <- apply(houses,1,isInDaHouse,lon,lat)   
    # Return the matching rows in houses
    return(unlist(houses[matchInHouses,]))
}

# Not finalised, not used
findInHouses2 <- function(lon,lat,houses){
    # We assume houses contains unique values, so only one hit should come 
    house <- houses[round(houses$Lon,6)== round(lon,6) & round(houses$Lat,2)==round(lat,2),]
    
    return(house)
}

bv <- read.csv("detroit-blight-violations.csv",stringsAsFactors = FALSE)
bv <- subset(bv,select=c("TicketID","ViolationStreetNumber","ViolationStreetName","MailingCity","ViolationAddress"))
bv$NewAd <- unlist(strsplit(bv$ViolationAddress,split="\n"))
bv$new <- grep(pattern="'('{1}.*')'{1}",x=bv$ViolationAddress,value=TRUE)


# 2.7.2016 
findInHouses <- function(row, houses) {
    rlat <- round(row$Lat,digits=4)
    rlon <- round(row$Lon,digits=4)    
    # coord <- c(round(row["Lat"],digits=4),round(row["Lon"],digits=4))
    res <- which((houses$Lat == rlat && houses$Lon == rlon))
    return(res)
    # tfVect <- match(houses[,1:2],coord)
    # return(houses[tfVect,])
}

hm <- data.frame(Lat=c(1,1,2), Lon=c(2,3,2),count=c(1,1,1), id=c(1,2,3))
d311m <- data.frame(Lat=c(1,1,1), Lon=c(2,3,2),count=c(1,1,2), id=c(1,2,3))

addToHouses <- function(df,houses) {
    # Compare df with houses. If the coordinates in df already exist in houses, increase house counter. If not, add a new house.
    rNewCoord <-
        cbind(round(df$Lat,digits = 4),round(df$Lon,digits = 4))
    rm(df)
    # First aggregate the df incidents
    agg <- aggregate(rNewCoord[,1],by = list(rNewCoord[,1],rNewCoord[,2]),length)
    names(agg) <- c("Lat","Lon","count")
    agg <- agg[order(agg$Lat,agg$Lon),]
    
    # Then compare with houses
    newNo <- 0
    newHouses <- data.frame(Lon=NULL, Lat=NULL, count=NULL, id=NULL)
    for (i in 1:nrow(agg)) {
        found <- FALSE
        
        for (j in 1:nrow(houses)) {
            if ((agg[i,1] == houses$Lat[j]) & (agg[i,2] == houses$Lon[j])) {
                # The coord of row i in agg match those of row j in houses
                houses$count[j] <- (houses$count[j] + agg$count[i])
                found <- TRUE
                # Stop looping houses for this agg row
                break
            } else if (houses$Lat[j] > agg$Lat[i]) {
                # houses are sorted ascending. We're past the point of finding.
                # The coordinates don't match, try next row of houses
                break
            }
            if (found == FALSE) {
                # No matching house was found, so add new one
                newNo <- newNo + 1
                newHouse <- c(agg$Lat[i], agg$Lon[i], agg$count[i], newNo)
                newHouses[newNo,] <- newHouse
            }
        }
    } #agg loop
    houses <- rbind(houses,newHouses)
    return(houses)
}

# coord <- matrix(unlist(coord, use.names=FALSE),ncol=2,byrow=TRUE) #This collapses the NAs
# coord <- as.array(coord)
#         dem["Lon"] <- as.numeric(coord[,2])
# dem["Lat"] <- as.numeric(coord[,1])

addToHouses2 <- function(df,houses) {
    # Create a coordinate summary of df. Append to houses and aggregate
    rNewCoord <-
        cbind(round(df$Lat,digits = 4),round(df$Lon,digits = 4))
    rm(df)
    # First aggregate the df incidents
    agg <- aggregate(rNewCoord[,1],by = list(rNewCoord[,1],rNewCoord[,2]),length)
    names(agg) <- c("Lat","Lon","count")
    agg <- agg[order(agg$Lat,agg$Lon),]
    agg$id <- row.names(agg)
    ha <- rbind(houses, agg)
    ha <- aggregate(ha$count, by=list(ha$Lat, ha$Lon), sum)
    ha$id <- row.names(ha)
    names(ha) <- c("Lat","Lon","count","id")
    return(ha)
}

aggrCoord <- function(df, precision){
    agg <- aggregate(x = df$Lat, 
                     by = list(round(df$Lat,digits = precision), round(df$Lon,digits = precision)), FUN=length)
    agg$id <- row.names(agg)
    names(agg) <- c("Lat","Lon","count","id")
    return(agg)
}

# This function aggregates the incidents to a grid of squares of the given precision, e.g. 0.001 deg a side.
# Input df must have columns Lat, Lon, Type, ...
# Result: df with Lon, Lat, (prefix)Type, (prefix)count
# Under construction!
aggrCoord2 <- function(df, prefix, precision){
    agg <- aggregate(x = df$Lat, 
                     by = list(round(df$Lat,digits = precision), round(df$Lon,digits = precision)), df[,3], FUN=length)
    agg$id <- row.names(agg)
    names(agg) <- c("Lat","Lon","count","id")
    return(agg)
}
 
# Version of addToHouses based on the idea that we'll add new columns for each df (BViol, D311, crime etc.)
# 20.7.2016
addToHouses <- function(df,houses,inc) {
    # Add incident summary from df as new columns to the existing set of houses (and incident types).
    # df must have columns Lat, Lon, Type
    rNewCoord <-
        data.frame(Lat=round(df$Lat,digits = 4), Lon=round(df$Lon,digits = 4), Type=df$Type)
    agg <- aggregate(rNewCoord$Lat, by=list(rNewCoord$Lat, rNewCoord$Lon, rNewCoord$Type ),length)    
    incTypeName <- paste0(inc,"Type")
    incTypeCount <- paste0(inc,"Count")
    n <- c("Lat", "Lon", incTypeName, incTypeCount)
    names(agg) <- n
    agg$Lat <- as.numeric(agg$Lat )
    agg$Lon <- as.numeric(agg$Lon )    
    # First aggregate the df incidents
    agg <- agg[order(agg$Lat,agg$Lon),]
    # agg$id <- row.names(agg)
    c1 <- length(houses) + 1
    c2 <- c1 + 1
    nh <- c(names(houses), incTypeName, incTypeCount)
    houses[,c1] <- ""
    houses[,c2] <- 0
    names(houses) <- nh
    lastRowNr <- nrow(houses)
    lastOldHouse <- houses[lastRowNr,]
    oldColumns <- names(houses)
    oldColumnNr <- ncol(houses)
    clearHouse <- lastOldHouse
    for(colIdx in 1:(oldColumnNr)){
        clearHouse[,colIdx]=""
    }
    
    for(rnum in 1:nrow(agg)){
        houseNr <- findInHouses(agg[rnum,], houses)
        if(houseNr>0){
            # Add to existing house. 
            if(houses[houseNr,c1]==as.character(agg[rnum,incTypeName])){
                # If incidents of this type have already been registered, increase the count.
                houses[houseNr,c2] <- ( houses[houseNr,c2] + agg[rnum,incTypeCount] )
            } else {
                # Otherwise add a new incident type to the house -> a new row, but same Lat, Lon, HouseId
                newHouse <- houses[houseNr,]
                newRowNr <- lastRowNr + 1
                newHouse[incTypeName] <- as.character(agg[rnum,incTypeName])
                newHouse[incTypeCount] <- agg[rnum,incTypeCount]
                houses[newRowNr,] <- newHouse
            }
        } else {
            # Using the last old house, create a new house adding the new columns
            newHouse <- clearHouse
            newRowNr <- lastRowNr + 1
            newHouse$HouseId <- newRowNr
            newHouse$Lat <- agg$Lat[rnum]
            newHouse$Lon <- agg$Lon[rnum]
            newHouse[incTypeName] <- as.character(agg[rnum,incTypeName])
            newHouse[incTypeCount] <- agg[rnum,incTypeCount]
            houses[newRowNr,] <- newHouse
        }        
    }
}

buildHousesFromBv <- function(df, precision){
    agg <- aggregate(x = df$Lat, 
                     by = list(round(df$Lat,digits = precision), round(df$Lon,digits = precision), df$VCode), FUN=length)
    agg <- cbind(row.names(agg), agg)
    names(agg) <- c("HouseId","Lat","Lon", "Type", "Count")
    return(agg)
}


addIncidentsFromDf <- function(df, houses, precision){
    # df must have columns Lat, Lon, IncType
    agg <- aggregate(x = df$Lat, 
                     by = list(round(df$Lat,digits = precision), round(df$Lon,digits = precision), df$IncType), FUN=length)
    if(is.null(houses)){
        nh <- 0
    } else {
        nh <- nrow(houses)    
    }
    hId <- ((nh+1):(nh+nrow(agg)))
    agg <- cbind(hId,agg)
    names(agg) <- c("HouseId","Lat","Lon", "Type", "Count")
    houses <- rbind(houses, agg)
    return(houses)
}


# --------- UNUSED


findDefaultCoordinates <- function(df,precision) {
    #Here, we try to find observations in a df with coordinates that are same to a level of precision
    agg <-
        aggregate(x = df$ID, by = list(
            round(df$Lon,digits = precision),round(df$Lat,digits = precision)
        ),length)
    agg <-
        agg[agg$x > 2,] #If the number of x is high, we probably have a set of default coordinates
    # sprintf("%.8f",agg[,1])
    return(agg)
}

# get unique violation codes for examination
#     un <- unique(bv$ViolationCode,use.names = FALSE)
#     write.csv(file = "violcodes.csv",x = un, row.names = FALSE)

# violations <- data.frame(bv$ViolationCode,bv$ViolDescription,bv$Disposition)
# write.csv(file="violations.csv",x=violations,row.names=FALSE)
# assign("violations",violations,.GlobalEnv)

matchHouses <- function(row,houses){
    nr <- nrow(houses)
    idx <- findInHouses(row,houses)
    if(idx > 0){
        houses$count = houses$count + row$count
    } else {
        houses
    }
}


buildHousesFromBv <- function(df, precision){
    agg <- aggregate(x = df$Lat, 
                     by = list(round(df$Lat,digits = precision), round(df$Lon,digits = precision), df$VCode), FUN=length)
    agg <- cbind(row.names(agg), agg)
    names(agg) <- c("HouseId","Lat","Lon", "Type", "Count")
    return(agg)
}

# 26.9.2016


# Return the id of the house whose coordinates match those of the given row
findInHouses <- function(row, houses) {
    rlat <- round(as.numeric(row["Lat"]),digits=4)
    rlon <- round(as.numeric(row["Lon"]),digits=4)    
    rowMatch <- ((rlat == houses$Lat) & (rlon == houses$Lon))
    if(sum(rowMatch)< 1){
        return(0)
        # row$house <- 0
    } else{
        matchIdx <- which(rowMatch)
        if(length((matchIdx))>1){
            # House is not unique. SHould not happen!
            return(-1)
        } else {
            return(matchIdx)
        }
    }
}

# Would this perform better?
findInHouses2 <- function(row, houses) {
    rlat <- round(as.numeric(row["Lat"]),digits=4)
    rlon <- round(as.numeric(row["Lon"]),digits=4)    
    res <- which(houses$Lat == rlat & houses$Lon == rlon)
    houses[res,]$HouseId
    # coord <- c(round(row["Lat"],digits=4),round(row["Lon"],digits=4))    
    # tfVect <- match(coord, houses[,1:2])
    # OR
    # tfVect <- coord %in% houses[,1:2]
    # return(houses[tfVect,])
}

#nbInd <- sample(1:nrow(houses), size=nrow(sgh), replace=FALSE)
#nbTrainInd <- sample(nbInd, size=length(nbInd), replace=FALSE)
#nbTestInd <- nbInd[!(nbInd %in% nbTrainInd)]
#nbHousesTrain <- houses[nbTrainInd,]
#nbHousesTest <- houses[nbTestInd,]


buildHousesFromDf <- function(df, houses, precision){
    # We'll aggregate all incidents from any df to a set of houses (coordinates) irrespective of incident type.
    # House structure: (HouseId, Lat, Lon). Lat and Lon represent the center of the house, which is about 11m x 11m.
    hCoord <- data.frame(Lat=houses$Lat, Lon=houses$Lon)
    dCoord <- data.frame(Lat=round(df$Lat,digits = precision), Lon=round(df$Lon,digits = precision))
    # Remove NA values
    naVector <- is.na(dCoord$Lat+dCoord$Lon)
    dCoord <- dCoord[!naVector,]
    coord <- rbind(hCoord,dCoord)
    rm(df,hCoord,dCoord)
    # Remove added duplicates
    coord <- coord[!duplicated(coord),]
    houses <- coord
    rm(coord)
    return(houses)
}


buildHouses <- function(){
    # Load each file and allocate incident coordinates to houses
    bv <- getBViol()
    bv <- formatBv(bv)
    houses <- NULL
    houses <- buildHousesFromDf(bv, houses, 4)
    rm(bv)
    d311 <- getD311()
    houses <- buildHousesFromDf(d311, houses, 4)
    rm(d311)
    cr <- getCrime()
    houses <- buildHousesFromDf(cr, houses, 4)
    houses <- houses[order(houses$Lat, houses$Lon),]
    row.names(houses) <- 1:nrow(houses)
    houses <- cbind(row.names(houses), houses)
    names(houses) <- c("HouseId", "Lat", "Lon")
    houses$HouseId <- as.character(houses$HouseId)
    houses <- data.table(houses)
    setkey(houses,Lat,Lon)
    return(houses)
}


addIncidentsFromDf <- function(df, inc, precision){
    # df must have columns Lat, Lon, IncType
    newRows <- cbind(df$Lat, df$Lon, 1)
    colnames(newRows) <- c("Lat", "Lon", "Count")
    inc <- rbind(inc, newRows)
    agg <- aggregate(x = inc$Count, by = list(round(inc$Lat,digits = precision), round(inc$Lon,digits = precision)), FUN=sum)
    #     agg <- aggregate(x = df$Lat, 
    #                      # by = list(round(df$Lat,digits = precision), round(df$Lon,digits = precision), df$IncType), FUN=length)
    #                      by = list(round(df$Lat,digits = precision), round(df$Lon,digits = precision)), FUN=length)
    # names(agg) <- c("Lat","Lon", "Type", "Count")
    names(agg) <- c("Lat","Lon", "Count")
    # inc <- rbind(inc, agg)
    return(agg)
}


sumIncidents <- function(){
    bv <- getBViol()
    bv <- formatBv(bv)
    inc <- data.frame(Lat=NULL, Lon=NULL, IncType=NULL)
    inc <- addIncidentsFromDf(bv, inc, 4)    
    d311 <- getD311()
    inc <- addIncidentsFromDf(d311, inc, 4)
    cr <- getCrime()
    inc <- addIncidentsFromDf(cr, inc, 4)
    inc <- inc[order(inc$Lat, inc$Lon),]
    row.names(inc) <- 1:nrow(inc)
    return(inc)
}

getD311 <- function() {
    d311 <- loadDf("detroit-311.csv")
    # We'll ignore some alerts, as they are not specific to a house
    # nor caused by the house owner
    ignore <-
        c(
            "Water Main Break", "Fire Hydrant Issue", "Traffic Signal Issue",
            "Potholes", "Test (internal use only, public issue)",
            "Customer Service (internal use only, private issue)",
            "Graffiti Abatement (internal use only, public issue)"
        )
    # d311 <- d311[!(d311$issue_type %in% ignore),]
    #d311 <- subset(d311,d311$Lat<42.3)
    u311type <- unique(d311$issue_type,use.names = FALSE)
    d311 <- subset(
        d311,!(d311$issue_type %in% ignore),
        select = c(
            issue_type,ticket_closed_date_time,ticket_created_date_time,
            address,lat,lng
        )
    )
    # assign("u311type",u311type,envir = .GlobalEnv)
    n311 <- names(d311)
    n311[1] <- "IncType"
    n311[5] <- "Lat"
    n311[6] <- "Lon"
    names(d311) <- n311
    d311$Lat <- as.numeric(d311$Lat)
    d311$Lon <- as.numeric(d311$Lon)
    return(d311)
}

buildHouses2 <- function(precision){
    bv <- getBViol()
    bv <- formatBv(bv)
    inc <- data.frame()
    inc <- rbind(inc, formatForInc(bv))
    rm(bv)
    d311 <- getD311()
    inc <- rbind(inc, formatForInc(d311))
    rm(d311)
    cr <- getCrime()
    inc <- rbind(inc, formatForInc(cr))
    rm(cr)
    houses <- aggregate(x=inc$Lat, by=list(round(as.numeric(inc$Lat),digits=precision), round(as.numeric(inc$Lon), digits = precision)), FUN=length)
    rm(inc)
    houses <- data.table(houses)
    colnames(houses) <- c("Lat", "Lon", "Count")
    setkey(houses, Lat, Lon)
    houses <- cbind(row.names(houses),houses)
    colnames(houses) <- c("HouseId", "Lat", "Lon", "Count")
    return(houses)
}

housesInDetroitSnip <- function(){
    #crs_geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")  # geographical, datum WGS84
    #proj4string(detroit) <- crs.geo 
    
}
    

demolitionHouses <- function(dem, houses){
        # Replaced the below with a simple inner join!
        d <- apply(dem, 1, findInHouses3,houses)
        dem <- cbind(d, dem)
        names(dem)[names(dem)=="d"] <- "house"
        dem
}

findInHouses3 <- function(row, houses) {
    # houses must be a data.table. Returns HouseId (an integer) or NA
    rlat <- round(as.numeric(row["Lat"]),digits=4)
    rlon <- round(as.numeric(row["Lon"]),digits=4)
    res <- houses[list(rlat,rlon)]
    res$HouseId
}

getAndSaveData <- function(){
    # WORK IN PROGRESS: this function builds and filters the houses, demolition permits and test data, and saves to files for later use
    houses <- buildHouses3(precision = 4)
    houses <- housesInDetroit(houses)
    names(houses) <- c("HouseId","Count","Lat","Lon")
    writeData(data = houses, filename = "housesWithinDetroit.csv")
    assign("houses",houses,.GlobalEnv)
    dem <- prepareDemolition()
    dh <- demolitionHouses(dem,houses)
    assign("demolitionhouses",dh,.GlobalEnv)
    write.csv(file = "dem-houses.csv",x = dh,row.names = FALSE)
}


incPerTtHouse <- function(tset){
    bvc <- loadDf("bvcounts.csv") #Number of blight violations in each house
    bvc <- data.table(bvc)
    bvc <- setkey(bvc, Lat, Lon)
    
    findCount <- function(tsetrow){
        # bvcrow <- bvc[.(tsetrow["Lat"],tsetrow["Lon"])]
        lat <- tsetrow["Lat"]
        lon <- tsetrow["Lon"]
        bvcrow <- bvc[.(lat,lon)]
        if(nrow(bvcrow)>1){
            print("Error!!")
        }
        count <- bvcrow$Count
        return(count)
    }
    cnt <- unlist(apply(tset,1,findCount))
    tset <- cbind(tset,cnt)
}


#31.12.2016 - for simple model
formatBv <- function(bv) {
    bv <- subset(
        bv,!(is.null("ViolationCode")),select = c(
            "ViolationCode", "ViolDescription",
            "ViolationAddress","ViolationStreetNumber", "ViolationStreetName", "TicketIssuedDT"
        )
    )
    extractCoord <- function(bvsRow) {
        # bvsRow is like ..., 2566 GRAND BLVD\nDetroit, MI\n(42.36318237000006, -83.09167672099994)
        rowStr <- unlist(strsplit(toString(bvsRow),split = "\n"))[3]
        # Now we have something like (42.32544917300004, -83.06413908999997)
        coord <- gsub("[()]","",rowStr)
        coord <- unlist(strsplit(coord,split = ","))
        coord <- c(coord[1], coord[2])
        return(coord)
    }
    coord <- t(apply(bv,1,extractCoord))
    # Add latitude and longitude as first columns
    bv <- cbind(as.numeric(coord[,1]),as.numeric(coord[,2]),bv)
    names(bv) <-
        c("Lat","Lon","IncType","ViolDescription", "VAddress","VStrNr","VStrName","TicketIssueDT")
    return(bv)
} # formatBv

formatBvAddress <- function(bv){
    # Check column names before calling this function!
    # bv$ViolationAddress <- gsub("\n",",",bv$ViolationAddress)
    bv$ViolationAddress <- gsub("\n",",",bv$VAddress)
    return(bv)
}

main <- function(){
    library(data.table)
    setDirs()
    if(is.null(get0("houses"))){ 
        # We need to load houses from file or build them again
        houses <- tryCatch(loadDt("housesWithinDetroit.csv"), 
                           error = function(e){
                               # File not found, need to rebuild from source data
                               houses <- buildHouses3(precision = 4)
                               houses <- overDetroit(houses)
                               names(houses) <- c("HouseId","Count","Lat","Lon")
                               writeData(data = houses, filename = "housesWithinDetroit.csv")
                               assign("houses",houses,.GlobalEnv)
                               return(houses)
                           } 
        )
    }
    #Get demolition permits, geocode where necessary, clear outliers and assign to houses
    dh <- tryCatch(loadDt("dem-houses.csv"),
                   # stop("Could not load houses with demolition!")
                   error = function(e){
                       # File not found, need to rebuild from source data
                       dem <- prepareDemolition()
                       dh <- demolitionHouses(dem,houses)
                       assign("demolitionhouses",dh,.GlobalEnv)
                       write.csv(file = "dem-houses.csv",x = dh,row.names = FALSE)
                       return(dh)
                   } 
    )
    setkey(dh,HouseId)
    prepareTtSet(dh,houses)
    #     # Next go through the files again and build aggregates by location and incident type 
    #     inc <- sumIncidents()
    #     # Next see in which house each group of incidents took place, 
    #     # so finally we have a df of incident grouped by location (house number, coordinates, incident type)
    #     # Col hinc is the house number The first column (row name) has no meaning.
    #     hinc <- apply(inc,1,findInHouses3,houses)
    #     inci <- cbind(hinc,inc)
    #     inci <- inci[order(inci$hinc),]
    #     write.csv(file="incidents_aggr.csv", x=inci)
    
} #main