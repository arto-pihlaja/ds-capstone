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
