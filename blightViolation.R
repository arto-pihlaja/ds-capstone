# Program idea: fit incident data to data frame. Select only relevant columns, and always use same names. 
# For starters: Lat, Lon, Type.
# Bundle incidents to houses. House data: Lat, Lon, IncidentType1, IncidentType1Count, IncidentType2, IncidentType2Count, ...

loadDf <- function(fileName) {
    basedir <-
        "C:/Users/setup/Documents/coursera/uw/datascience/capstone"
    datadir <- paste0(basedir, "/data")
    codedir <- paste0(basedir, "/code")
    fullName <- paste0(datadir, "/", fileName)
    df <- read.csv(fullName, stringsAsFactors = FALSE)
    return(df)
}

getBViol <- function(){
    bv <- loadDf("detroit-blight-violations.csv")
    return(bv)
}

formatBv <- function(bv) {
    bv <- subset(
        bv,select = c(
            "TicketID","ViolationCode",
            "ViolationAddress","ViolationStreetNumber",
            "ViolationStreetName"
        )
    )
    extractCoord <- function(bvsRow) {
        # bvsRow is like 26288 2566 GRAND BLVD\nDetroit, MI\n(42.36318237000006, -83.09167672099994)
        rowStr <- unlist(strsplit(toString(bvsRow),split = "\n"))[3]
        # Now we have something like (42.32544917300004, -83.06413908999997)
        coord <- gsub("[()]","",rowStr)
        coord <- unlist(strsplit(coord,split = ","))
        return(coord)
    }
    coord <- t(apply(bv,1,extractCoord))
    # Add latitude and longitude as first columns
    
#     bv["Lat"] <- as.numeric(coord[,1])
#     bv["Lon"] <- as.numeric(coord[,2])
    bv <- cbind(as.numeric(coord[,1]),as.numeric(coord[,2]),bv)
    names(bv) <-
        c("Lat","Lon", "TicketId","VCode","VAddress","VStrNr","VStrName")
    bv <- bv[,1:7]
} # formatBv

buildHousesFromBv <- function(df, precision){
    agg <- aggregate(x = df$Lat, 
                     by = list(round(df$Lat,digits = precision), round(df$Lon,digits = precision), df$VCode), FUN=length)
    agg$id <- row.names(agg)
    names(agg) <- c("Lat","Lon", "VCode", "Count","HouseId")
    return(agg)
}

get311 <- function() {
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
    n311[5] <- "Lat"
    n311[6] <- "Lon"
    names(d311) <- n311
    return(d311)
}

getCrime <- function(){
    cr <- loadDf("detroit-crime.csv")
    cr <- subset(cr,select=c("CATEGORY", "LAT", "LON"))
    names(cr) <- c("Category", "Lat", "Lon")
    return(cr)
}

getDemolition <- function(){
        # if(is.null(dem)){
            dem <- read.csv(file=
                                "C:/Users/setup/Documents/coursera/uw/datascience/capstone/data/detroit-demolition-permits.tsv",
                            sep = "\t", stringsAsFactors = FALSE)
        # }
        dem <- subset(dem, select=c(PERMIT_ISSUED, CASE_TYPE, BLD_PERMIT_TYPE, site_location))
        return(dem)
}

formatDemolition <- function(dem){
        extractCoord <- function(row){
            crd <- row["site_location"]
            # crd is like "4331 BARHAM\nDetroit, MI\n(42.394106, -82.9474)"
            crd <- unlist(strsplit(toString(crd),split = "\n"))[3]
            # crd <- crd[3]
            # Now we have something like (42.32544917300004, -83.06413908999997)
            crd <- gsub("[()]","",crd)
            crd <- unlist(strsplit(crd,split = ","))
            return(crd)
        }
        coord <- t(apply(dem, 1, extractCoord))
        # Template for alternative: vapply(x, FUN = length, FUN.VALUE = 0L)
        dem$Lon <- NULL
        dem$Lat <- NULL
        for(i in 1:length(coord)){
            dem$Lat[i] <- as.numeric(coord[[i]][1]) 
            dem$Lon[i] <- as.numeric(coord[[i]][2])
        }
        return(dem)
}

# Return the id of the house whose coordinates match those of the given row
findInHouses <- function(row, houses) {
        rlat <- round(row$Lat,digits=4)
        rlon <- round(row$Lon,digits=4)    
        # Make a vector and a matrix
        coord <- c(rlat, rlon)
        mh <- cbind(houses$Lat, houses$Lon)
        rowMatch <- apply(mh,1,identical,coord)
        if(sum(rowMatch)< 1){
            return(0)
        } else{
        matchIdx <- which(rowMatch)
        return(matchIdx)
        }
}

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
                # Otherwise add a new incident type to the house -> a new row based on the existing house
                newHouse <- houses[houseNr,]
                newRowNr <- lastRowNr + 1
                houses[houseNr,c1] <- as.character(agg[rnum,incTypeName])
                houses[houseNr,c2] <- agg[rnum,incTypeCount]
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

main <- function(){
    bv <- getBViol()
    bv <- formatBv(bv)
    houses <- dedupWithRound(bv)
    houses$id <- row.names(houses)
    write.csv(file = "houses.csv",x = houses,row.names = TRUE)
    assign("houses",houses,.GlobalEnv)
    d311 <- get311()
    houses <- addToHouses(d311,houses)
    rm(d311)
    cr <- getCrime()
    houses <- addToHouses(cr,houses)
    rm(cr)
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
