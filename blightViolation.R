# Program idea: fit incident data to data frame. Select only relevant columns, and always use same names for them. 
# From incident data, derive unique locations of a given area = houses. House data: HouseId, Lat, Lon.
# Then aggregate incidents by type to houses.

loadDf <- function(fileName) {
    basedir <-
        "C:/Users/setup/Documents/coursera/uw/datascience/capstone"
    datadir <- paste0(basedir, "/data")
    codedir <- paste0(basedir, "/code")
    setwd(datadir)
    fullName <- paste0(datadir, "/", fileName)
    df <- read.csv(fullName, stringsAsFactors = FALSE)
    return(df)
}

loadDt <- function(fileName){
    d <- loadDf(fileName)
    d <- data.table(d)
    return(d)
}

getBViol <- function(){
    bv <- loadDf("detroit-blight-violations.csv")
    return(bv)
}

formatBv <- function(bv) {
    bv <- subset(
        bv,select = c(
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
        return(coord)
    }
    coord <- t(apply(bv,1,extractCoord))
    # Add latitude and longitude as first columns
    bv <- cbind(as.numeric(coord[,1]),as.numeric(coord[,2]),bv)
    names(bv) <-
        c("Lat","Lon","IncType","ViolDescription", "VAddress","VStrNr","VStrName")
    bv <- bv[,1:7]
} # formatBv

formatBvAddress <- function(bv){
    # Check column names before calling this function!
    # bv$ViolationAddress <- gsub("\n",",",bv$ViolationAddress)
    bv$ViolationAddress <- gsub("\n",",",bv$VAddress)
    return(bv)
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

getCrime <- function(){
    cr <- loadDf("detroit-crime.csv")
    cr <- subset(cr,select=c("CATEGORY", "LAT", "LON"))
    names(cr) <- c("IncType", "Lat", "Lon")
    return(cr)
}


getDemolition <- function(){
        # if(is.null(dem)){
            dem <- read.csv(file=
                                "C:/Users/setup/Documents/coursera/uw/datascience/capstone/data/detroit-demolition-permits.tsv",
                            sep = "\t", stringsAsFactors = FALSE)
        # }
#         dem <- subset(dem, select=c(PERMIT_ISSUED, CASE_TYPE, BLD_PERMIT_TYPE, site_location))
        return(dem)
}

formatDemolition <- function(dem){
        extractCoordAdd <- function(row){
            loc <- row["site_location"]
            # loc is like "4331 BARHAM\nDetroit, MI\n(42.394106, -82.9474)"
            loc <- unlist(strsplit(toString(loc),split = "\n"))
            str <- loc[1]
            cty <- loc[2]
            crd <- loc[3]
            # Now we have something like (42.32544917300004, -83.06413908999997)
            crd <- gsub("[()]","",crd)
            crd <- unlist(strsplit(crd,split = ","))
            lat <- crd[1]
            lon <- crd[2]
            coord <- c(lat, lon, str, cty)
            return(coord)
        }
        dnam <- names(dem)
        coord <- t(apply(dem, 1, extractCoordAdd))
        dem <- cbind(as.numeric(coord[,1]), as.numeric(coord[,2]), coord[,3], coord[,4], dem)
        dnam <- c("Lat", "Lon", "Street", "City", dnam)
        names(dem) <- dnam
        dem$Street <- as.character(dem$Street)
        dem$City <- as.character(dem$City)
        return(dem)
}


formatForInc <- function(df){
    inc <- data.frame(Lat = df$Lat, Lon=df$Lon) #possibly add IncType
    return(inc)
    
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


prepareDemolition <- function(){
    dem <- getDemolition()
    dem <- formatDemolition(dem)
    dem <- data.table(dem)
    # Some 900 entries have NA for coordinates. We need to treat them separately with geocoding.
    demToGeocode <- dem[is.na(Lat)]
    # demToGeocode <- dem[is.na(dem$Lat),]
    # demToGeocode <- head(demToGeocode,10)
    # Remove from dem the entries that need to be geocoded 
    dem <- dem[!(is.na(Lat))]
    
    addresses <- paste0(demToGeocode$Street, " ",gsub(",", "",demToGeocode$City))
    geocoded <- data.frame()
    # Start the geocoding process - address by address. geocode() function takes care of query speed limit.
    for (ii in (1:length(addresses))){
        if (ii %% 50 == 0){
            print(paste("Working on index", ii, "of", length(addresses)))    
        }
        #query the google geocoder - this will pause here if we are over the limit.
        result = geoCodeAddress(addresses[ii]) 
        # print(result$status)     
        result$index <- ii
        #append the answer to the results file.
        geocoded <- rbind(geocoded, result)
        #save temporary results as we are going along
        # saveRDS(geocoded, tempfilename)
    }
    # demToGeocode$accuracy <- geocoded$accuracy
    demToGeocode$Lat <- geocoded$Lat
    demToGeocode$Lon <- geocoded$Lon
    # Remove geographic outliers
    demToGeocode <- demToGeocode[Lat %between% c(42.24,42.5) & Lon %between% c(-83.3,-82.9)]
    # Finally add the geocoded entries to dem
    dem <- rbind(dem,demToGeocode)
    return(dem)
}

geoCodeAddress <- function(address){
    geo_reply = geocode(address, output='all', messaging=TRUE, override_limit=TRUE)
    #now extract the bits that we need from the returned list
    answer <- data.frame(Lat=NA, Lon=NA, accuracy=NA, formatted_address=NA, address_type=NA, status=NA)
    answer$status <- geo_reply$status
    #if we are over the query limit - want to pause for an hour
    while(geo_reply$status == "OVER_QUERY_LIMIT"){
        print("OVER QUERY LIMIT - Pausing for 1 hour at:") 
        time <- Sys.time()
        print(as.character(time))
        Sys.sleep(60*60)
        geo_reply = geocode(address, output='all', messaging=TRUE, override_limit=TRUE)
        answer$status <- geo_reply$status
    }
    if (geo_reply$status != "OK"){
        return(answer)
    }   
    #else, extract what we need from the Google server reply into a dataframe:
    answer$Lat <- geo_reply$results[[1]]$geometry$location$lat
    answer$Lon <- geo_reply$results[[1]]$geometry$location$lng   
    if (length(geo_reply$results[[1]]$types) > 0){
        answer$accuracy <- geo_reply$results[[1]]$types[[1]]
    }
    answer$address_type <- paste(geo_reply$results[[1]]$types, collapse=',')
    answer$formatted_address <- geo_reply$results[[1]]$formatted_address
    return(answer)
}

demolitionHouses <- function(dem, houses){
    # h <- data.table(houses)
    # setkey(h,Lat,Lon)
    # Link demolition permits to houses
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

prepareTtSet <- function(dh, houses){
    dh <- dh[!is.na(house)]
    dh <- subset(dh,select=c("house","Lat","Lon","PERMIT_ISSUED","LEGAL_USE","PARCEL_SIZE","STORIES"))
    dh <- dh[!duplicated(dh$house)]
    dh$Lat <- round(dh$Lat,digits=4)
    dh$Lon <- round(dh$Lon,digits=4)
    n <-nrow(dh)
    bHouses <- data.table(HouseId = dh$house, Lat = dh$Lat, Lon=dh$Lon, ToDemolition = TRUE)
    nbHouses <- houses[!(HouseId %in% dh$house)]
    nbHouses <- data.table(HouseId = as.integer(nbHouses$HouseId), Lat=nbHouses$Lat, Lon=nbHouses$Lon, ToDemolition = FALSE)
    nbHouses <- nbHouses[sample(1:nrow(nbHouses), size=nrow(bHouses), replace=FALSE),]
    # as result, we have nrow(bHouses) = nrow(nbHouses) = 2483
    print(paste("Number of blighted houses: ", nrow(bHouses)))
    print(paste("Number of non-blighted houses: ", nrow(nbHouses)))
    ttSet <- rbind(bHouses, nbHouses)
    # Ordering these by HouseId will mix the b and nb houses
    ttSet <- ttSet[order(ttSet$HouseId),]
    n <- nrow(ttSet)
    trainInd <- sample(1:n, size=round(0.7*n),replace=FALSE)
    trainSet <- ttSet[trainInd,]
    testSet <- ttSet[-trainInd,]
    write.csv(file="trainSet.csv", x=trainSet, row.names = FALSE)
    write.csv(file="testSet.csv", x=testSet, row.names = FALSE)
    assign("trainSet",trainSet,.GlobalEnv)
    assign("testSet",testSet,.GlobalEnv)
}

incPerTtHouse <- function(tset){
    bvc <- loadDf("bvcounts.csv")
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

main <- function(){
    library(data.table)
    if(is.null(get0("houses"))){ 
        houses <- loadDt("housesWithinDetroit.csv") #reuse the cleaned set from Carto
        setkey(houses,Lat,Lon)
#         #build only if needed
#         houses <- buildHouses2()
#         write.csv(file = "houses.csv",x = houses,row.names = TRUE)
        assign("houses",houses,.GlobalEnv)
    }
    
    #Get demolition permits, geocode where necessary, clear outliers and assign to houses
    # -- use the files already created instead of building them with the code below 
    dh <- loadDt("houses_with_demolition.csv")
    setkey(dh,house)
    # library(ggmap)
    # dem <- prepareDemolition()
    # dh <- demolitionHouses(dem,houses)
    # assign("demolitionhouses",dh,.GlobalEnv)
    # write.csv(file = "dem-houses.csv",x = dh,row.names = FALSE)
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


#Function to build a list of unique bv violation codes and description. ViolName is the violator's name!
violCodes <- function(bv){
    uc <- unique(bv$ViolationCode)
    ud <- unique(bv$ViolDescription)
    av <- aggregate(x=bv$ViolationCode, by=list(bv$ViolationCode,bv$ViolDescription), FUN=length )
    av <- av[order(av[,1]),]
    colnames(av) <- c("ViolationCode", "ViolDescription", "count")
    write.csv(file="uniqueViolCodes.csv", x=av, row.names = FALSE)
    return(av)
}



# # Sample code for reading URL
# url <- "https://raw.githubusercontent.com/gastonstat/CreditScoring/master/CleanCreditScoring.csv"
# cs_data <- getURL(url)
# cs_data <- read.csv(textConnection(cs_data))
# describe(cs_data)

findCount <- function(tsrow){
    res <- houses[list(tsrow["Lat"], tsrow["Lon"]),nomatch=0]
    res
}

isInDetroit <- function(bvrow){
    res <- bvc[.(lat,lon)]
}
