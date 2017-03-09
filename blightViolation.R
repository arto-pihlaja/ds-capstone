# Program idea: fit incident data to data table. Select only relevant columns, and always use same names for them. 
# From incident data, derive unique locations of a given area = houses. House data: HouseId, Lat, Lon.
# Count number of incidents per house.
# Assign demolition permits to houses.
# Train simple model: demolition(yes/no) ~ incident count
setDirs <- function(){
    basedir <-
        "C:/Users/setup/Documents/coursera/uw/datascience/capstone"
    assign("datadir",(paste0(basedir, "/data")),.GlobalEnv)
    assign("codedir",(paste0(basedir, "/code")),.GlobalEnv)
    setwd(datadir)    
}

loadDf <- function(fileName) {
    setDirs()
    fullName <- paste0(datadir, "/", fileName)
    # tryCatch(
        df <- read.csv(fullName, stringsAsFactors = FALSE)
#         error = function(e) "File not found!"
#     )
    return(df)
}

loadDt <- function(fileName){
    d <- loadDf(fileName)
    d <- data.table(d)
    return(d)
}

writeData <- function(data, filename) {
    setDirs()
    file <- paste0(datadir,"/",filename)
    write.csv(x = data, file = file, row.names = FALSE)
}

getBViol <- function(){
    bv <- loadDt("detroit-blight-violations.csv")
    # bv <- loadDt("detroit-blight-violations-s.csv")
    return(bv)
}

formatBv <- function(bv) {
    extractCoord <- function(bvsRow) {
        # "XAddress" is like ..., 2566 GRAND BLVD\nDetroit, MI\n(42.36318237000006, -83.09167672099994), ...
        # rowstr <- unlist(strsplit(toString(bvsRow),split = "\n"))
        vcoord <- unlist(strsplit(toString(bvsRow["ViolationAddress"]),split = "\n"))[3]
        # Now we have something like (42.32544917300004, -83.06413908999997)
        # vcoord <- gsub("[()]","",vcoord)
        # vcoord <- unlist(strsplit(vcoord,split = ","))
        mcoord <- unlist(strsplit(toString(bvsRow["MailingAddress"]),split = "\n"))[3]
        coord <- paste(vcoord,",", mcoord)
        coord <- gsub("[()]","",coord)
        coord <- unlist(strsplit(coord,split = ","))
        
        # coord <- vcoord
        return(coord)
    }
    coord <- t(apply(bv,1,extractCoord))
    # coord <- ldply(coord, data.frame)
    suppressWarnings( 
        coord <- tryCatch( data.frame(Reduce(rbind,coord), stringsAsFactors = FALSE),
        error = function(e) { } #Now what? Throw exception?
        )
    )
    # Add Violation and Mailing latitude and longitude as columns
    bv$Lat <- as.numeric(coord[,1])
    bv$Lon <- as.numeric(coord[,2])
    bv$MLat <- as.numeric(coord[,3])
    bv$MLon <- as.numeric(coord[,4])

    # Add a column that indicates if the violation and mail addresses are equal (a proxy for the owner living in the building)
    bv$ViolEqMail <- (bv$Lat == bv$MLat & bv$Lon == bv$MLon)
    # Add a column for engineered violation category (and turn bv into a data.table)
    bv <- categorizeBv(bv)
    setkey(bv,Lat,Lon,VCategory)
    bv <- subset(bv, select = c("VCategory", "ViolName", "ViolationCode", "Lat", "Lon", "ViolEqMail"))
    return(bv)
} # formatBv


getD311 <- function() {
    d311 <- loadDf("detroit-311.csv")
#    u311type <- unique(d311$issue_type,use.names = FALSE)
    d311 <- subset(
        d311,
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
    d311 <- data.table(d311)
    # Guessing which incident types could be indicators of blight and selecting only those
    d311 <- d311[IncType %in% c("Abandoned Vehicle", "Curbside Solid Waste Issue", "Illegal Dumping / Illegal Dump Sites",
                                "Running Water in a Home or Building", "Trash Issue - Improper placement of refuse container between collections/left at curbside")]
    return(d311)
}

getCrime <- function(){
    cr <- loadDt("detroit-crime.csv")
    cr <- subset(cr,select=c("CATEGORY", "LAT", "LON"))
    names(cr) <- c("IncType", "Lat", "Lon")
    return(cr)
}


buildHouses <- function(precision, bv){
    inc <- data.table(Lat = bv$Lat, Lon = bv$Lon, IncType = bv$VCategory, ViolEqMail = bv$ViolEqMail
                      )
    rm(bv)
    d311 <- getD311()
    d311$ViolEqMail <- "NA"
    d311 <- categorize311(d311)
    d311$IncType <- d311$d311Category
    d311 <- subset(d311, select=c("Lat", "Lon", "IncType", "ViolEqMail"))

    inc <- rbind(inc, d311)
    rm(d311)
# At a closer look, crime data doesn't seem like a good predictor for blight, so it's not used here to determine houses.

    # Remove clear geographic outliers. Coordinates from Detroit bounding box with a margin.
    inc <- inc[Lat %between% c(42.25,42.5) & Lon %between% c(-83.3,-82.9)]
    # Then against Detroit city boundaries
    inc <- overDetroit(inc)
    print(paste('Number of incidents in Detroit total: ', nrow(inc)))
    inc <- data.table(inc)
    inc$Lat <- round(as.numeric(inc$Lat),digits=precision)
    inc$Lon <- round(as.numeric(inc$Lon),digits=precision)
    setkey(inc, Lat, Lon, IncType)
    # The data table operator .N adds the number of incidents in the by group to every member of the group. 
    # We add it as a new column Count, which has the number of incidents per rounded location and type
    inc <- inc[,":=" (Count = .N), by= list(Lat, Lon, IncType)]
    # We need separate columns for each incident type
    inc <- addCountColumnsPerIncType(inc)
    inc <- unique(inc)
    inc <- inc[ , j=list(IncType, ViolEqMail, max(TrashIncCount), max(PermitIncCount), max(MaintenanceIncCount), max(VehicleIncCount), max(WaterIncCount)), by=list(Lat, Lon)]
    colnames(inc) <- c("Lat", "Lon", "IncType", "ViolEqMail", "Trash", "Permit", "Maintenance", "Vehicle", "Water")
    # Now we still need to remove duplicates (incidents with identical coordinates, when rounded to precision)(DT uses key fiels to determine uniqueness)
    setkey(inc, Lat, Lon)
    houses <- unique(inc)
    rm(inc)
    setkey(houses, Lat, Lon)
    # Houses are now sorted by ascending Lat, Lon. Add row number as unique HouseId
    houses <- cbind(row.names(houses),houses)
    cnh <- colnames(houses)
    cnh[1] <- "HouseId"
    colnames(houses) <- cnh
    st <- sum(houses$Trash) + sum(houses$Permit)+ sum(houses$Maintenance) + sum(houses$Vehicle) + sum(houses$Water)  
    print(paste('Total counts in summary table houses: ', st))
    return(houses)
}

overDetroit <- function(dt){
    # dt must be a data table with columsn Lon and Lat with no NAs
    library(sp)
    library(rgdal)
    coordinates(dt) <- ~Lon + Lat
    detroitShapeDir <- paste0(datadir, "/City Boundaries shapefile")
    detroit <- readOGR(dsn=detroitShapeDir, layer = "geo_export_941b9f41-e08a-40db-ae36-d77a7046e1a5")
    proj4string(dt) <- proj4string(detroit)
    dInD <- dt[detroit,] #shorthand for dt[!is.na(over(dt,detroit)),]
    dt <- tryCatch(
        cbind(dInD@coords[,2],dInD@coords[,1], dInD@data), # add Lat, Lon
        error = function(e){
            print("Could not find additional data in dt!")
            return(data.table(Lat=dInD@coords[,2],Lon=dInD@coords[,1]))
        }
    )
    colnames(dt)[1:2] <- c("Lat", "Lon")
    dt <- data.table(dt)
    return(dt)
}

getDemolition <- function(){
    # if(is.null(dem)){
    dem <- read.csv(file=
                        "C:/Users/setup/Documents/coursera/uw/datascience/capstone/data/detroit-demolition-permits.tsv",
                    sep = "\t", stringsAsFactors = FALSE)
    # }
    return(dem)
}

formatDemolition <- function(dem){
    library(stringr)
    siteLoc <- strsplit(dem$site_location, split = "\n")
    siteLocLen <- unlist(Map(length,siteLoc))
    
    # Get the coordinates
    crd <- str_extract_all(dem$site_location, "[(].+[)]", simplify = TRUE) # we get (42.nnnn, -83.nnn)
    crd <- gsub("[()]","",crd)
    crd <- strsplit(crd,split = ",")
    # strsplit returns a list. Now need to replace missing coordinates with NA before Reducing list to matrix,
    # or else the rows won't match.
    crd <- Map(function(crdlistitem){ if(length(crdlistitem==2)) return(crdlistitem) else return(NA) }, crd)
    crd <- Reduce(rbind,crd)
    
    # Get the address
    extractDemAddr <- function(row){
        l <- length(row)
        if(l<1) return(c(NA,NA))
        else if(l==1) return(c(row[1],NA))
        else return(c(row[1],row[2]))
    }
    add <- Map(extractDemAddr,siteLoc)
    add <- t(as.data.frame(x=add, stringsAsFactors=FALSE ))
    dnam <- c("Lat", "Lon", "Street", "City", names(dem))
    dem <- cbind(as.numeric(crd[,1]), as.numeric(crd[,2]), add[,1], add[,2], dem)
    names(dem) <- dnam
    dem$site_location <- gsub("\n"," ",dem$site_location)
    dem$owner_location <- gsub("\n"," ",dem$owner_location)
    dem$contractor_location <- gsub("\n"," ",dem$contractor_location)
    dem$Street <- as.character(dem$Street)
    dem$City <- as.character(dem$City)
    return(dem)
}

prepareDemolition <- function(){
    dem <- getDemolition()
    dem <- formatDemolition(dem)
    dem <- data.table(dem)
    dem <- geoCodeDem(dem)
    # Slice out those in Detroit only!
    dem <- dem[!(is.na(Lat))] #in case there still are some
    dem <- overDetroit(dem)
    dem$Lat <- round(dem$Lat, digits = 4)
    dem$Lon <- round(dem$Lon, digits = 4)
    return(dem)
}

geoCodeDem <- function(dem){
    # Some 900 entries have NA for coordinates. We'll get them from file, if available, or geocode them with ggmap::geocode    
    demToGeocode <- dem[is.na(Lat)] #I've tested that identical(is.na(dem$Lat), is.na(dem$Lon)) 
    if(nrow(demToGeocode) < 1){
        return()
    }
    geocoded <- tryCatch(
        loadDf("geocoded2.csv"),
        error = function(e){
            dem <- dem[!(is.na(Lat))]
            demToGeocode <- get0("demToGeocode")
            addresses <- paste0(demToGeocode$Street, " ",gsub(",", "",demToGeocode$City))
                        # TESTING: First try geocoding with a small sample
            addresses <- head(addresses, 10)
            # Start the geocoding process - address by address. geocode() function takes care of query speed limit.
            library(ggmap)
            geocoded <- data.frame()
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
            # Export the geocoded results for scrutiny
            assign("geocoded",geocoded,.GlobalEnv)
            writeData(data = geocoded, filename="geocoded2.csv")
            return(geocoded)
        }
    )
    # Finally round and add the geocoded entries to dem
    demToGeocode$Lat <- geocoded$Lat
    demToGeocode$Lon <- geocoded$Lon
    dem <- rbind(dem,demToGeocode)
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
    # Link demolition permits to houses. Both must be data.table, with Lat, Lon rounded to 4
    #   Use a simple inner join of dt!
    # dem: Lat, Lon, Street...  house: HouseId, Count, Lat, Lon.
    setkey(dem, Lat, Lon)
    setkey(houses, Lat, Lon)
    dh <- houses[dem, nomatch=0]
    # Multiple demolition permits have been sent to the same houses, so we must dedup dh
    dh <- unique(dh)
    return(dh)
}

prepareTtSet <- function(dh, houses){
    dh <- dh[!is.na(HouseId)]
    dh <- subset(dh,select=c("Lat","Lon","HouseId", "IncType", "ViolEqMail", "Trash", "Permit", "Maintenance", 
                             "Vehicle", "Water", "PERMIT_ISSUED","LEGAL_USE","PARCEL_SIZE","STORIES"))
    # Multiple demolition permits have been issued to the same house. Remove duplicates.
    dh <- dh[!duplicated(dh$HouseId)]
    n <-nrow(dh)
    # all houses in dh are marked for demolition. Consider them blighted. Form a data table with them, with flag ToDemolition.
    dh$ToDemolition <- TRUE
    # bHouses <- data.table(HouseId = dh$HouseId, Lat = dh$Lat, Lon=dh$Lon, Count = dh$Count, ToDemolition = TRUE)
    # we consider all houses that are not part of dh as non-blighted houses. 
    nbHouses <- houses[!(HouseId %in% dh$HouseId)]
    # nbHouses <- data.table(HouseId = as.integer(nbHouses$HouseId), Lat=nbHouses$Lat, Lon=nbHouses$Lon, 
    #                        Trash=nbHouses$Trash, Permit = nbHouses$Permit,
    #                        ToDemolition = FALSE)
    nbHouses$HouseId <- as.integer(nbHouses$HouseId)
    
    # Take a random sample of non-blighted houses, size equal to blighted.
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

main <- function(){
    library(data.table)
    setDirs()
    if(is.null(get0("houses"))){ 
        # We need to load houses from file or build them again
        houses <- tryCatch(loadDt("housesInDetroit2.csv"), 
                    error = function(e){
                        # File not found, need to rebuild from source data
                        bv <- getBViol()
                        bv <- formatBv(bv)
                        houses <- buildHouses(precision = 4, bv)
                        houses <- overDetroit(houses)
                        # names(houses) <- c("HouseId","Count","Lat","Lon")
                        writeData(data = houses, filename = "housesInDetroit2.csv")
                        assign("houses",houses,.GlobalEnv)
                        return(houses)
                    } 
        )
    }
    # houses: DT with cols Lat, Lon, IncType, ViolEqMail, Trash, Permit, Maintenance, Vehicle, Water
    
    # Get demolition permits, geocode where necessary, clear outliers and assign to houses => dh
    dh <- tryCatch(loadDt("dem-houses2.csv"),
             # stop("Could not load houses with demolition!")
                error = function(e){
                 # File not found, need to rebuild from source data
                    dem <- prepareDemolition()
                    dh <- demolitionHouses(dem,houses)
                    assign("demolitionhouses",dh,.GlobalEnv)
                    write.csv(file = "dem-houses.csv2",x = dh,row.names = FALSE)
                    return(dh)
             } 
    )
    setkey(dh,HouseId)
    prepareTtSet(dh,houses)
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

categorizeBv <- function(bv) {
    bv <- data.table(bv)
    setkey(bv, ViolationCode)    
    ag <- subset(bv,select=c("ViolationCode","ViolDescription"))
    setkey(ag, ViolationCode)
    ag <- unique(ag) #DT only uses the key to compare
    ag$VCategory <- "maintenance"
    ag$VCategory[grepl(pattern="waste",x=ag$ViolDescription,ignore.case=TRUE)] <- "waste"
    ag$VCategory[grepl(pattern="certificate|clearance",x=ag$ViolDescription,ignore.case=TRUE)] <- "permits"
    #ag$VCategory[ag$Group.2 %in% c("9-1-104", "")]
    
    # Finally left outer join to add the new category to bv and remove duplicate columns
    bv <- ag[bv]
    bv <- bv[,c("i.ViolDescription") := NULL]
    return(bv)
}

categorize311 <- function(d311){
    d311$d311Category <- "waste"
    d311$d311Category[grepl(pattern="waste",x=d311$IncType,ignore.case = TRUE)] <- "waste"
    d311$d311Category[grepl(pattern="trash",x=d311$IncType,ignore.case = TRUE)] <- "waste"
    d311$d311Category[grepl(pattern="running water",x=d311$IncType,ignore.case = TRUE)] <- "water"
    d311$d311Category[grepl(pattern="abandoned vehicle",x=d311$IncType,ignore.case = TRUE)] <- "vehicle"
    return(d311)
}

addCountColumnsPerIncType <- function(inc) {
    inc$TrashIncCount <- 0L #default value
    inc[IncType=="waste"]$TrashIncCount <- inc[IncType=="waste"]$Count
    inc$PermitIncCount <- 0L #default value
    inc[IncType=="permits"]$PermitIncCount <- inc[IncType=="permits"]$Count    
    inc$MaintenanceIncCount <- 0L #default value
    inc[IncType=="maintenance"]$MaintenanceIncCount <- inc[IncType=="maintenance"]$Count    
    inc$VehicleIncCount <- 0L #default value
    inc[IncType=="vehicle"]$VehicleIncCount <- inc[IncType=="vehicle"]$Count    
    inc$WaterIncCount <- 0L #default value
    inc[IncType=="water"]$WaterIncCount <- inc[IncType=="water"]$Count        
    return(inc)
}