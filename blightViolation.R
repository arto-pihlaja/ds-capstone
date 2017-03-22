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
    
    coord <- t(pbapply(bv,1,extractCoord))
    # Add Violation and Mailing latitude and longitude as columns
    splitCoord <- function(crow){
        c <- c(crow[1],crow[2],crow[3],crow[4])
    }
    coord <- t(pbsapply(coord,splitCoord))
    bv$Lat <- as.numeric(coord[,1])
    bv$Lon <- as.numeric(coord[,2])
    bv$MLat <- as.numeric(coord[,3])
    bv$MLon <- as.numeric(coord[,4])

    # Add a column that indicates if the violation and mail addresses are equal (a proxy for the owner living in the building)
    bv$ViolEqMail <- (bv$Lat == bv$MLat & bv$Lon == bv$MLon)
    # Add a column for engineered violation category (and turn bv into a data.table)
    print("Start categorizing Blight Violations")
    bv <- categorizeBv(bv)
    setkey(bv,Lat,Lon,VCategory)
    bv <- subset(bv, select = c("VCategory", "ViolName", "ViolationCode", "Lat", "Lon", "ViolEqMail"))
    return(bv)
} # formatBv

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

getD311 <- function() {
    d311 <- loadDf("detroit-311.csv")
    return(d311)
}


categorize311 <- function(d311){
    d311$d311Category <- "waste"
    d311$d311Category[grepl(pattern="waste",x=d311$IncType,ignore.case = TRUE)] <- "waste"
    d311$d311Category[grepl(pattern="trash",x=d311$IncType,ignore.case = TRUE)] <- "waste"
    d311$d311Category[grepl(pattern="running water",x=d311$IncType,ignore.case = TRUE)] <- "water"
    d311$d311Category[grepl(pattern="abandoned vehicle",x=d311$IncType,ignore.case = TRUE)] <- "vehicle"
    return(d311)
}

formatD311 <- function(d311){
    d311 <- subset(
        d311,
        select = c(
            issue_type,ticket_closed_date_time,ticket_created_date_time,
            address,lat,lng
        )
    )
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
    
    #There's no equivalent for Mailing address in 311 data. Mark NA for now, then decide later about using the feature.
    d311$ViolEqMail <- "NA"
    d311 <- categorize311(d311)
    d311$IncType <- d311$d311Category
    d311 <- subset(d311, select=c("Lat", "Lon", "IncType", "ViolEqMail"))
}

getCrime <- function(){
    cr <- loadDt("detroit-crime.csv")
    cr <- subset(cr,select=c("CATEGORY", "LAT", "LON"))
    names(cr) <- c("IncType", "Lat", "Lon")
    return(cr)
}

buildHouses <- function(precision, bv, d311){
    inc <- data.table(Lat = bv$Lat, Lon = bv$Lon, IncType = bv$VCategory, ViolEqMail = bv$ViolEqMail )
    rm(bv)
    # d311 <- data.table(Lat = d311$Lat, Lon = d311$Lon, IncType = d311$IncType, ViolEqMail = d311$ViolEqMail)
    inc <- rbind(inc, d311)
    rm(d311)

    # Remove geographic outliers by comparing coordinates against Detroit city boundaries
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
    # Now add the count of each incident type per (Lat, Lon) pair to every row. This also sets the key to Lat, Lon. ViolEqMail and Count lose their meaning.
    inc <- inc[ , j=list(IncType, ViolEqMail, Count, max(TrashIncCount), max(PermitIncCount), max(MaintenanceIncCount), max(VehicleIncCount), max(WaterIncCount)), by=list(Lat, Lon)]
    # Leave only one row per unique coordinates.
    inc <- unique(inc)
    colnames(inc) <- c("Lat", "Lon", "IncType", "ViolEqMail", "Count", "Trash", "Permit", "Maintenance", "Vehicle", "Water")
    inc$Count <- inc$Trash + inc$Permit + inc$Maintenance + inc$Vehicle + inc$Water
    houses <- inc
    rm(inc)
    # Houses are now sorted by ascending Lat, Lon. Add row number as unique HouseId
    houses <- cbind(row.names(houses),houses)
    cnh <- colnames(houses)
    cnh[1] <- "HouseId"
    colnames(houses) <- cnh
    print(paste('Total counts in summary table houses: ', sum(houses[,j=7:11,with=FALSE])))
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

getDemolition <- function(){
    dem <- read.csv(file=
                        "C:/Users/setup/Documents/coursera/uw/datascience/capstone/data/detroit-demolition-permits.tsv",
                    sep = "\t", stringsAsFactors = FALSE)
    return(dem)
}

formatDemolition <- function(dem){
    library(stringr)
    siteLoc <- strsplit(dem$site_location, split = "\n")
    
    # Get the coordinates
    crd <- str_extract_all(dem$site_location, "[(].+[)]", simplify = TRUE) # we get (42.nnnn, -83.nnn)
    crd <- gsub("[()]","",crd)
    crd <- strsplit(crd,split = ",")
    # strsplit returns a list. Now need to replace missing coordinates with NA before Reducing list to matrix,
    # or else the rows won't match.
    crd <- Map(function(crdlistitem){ 
        if(length(crdlistitem==2)) return(crdlistitem) 
        else return(NA) }, crd)
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
    dem$Street <- as.character(dem$Street)
    # In some cases, the address can't be found from site_location, but from the other fields
    dem[is.na(dem$Street),]$Street <- dem[is.na(dem$Street),]$SITE_ADDRESS
    dem[is.na(dem$City),]$City <- "Detroit, MI"
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
    print(paste("Found", nrow(dem), "demolition permits."))
    dem$Lat <- round(dem$Lat, digits = 4)
    dem$Lon <- round(dem$Lon, digits = 4)
    return(dem)
}

geoCodeDem <- function(dem){
    # 800+ entries have NA for coordinates. We'll get them from file, if available, or geocode them with ggmap::geocode    
    demToGeocode <- dem[is.na(Lat)] #I've tested that identical(is.na(dem$Lat), is.na(dem$Lon)) 
    dem <- dem[!is.na(Lat)]
    if(nrow(demToGeocode) < 1){
        return()
    }
    geocoded <- tryCatch(
        # Geocoding takes quite a while, so let's not do it unless we have to!
        loadDf("geocoded.csv"),
        error = function(e){
            dem <- dem[!(is.na(Lat))]
            demToGeocode <- get0("demToGeocode")
            addresses <- paste0(demToGeocode$Street, " ",gsub(",", "",demToGeocode$City))

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
            writeData(data = geocoded, filename="geocoded3.csv")
            return(geocoded)
        }
    )
    # Finally round and add the geocoded entries to dem
    demToGeocode$Lat <- geocoded$Lat
    demToGeocode$Lon <- geocoded$Lon
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
    # Link demolition permits to houses. Both must be data.table, with Lat, Lon rounded to 4
    #   Use a simple inner join of dt!
    # dem: Lat, Lon, Street...  house: HouseId, Count, Lat, Lon.
    setkey(dem, Lat, Lon)
    setkey(houses, Lat, Lon)
    dh <- houses[dem, nomatch=0]
    print(paste(nrow(dh),"demolition permits are assigned to houses (i.e. locations with incidents)."))
    # Multiple demolition permits have been sent to the same houses, so we must dedup dh
    dh <- unique(dh)
    print(paste(nrow(dh), "houses have been assigned a demolition permit."))
    return(dh)
}

prepareTtSet <- function(dh, houses){
    dh <- dh[!is.na(HouseId)]
    dh <- subset(dh,select=c("Lat","Lon","HouseId", "IncType", "ViolEqMail", "Trash", "Permit", "Maintenance", 
                             "Vehicle", "Water", "Count")) 
    # Multiple demolition permits have been issued to the same house. Remove duplicates.
    dh <- dh[!duplicated(dh$HouseId)]
    n <-nrow(dh)
    # all houses in dh are marked for demolition. Consider them blighted. Form a data table with them, with flag ToDemolition.
    dh$ToDemolition <- TRUE
    bHouses <- dh
    rm(dh) 
    # we consider all houses that are not part of dh as non-blighted houses. 
    nbHouses <- houses[!(HouseId %in% bHouses$HouseId)]
    nbHouses$HouseId <- as.integer(nbHouses$HouseId)
    nbHouses$ToDemolition <- FALSE
    
    # Take a random sample of non-blighted houses, size equal to blighted.
    nbHouses <- nbHouses[sample(1:nrow(nbHouses), size=nrow(bHouses), replace=FALSE),]
    print(paste("Number of blighted houses: ", nrow(bHouses)))
    print(paste("Number of non-blighted houses: ", nrow(nbHouses)))
    ttSet <- rbind(bHouses, nbHouses)
    # Ordering these by HouseId will mix the b and nb houses
    ttSet <- ttSet[order(ttSet$HouseId),]
    n <- nrow(ttSet)
    trainInd <- sample(1:n, size=round(0.7*n),replace=FALSE)
    trainSet <- ttSet[trainInd,]
    testSet <- ttSet[-trainInd,]
    res <- list(trainSet, testSet)
    return(res)
}

main <- function(){
    library(data.table)
    library(pbapply)
    library(stringr)
    library(tree)
    library(ROCR)
    setDirs()
    if(is.null(get0("houses"))){ 
        # We need to load houses from file or build them again
        houses <- tryCatch(loadDt("houses1.csv"), 
                    error = function(e){
                        # File not found, need to rebuild from source data
                        bv <- getBViol()
                        bv <- formatBv(bv)
                        d311 <- getD311()
                        d311 <- formatD311(d311)
                        houses <- buildHouses(precision = 4, bv, d311)
                        houses <- overDetroit(houses)
                        # names(houses) <- c("HouseId","Count","Lat","Lon")
                        writeData(data = houses, filename = "houses.csv")
                        assign("houses",houses,.GlobalEnv)
                        return(houses)
                    } 
        )
    }
    # houses: DT with cols Lat, Lon, HouseId, IncType, ViolEqMail, Trash, Permit, Maintenance, Vehicle, Water
    
    # Get demolition permits, geocode where necessary, clear outliers and assign to houses => dh
    dem <- prepareDemolition()
    dh <- demolitionHouses(dem,houses)
    setkey(dh,HouseId)
    tt <- prepareTtSet(dh,houses)
    trainSet <- tt[[1]]
    trainSet$ViolEqMail <- as.logical(trainSet$ViolEqMail)
    trainSet[is.na(trainSet$ViolEqMail)]$ViolEqMail <- FALSE
    testSet <- tt[[2]]
    testSet$ViolEqMail <- as.logical(testSet$ViolEqMail)    
    testSet[is.na(testSet$ViolEqMail)]$ViolEqMail <- FALSE
    rm(tt,dh,houses)
    assign("trainSet",trainSet,.GlobalEnv)
    assign("testSet",testSet,.GlobalEnv)
    fn <- formula(ToDemolition ~ Count) # Naive model
    predictTree(fn,trainSet,testSet, "naive")
    
    fe <- formula(ToDemolition ~ ViolEqMail + Trash + Permit + Maintenance + Vehicle + Water)
    predictTree(fe,trainSet,testSet, "engineered")
    
    logreg <- glm(formula = fn, data=trainSet, family = binomial)
    res3 <- predict(logreg,newdata=testSet, type="response")
    # testres <- ifelse(testres>0.5,TRUE,FALSE)
    pr <- prediction(res3,testSet$ToDemolition)
    prf <- performance(pr, measure="tpr",x.measure = "fpr")
    plot(prf)
    auc <- performance(pr, measure = "auc")
    auc <- auc@y.values[[1]]
    print(paste("Accuracy (Area under the curve) for log.reg.model", auc))
} #main

predictTree <- function(f,train,test, modeltype){
    treeModel <- tree(f,data=train)
    res <- predict(treeModel,newdata=test)
    pr <- prediction(res,test$ToDemolition)
    prf <- performance(pr, measure = "tpr", x.measure = "fpr")
    plot(prf, main = modeltype)
    auc <- performance(pr, measure="auc")@y.values
    print(paste("Accuracy (Area under the curve) for", modeltype, "model", auc))
    print(treeModel)
}


# # Sample code for reading URL
# url <- ""
# cs_data <- getURL(url)
# cs_data <- read.csv(textConnection(cs_data))
# describe(cs_data)

