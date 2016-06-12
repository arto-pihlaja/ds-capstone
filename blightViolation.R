extractViolationInfo <- function(bv) {
    # Save violation information for later analysis
    violations <- data.frame(bv$ViolationCode,bv$ViolDescription,bv$Disposition)
    write.csv(file="violations.csv",x=violations,row.names=FALSE)
    assign("violations",violations,.GlobalEnv)     
}
extractFields <- function(bv){
    # Select a subset of the fields
    bv <- subset(bv,select = c("TicketID","ViolationCode",
                                "ViolationAddress","ViolationStreetNumber",
                               "ViolationStreetName"))
    names(bv) <- c("ID","Code","Address","VStrNr","VStrName")
    return(bv)
}
getUniqueVCodes <- function(bv){
    un <- unique(bv$ViolationCode,use.names=FALSE)
    write.csv(file="violcodes.csv",x=un, row.names = FALSE)
    return(un)
}
extractCoord <- function(bvsRow) {
    # bvsRow is like 26288 2566 GRAND BLVD\nDetroit, MI\n(42.36318237000006, -83.09167672099994)
    rowStr <- unlist(strsplit(toString(bvsRow),split = "\n"))[3]
    # Now we have something like (42.32544917300004, -83.06413908999997)
    coord <- gsub("[()]","",rowStr)
    coord <- unlist(strsplit(coord,split=","))
    return(coord)    
}
getViolationCoordinates <- function(mainEnv){
    if(!is.null(bvs <- get0("bvs",mainEnv))){
        coord <- t(apply(bvs,1,extractCoord))
        bvs["Lat"] <- as.numeric(coord[,1]) 
        bvs["Lon"] <- as.numeric(coord[,2])
        return(bvs)
    }
}

processViolations <- function(){
    setwd("C:/Users/setup/Documents/coursera/uw/datascience/capstone")
    mainEnv <- environment()
    if(is.null(bvs <- get0("bvs",.GlobalEnv))){
        # Need to reload data from file and format it
        bv <- read.csv("detroit-blight-violations-s.csv", stringsAsFactors = FALSE)        
        un <- getUniqueVCodes(bv)
        extractViolationInfo(bv)
        bvs <- extractFields(bv)
        bvs <- getViolationCoordinates(mainEnv)
        assign("bvs",bvs,.GlobalEnv)    
        rm(bv)
    }
    houses <- dedupWithRound(bvs)
    houses$id <- row.names(houses)
    # plot(houses)
    write.csv(file="houses.csv",x=houses,row.names=TRUE)
    assign("houses",houses,.GlobalEnv)    
}

plot <- function(bvs) {
        # loading the required packages
        library(ggplot2)
        library(ggmap)    
        # getting the map
        # mapgilbert <- get_map(location = c(lon = mean(as.numeric(bvs$lon)), lat = mean(as.numeric(bvs$lat))), zoom = 14,
        mapgilbert <- get_map(location = c(lon = mean(bvs$Lon), lat = mean(bvs$Lat)), zoom = 12,
                              # maptype = "satellite", scale = "auto")    
                              maptype = "roadmap", scale = "auto")    
        # plotting the map with some points on it
        ggmap(mapgilbert) +
            geom_point(data = bvs, aes(x = Lon, y = Lat, fill = "red", alpha = 0.8), size = 3, shape = 21) +
            guides(fill=FALSE, alpha=FALSE, size=FALSE)      
} 

dedupWithRound <- function(bvs){
    library(dplyr)
    bvs %>% select(Lon, Lat) %>% group_by(Lon=round(Lon,4), Lat=round(Lat,4)) %>% summarise(count=n())
}

findDefaultCoordinates <- function(bvs,precision){
    #Here, we try to find observations whose coordinates are the same to a level of precision
    agg <- aggregate(x=bvs$ID, by=list(round(bvs$Lon,digits=precision),round(bvs$Lat,digits=precision)),length)
    agg <- agg[agg$x>2,] #If the number of x is high, we probably have a set of default coordinates
    # sprintf("%.8f",agg[,1])
    return(agg)        
}

get311 <- function(){
    d311 <- read.csv("detroit-311.csv", stringsAsFactors = FALSE)   
    # We'll ignore some alerts, as they are not specific to a house 
    # nor caused by the house owner
    ignore <- c("Water Main Break", "Fire Hydrant Issue", "Traffic Signal Issue",
                "Potholes", "Test (internal use only, public issue)", 
                "Customer Service (internal use only, private issue)",
                "Graffiti Abatement (internal use only, public issue)")
    # d311 <- d311[!(d311$issue_type %in% ignore),]
    u311type <- unique(d311$issue_type,use.names=FALSE)
    d311 <- subset(d311,!(d311$issue_type %in% ignore),
                   select= c(issue_type,ticket_closed_date_time,ticket_created_date_time,
                             address,lat,lng))
    assign("u311type",u311type,envir=.GlobalEnv)
    n311 <- names(d311)
    n311[5] <- "Lat"
    n311[6] <- "Lon"
    names(d311) <- n311
    return(d311)    
}

# For a pair of coordinates (cRow), see if it matches any of the rows in houses
findInHouses <- function(cRow, houses){
    lon <- cRow["Lon"]
    lat <- cRow["Lat"]
    isInDaHouse <- function(house,lon,lat){
        #     lon <- get0("lon",envir=parent.frame())
        #     lat <- get0("lat")
        off <- 0.0005
        hlon <- as.numeric(house["Lon"])
        hlat <- as.numeric(house["Lat"])
        if(lon >= hlon & lon <= (hlon + off) 
           & lat >= hlat & lat <= (hlat + off)){
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

# Check if the incidents in the file already exist in houses. If not, add to houses.
findCoord <- function(houses,LonV,LatV){
    coord <- data.frame(Lon=LonV,Lat=LatV)
    apply(coord,1,findInHouses,houses)
  }

# Not finalised, not used
findInHouses2 <- function(lon,lat,houses){
# We assume houses contains unique values, so only one hit should come 
    house <- houses[round(houses$Lon,6)== round(lon,6) & round(houses$Lat,2)==round(lat,2),]
    
    return(house)
}
