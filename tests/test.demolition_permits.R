library(testthat)
library(data.table)
context("Formatting demolition permits")

### Test formatDemolion
mock_dem <- data.frame(foo = c(1,2,3,4,5,6),
                       site_location = c(
                           "4331 BARHAM\nDetroit, MI\n(42.394101, -82.9471)",
                           "4331 WHATEVA\nDetroit, MI\n(42.394102, -82.9472)",
                           "(42.394103, -82.9473)",
                           "4331 SUMTHIN\nDetroit, MI\n(42.394104, -82.9474)",
                           "4331 FOOBAR\nDetroit, MI\n",
                           "4331 GETTHIS\nDetroit, MI\n(42.394106, -82.9476)"
                       ),
                       stringsAsFactors = FALSE)
mock_dem$owner_location <- NA
mock_dem$contractor_location <- NA

res <- formatDemolition(mock_dem)

test_that("Function formatDemolition extracts coordinates from site_address",{
    expect_that(is.data.frame(res), equals(TRUE))
    expect_that(nrow(res), equals(6))
    expect_that(res[1,1], equals(42.394101))
    expect_that(res[1,2], equals(-82.9471))
    expect_that(res[6,2], equals(-82.9476))
    expect_that(res[6,3], equals("4331 GETTHIS"))
})

### Test demolitionHouses
# houses: DT with cols Lat, Lon, IncType, ViolEqMail, Trash, Permit, Maintenance, Vehicle, Water
mock_houses <- data.table(Lat = c(42.3941, 42.3942, 42.3943, 42.3944, 42.3945), 
                          Lon= c(-82.9471, -82.9472, -82.9473, -82.9474, -82.9475), 
                          IncType = c("vt1", "vt2", "vt3", "vt4", "vt5"), ViolEqMail = c(TRUE, TRUE, FALSE, TRUE, TRUE),
                          Trash = rep(0,5), Permit = c(0,0,1,1,0), Maintenance = rep(0,5), Vehicle = rep(1,5), Water = rep(1,5))


mock_dem <- data.table(Lat = c(42.3941, 42.3942, 42.3942, 42.3944, 42.3945), 
                       Lon= c(-82.9471, -82.9472, -82.9472, -82.9470, -82.9475),
                       dummy_dem = rep(FALSE,5))
mock_houses <- data.table(mock_houses)
res <- demolitionHouses(mock_dem, mock_houses)

test_that("Func demolitionHouses, testing the join", {
    expect_that(is.data.frame(res), equals(TRUE))
    # The house with no dem permit should not be included
    expect_that(sum(which(res$Lat==42.3943 & res$Lon==-82.9473)),equals(0))
    expect_that(nrow(res),equals(3))
    expect_that(length(res),equals(10))
    expect_that(names(res),equals(c("Lat", "Lon", "IncType", "ViolEqMail", "Trash", "Permit", "Maintenance", 
                                    "Vehicle", "Water", "dummy_dem")))
})