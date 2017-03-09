library(testthat)
context("Building the training and test sets")

mock_dh <- data.table(Lat = c(42.3941, 42.3942, 42.3943, 42.3944, 42.3945), 
                      Lon= c(-82.9471, -82.9472, -82.9473, -82.9474, -82.9475), HouseId= c("1001", "1002","1003", "1004", "1005"),
                      IncType = c("vt1", "vt2", "vt3", "vt4", "vt5"), ViolEqMail = c(TRUE, TRUE, FALSE, TRUE, TRUE),
                      Trash = rep(0,5), Permit = c(0,0,1,1,0), Maintenance = rep(0,5), Vehicle = rep(1,5), Water = rep(1,5), 
                      PDummy = rep(FALSE,5), PERMIT_ISSUED = rep(0,5), LEGAL_USE = rep("legaluse",5), 
                        PARCEL_SIZE = rep(100,5), STORIES = rep(2,5), D2 =rep(0,5))

mock_houses <- loadDt("test_houses.csv")

res <- prepareTtSet(mock_dh, mock_houses)
test_that("Correctly selecting features", {
    expect_that(length(mock_dh), equals(14))
    expect_that(names(res), equals(c("Lat","Lon","HouseId", "IncType", "ViolEqMail", "Trash", "Permit", "Maintenance", 
                                    "Vehicle", "Water", "PERMIT_ISSUED","LEGAL_USE","PARCEL_SIZE","STORIES")))
})
