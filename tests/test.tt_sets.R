library(testthat)
context("Building the training and test sets")

# mock_dh <- data.table(Lat = c(42.3941, 42.3942, 42.3943, 42.3944, 42.3945), 
#                       Lon= c(-82.9471, -82.9472, -82.9473, -82.9474, -82.9475), HouseId= c("1001", "1002","1003", "1004", "1005"),
#                       IncType = c("vt1", "vt2", "vt3", "vt4", "vt5"), ViolEqMail = c(TRUE, TRUE, FALSE, TRUE, TRUE),
#                       Trash = rep(0,5), Permit = c(0,0,1,1,0), Maintenance = rep(0,5), Vehicle = rep(1,5), Water = rep(1,5), 
#                       PDummy = rep(FALSE,5), PERMIT_ISSUED = rep(0,5), LEGAL_USE = rep("legaluse",5), 
#                         PARCEL_SIZE = rep(100,5), STORIES = rep(2,5), D2 =rep(0,5))

mock_houses <- loadDt("test_houses.csv")
mock_dh <- loadDt("test_demolitionhouses.csv")

set.seed(15)#so random sample will be repeatable
res <- prepareTtSet(mock_dh, mock_houses)
test_that("Correctly selecting features", {
    train <- res[[1]]
    test <- res[[2]]
    expect_that(nrow(train), equals(420))
    expect_that(nrow(test), equals(180))
    expect_that(names(train), equals(c("Lat","Lon","HouseId", "IncType", "ViolEqMail", "Trash", "Permit", "Maintenance", 
                                    "Vehicle", "Water", "ToDemolition")))
    # total 300 blighted, 300 non-blighted
    expect_equal(sum(train$ToDemolition==TRUE),210)
    expect_equal(sum(test$ToDemolition==TRUE),90)
    expect_equal(sum(test$ToDemolition==FALSE),90)
})
