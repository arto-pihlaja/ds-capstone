source("blightViolation.R")
test_that("Blight Violation data was read", {
    bv <- getBViol()
    expect_equal(ncol(bv), 31)
    expect_is(bv, "data.table") 
})

test_that("Coordinates extracted",{
    bv <- formatBv(bv)
    expect_that(bv$Lat[1], equals(42.363, tolerance = 0.01))
    expect_that(bv$Lon[1], equals(-83.09, tolerance = 0.01))
    expect_equal(bv$ViolEqMail[3], TRUE)
})

test_that("Geographic slicing with overDetroit function",{
    inTown <- data.frame(Lat = c(42.363, 42.361), Lon = c(-83.092, -83.095))
    outtaTown <- data.frame(Lat = c(44.363, 40.361), Lon = c(-81.092, -80.095))
    it <- overDetroit(inTown)
    expect_equal(nrow(it), 2)
})