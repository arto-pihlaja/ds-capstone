context("Processing blight violation data")

bvt <- loadDt("testviol.csv") # Small file
test_that("Loading blight data",{
 expect_that(nrow(bvt),equals(300))
    expect_equal(ncol(bvt),31)
    expect_that(bvt$TicketID[1], equals(268252))
})

bvt <- formatBv(bvt)
ut <- unique(bvt$VCategory)
test_that("Formatting blight data",{
    expect_that(nrow(bvt),equals(300))
    expect_equal(ncol(bvt),6)
    expect_equal(ut, c("waste", "maintenance", "permits"))
    expect_equal(bvt$VCategory[4], "waste")
    expect_equal(bvt$ViolEqMail[4], TRUE)
    expect_equal(bvt$ViolEqMail[5], FALSE)
    expect_equal(sum(bvt$VCategory=="waste"),71)
    expect_equal(bvt$VCategory[135], "permits")
})

          