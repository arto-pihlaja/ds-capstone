context("Building houses")
# TODO: break dependencies in buildHouses() to make better unit tests

tbv <- tryCatch(
    if(all(dim(tbv)==c(300,6))) return(tbv),
                  error = function(e) return(loadDt("test_formatted_bv.csv"))
)

test_that("Testing buildHouses",{
    # treat 311 incidents like in the unit under test...
    td311 <- getD311()
    td311 <- formatD311(td311)
    th <- buildHouses(4,tbv,td311)
    
    expect_that(nrow(th), equals(6607L))
    # expect_that(nrow(th), equals(562L))
    expect_that(sum(th$Trash),equals(3845L))
    expect_that(sum(th$Vehicle),equals(638L))
    expect_that(sum(th[,j=7:11, with= FALSE]),equals(7364L))
    expect_that(th$Count[47],equals(3))
    expect_that(th$Maintenance[66],equals(1))
})
