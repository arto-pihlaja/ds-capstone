context("Building houses")
# TODO: break dependencies in buildHouses() to make better unit tests
tbv <- tryCatch(
    if(all(dim(tbv)==c(300,6))) return(tbv),
                  error = function(e) return(loadDt("test_formatted_bv.csv"))
)

# treat 311 incidents like in the unit under test...
td311 <- tryCatch(td311,
                  error = function(e) return(loadDt("test_formatted_d311.csv"))
)
if (nrow(td311) != 7077){
    td311 <- getD311()
    }

td311$ViolEqMail <- "NA"
td311 <- categorize311(td311)

test_that("Testing D311 manipulation",{
 expect_that(nrow(td311),equals(7077))
    expect_that(names(td311)[8], equals("d311Category"))
    expect_that(round(td311$Lat[15],5), equals(42.35395))
    expect_that(td311$d311Category[67], equals("vehicle"))
    t <- table(td311$d311Category)
    expect_that(length(t), equals(3))
    expect_that(as.integer(t[2]), equals(2655L))
})

th <- buildHouses(4,tbv)
test_that("Testing buildHouses",{
    
    expect_that(nrow(th), equals(6607L))
    expect_that(sum(th$Trash),equals(3845L))
    expect_that(sum(th$Vehicle),equals(638L))
    expect_that(sum(th[,j=6:10, with= FALSE]),equals(7323L))
})
