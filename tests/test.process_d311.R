context("manipulate d311")
td311 <- tryCatch(td311,
                  error = function(e) return(loadDt("detroit-311.csv"))
)
# TODO - tests

# td311$ViolEqMail <- "NA"
# td311 <- categorize311(td311)
# 
# test_that("Testing D311 manipulation",{
#     expect_that(nrow(td311),equals(300))
#     expect_that(names(td311)[8], equals("d311Category"))
#     expect_that(round(td311$Lat[15],5), equals(42.35395))
#     expect_that(td311$d311Category[67], equals("vehicle"))
#     t <- table(td311$d311Category)
#     expect_that(length(t), equals(3))
#     # expect_that(as.integer(t[2]), equals(2655L))
#     expect_that(as.integer(t[2]), equals(23L))
# })