context("variables")

test_that("uknown method", {
        methods <- c("H", "Hybrid", "R", "Random", "S", "Systematic", "W", "ConstrainedWalk")
        msg <- paste0 (paste("Invalid value to argument 'method'. It must be one of the following:\n  ",
                             toString(paste0('"', methods, '"')) ))
        expect_error (g <- DepartureTime (method = "A"), msg)
})

test_that("dy as a valid year", {
        msg <- paste0("Argument 'dy' is not a valid year. Please provide a valid argument 'dy', e.g. dy = ",
                format(Sys.Date(), "%Y"))
        expect_error (g <- DepartureTime (dy = "A"), msg)
        expect_error (g <- DepartureTime (dy = 1.5), msg)
        expect_error (g <- DepartureTime (dy = 21)) # no msg as menu() cannot be used non-interactively
})

test_that("dm as a valid month", {
        msg <- paste0("Argument 'dm' is not a valid month. Please provide a valid argument 'dm', e.g. dm = ",
                      as.numeric(format(Sys.Date(), "%m")))
        expect_error (g <- DepartureTime (dm = "A"), msg)
        expect_error (g <- DepartureTime (dm = -5), msg)
        expect_error (g <- DepartureTime (dm = 1.4), msg)
        
        dy <- format(Sys.Date(), "%Y")
        msg <- paste0("Are you sure that the year ", dy, " has more than 12 months?")
        expect_error (g <- DepartureTime (dm = 20), msg)
})

test_that("dd as a valid day", {
        msg <- paste0("Argument 'dd' is not a valid month day. Please provide a valid argument 'dd', e.g. dd = ",
                as.numeric(format(Sys.Date(), "%d")))
        expect_error (g <- DepartureTime (dd = "A"), msg)
        expect_error (g <- DepartureTime (dd = -5), msg)
        expect_error (g <- DepartureTime (dd = 1.4), msg)
        
        expect_equal(nrow(DepartureTime(dy = 2020, dm = 2, dd = 29, tmin = 7, tmax = 8)), 12)
        
        dy <- 2020
        dm <- 4
        dd <- 31
        msg <- paste("Unfortunately, in", dy, month.name[as.numeric(dm)], "does not have", dd,
                     "days.\n Please provide a valid argument 'dd', e.g. dd = ",
                     as.numeric(format(Sys.Date(), "%d")))
        expect_error (g <- DepartureTime (dy = dy, dm = dm, dd = dd), msg)
        
        dy <- 2019
        dm <- 2
        dd <- 29
        msg <- paste("Unfortunately, in", dy, month.name[as.numeric(dm)], "does not have", dd,
                     "days.\n Please provide a valid argument 'dd', e.g. dd = ",
                     as.numeric(format(Sys.Date(), "%d")))
        expect_error (g <- DepartureTime (dy = dy, dm = dm, dd = dd), msg)
})

test_that("check tmin and tmax", {
        msg <- "Please provide valid 'tmin' value. It has to be a positive integer between 0 and 23"
        expect_error (gg <- DepartureTime (tmin = "A"), msg)
        expect_error (gg <- DepartureTime (tmin = 1.2), msg)
        expect_error (gg <- DepartureTime (tmin = 24), msg)
        
        msg <- "Please provide valid 'tmax' value. It has to be a positive integer between 1 and 24"
        expect_error (gg <- DepartureTime (tmax = "A"), msg)
        expect_error (gg <- DepartureTime (tmax = 1.2), msg)
        expect_error (gg <- DepartureTime (tmax = 25), msg)
        
        msg <- "Please provide valid 'tmin' and 'tmax' values:\n'tmin' has to be smaller than 'tmax'"
        expect_error (gg <- DepartureTime (tmin = 2, tmax = 1), msg)
})

test_that("check res", {
        msg <- "Please provide valid 'res' value. It has to be a positive integer"
        expect_error (gg <- DepartureTime (res = "A"), msg)
        expect_error (gg <- DepartureTime (res = 1.2), msg)
        expect_error (gg <- DepartureTime (res = -1), msg)
        
        tmin <- 7
        tmax <- 8
        msg <- paste("You are about to create an empty data.frame.
   Considering your 'tmin' and 'tmax' parameters, 'res' cannot be larger than",
                     (tmax - tmin)*60+1)
        expect_error (gg <- DepartureTime (tmin = tmin, tmax = tmax, res = 90), msg)
        
        msg <- "Please consider larger temporal resolution ('res') or
   smaller time window (difference between 'tmin' and 'tmax')"
        expect_error (gg <- DepartureTime (res = 1)) # no msg as menu() cannot be used non-interactively
})

test_that("check MMDD & ptw", {
        msg <- "Parameter 'MMDD' has to be TRUE or FALSE"
        expect_error (gg <- DepartureTime (MMDD = "A"), msg)
        expect_error (gg <- DepartureTime (MMDD = 1), msg)
        
        msg <- "Parameter 'ptw' has to be TRUE or FALSE"
        expect_error (gg <- DepartureTime (ptw = "A"), msg)
        expect_error (gg <- DepartureTime (ptw = 1), msg)
})

test_that("test results", {
        
        expect_equal(nrow(DepartureTime(tmin = 7, tmax = 8, res = 10, method = "S")), 7)
        expect_equal(nrow(DepartureTime(tmin = 7, tmax = 8, res = 10, method = "R")), 6)
        expect_equal(nrow(DepartureTime(tmin = 7, tmax = 8, res = 10, method = "H")), 6)
        
        # test systamatic method
        expect_equal(DepartureTime(tmin = 7, tmax = 8, res = 10, method = "S")[2,2], 
                     paste(format(Sys.Date(), "%m/%d/%Y"), " 07:10"))
        
        # begining and end of the window
        expect_gte(min(as.numeric(gsub(":", "", substr(DepartureTime(tmin = 7, tmax = 8, method = "R")$Date, 14, 17)))), 700)
        expect_lte(max(as.numeric(gsub(":", "", substr(DepartureTime(tmin = 7, tmax = 8, method = "R")$Date, 14, 17)))), 800)
        
        expect_gte(min(as.numeric(gsub(":", "", substr(DepartureTime(tmin = 7, tmax = 8, method = "H")$Date, 14, 17)))), 700)
        expect_lte(max(as.numeric(gsub(":", "", substr(DepartureTime(tmin = 7, tmax = 8, method = "H")$Date, 14, 17)))), 800)
        
        expect_gte(min(as.numeric(gsub(":", "", substr(DepartureTime(tmin = 7, tmax = 8, method = "W")$Date, 14, 17)))), 700)
        expect_lte(max(as.numeric(gsub(":", "", substr(DepartureTime(tmin = 7, tmax = 8, method = "W")$Date, 14, 17)))), 800)
        
        # test time window in Hybrid sampling model
        expect_gte(as.numeric(gsub(":", "", substr(
                DepartureTime(tmin = 7, tmax = 8, res = 10, method = "H")[2,2], 14, 17))), 710)
        expect_lte(as.numeric(gsub(":", "", substr(
                DepartureTime(tmin = 7, tmax = 8, res = 10, method = "H")[2,2], 14, 17))), 720)
})
