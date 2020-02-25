#' @title Generate Departure times
#'
#' @description \code{DepartureTimes} generate data.frame with departure times for accessibility analyses with temporal resolution (e.g GTFS)
#'
#' @param method (type: character) define sampling method. Optional values:
#'
#' + \code{`R`} OR \code{`Random`}: Simple random sampling;
#'
#' + \code{`S`} OR \code{`Systematic`}: Systematic sampling;
#'
#' + \code{`H`} OR \code{`Hybrid`}: Hybrid sampling;
#'
#' + \code{`W`} OR \code{`Walk`}: Constrained random walk sampling;
#'
#' default: \strong{\code{"H"}}
#'
#' @param dy year of analysis (format: YYYY)
#' @param dm month of analysis (format: MM)
#' @param dd day of analysis (format: DD)
#'
#' default: \strong{default: system date}.
#'
#' @param tmin lower limit of the time window
#' @param tmax upper limit of the time window
#'
#' \strong{default: all day} (i.e 00:00 - 24:00)
#'
#' @param res temporal resolution
#'
#' \strong{default: 5 minutes}
#'
#' @param MMDD date format of the output (TRUE / FALSE)
#'
#' + \code{TRUE} = MM/DD/YYYY
#'
#' + \code{FALSE} = DD/MM/YYYY
#'
#' \strong{default: TRUE}
#'
#' @param ptw Bolean: whether or not print limits of subsetted time-windows;
#'
#' \strong{default: FALSE;}
#'
#' @examples
#' \dontrun{
#' DepartureTime()
#'
#' DepartureTime(method = "S",       # systematic sampling method
#'  dm = 5, dd = 15,                 # user-defined date: 15th May, 2019 (current year)
#'  tmin = 7, tmax = 10,             # user-defined time window (07:00 - 10:00)
#'  res = 15)                        # user-defined temporal resolution (15 minutes)
#'  }
#'
#'
#' @export
#' @return data.frame with departure times in rows
#'
DepartureTime <- function(method = "H",
                          dy = format(Sys.Date(), "%Y"),
                          dm = format(Sys.Date(), "%m"),
                          dd = format(Sys.Date(), "%d"),
                          tmin = 0,
                          tmax = 24,
                          res = 5,
                          MMDD = TRUE,
                          ptw = FALSE) {

   # test if all parameters are correct ----

   # test if method is valid
   methods <- c("H", "Hybrid", "R", "Random", "S", "Systematic", "W", "ConstrainedWalk")

   if(!(method %in% methods) ) stop(paste("Invalid value to argument 'method'. It must be one of the following:\n  ",
                                          toString(paste0('"', methods, '"')) ))

   # test if 'dy' is a valid year
   if(dy != format(Sys.Date(), "%Y"))  {

      # check if 'dy' is numeric:
      if(!is.numeric(dy))  stop(paste0(
         "Argument 'dy' is not a year. Please provide a valid argument 'dy', e.g. dy = ",
         format(Sys.Date(), "%Y")))


      if(is.numeric(dy))  {

         # check if 'dy' is integer
         if(dy%%1!=0)  stop(
            paste0("Argument 'dy' is not a year. Please provide a valid argument 'dy', e.g. dy = ",
                   format(Sys.Date(), "%Y")))

      }
   }

   # test if 'dm' is a valid month
   if(dm != format(Sys.Date(), "%m"))  {

      # check if 'dm is numeric
      if(!is.numeric(dm))  stop(paste0(
         "Argument 'dm' is not a month Please provide a valid argument 'dm', e.g. dm = ",
         as.numeric(format(Sys.Date(), "%m"))))

      if(is.numeric(dm))  {

         # check if 'dm' is a positive integer
         if(dm%%1!=0 | dm < 1)  stop(
            paste0("Argument 'dm' is not a month Please provide a valid argument 'dm', e.g. dm = ",
                   as.numeric(format(Sys.Date(), "%m"))))

         # check if 'dm is not higher than 12
         if(dm > 12)  stop(paste0("Are you sure that ", dy, " year has more than 12 months?") )
      }
   }

   # test if 'dd' is a valid month day
   if(dd != format(Sys.Date(), "%d"))  {

      # check if 'dm is numeric
      if(!is.numeric(dd))  stop(paste0(
         "Argument 'dd' is not a month day. Please provide a valid argument 'dd', e.g. dd = ",
         as.numeric(format(Sys.Date(), "%d"))))

      if(is.numeric(dd)) {

         # check if 'dd' is a positive integer
         if(dd%%1!=0 | dd < 1)  stop(
            paste0("Argument 'dd' is not a month day. Please provide a valid argument 'dd', e.g. dd = ",
                   as.numeric(format(Sys.Date(), "%d"))))

         # check if 'dd' is a valid month day in month 'dm' and year 'dy'
         months31 <- c(1, 3, 5, 7, 8, 10, 12)
         if(dm %in% months31 & dd > 31 |
            (!dm %in% months31) & dd > 30 |
            (dm == 2 | dm == "02") & dd > 29 |
            as.numeric(dy)%%4 != 0 & (dm == 2 | dm == "02") & dd > 28) stop(
               paste("Unfortunately, in", dy, month.name[as.numeric(dm)], "does not have", dd,
                     "days.\n Please provide a valid argument 'dd', e.g. dd = ",
                     as.numeric(format(Sys.Date(), "%d"))))


      }

   }

   # confirm selection of 'dy' (in case is from a past or future)
   if(is.numeric(dy))  {

      if(dy%%1==0 & (dy < 2015 | dy > as.numeric(format(Sys.Date(), "%Y"))))  {
         response_menu <- utils::menu(c("Yes", "No"), title=paste0("You have selected ", dy,
                                                                   " as a year. \n  Do you want to continue?"))
         if(response_menu == 1)  warning(paste("You are using", dy, "as a year"))
         if(response_menu == 2)  stop("Please provide a a valid argument 'dy', e.g. dy = ",
                                      format(Sys.Date(), "%Y"))

      }
   }

   # test tmin and tmax values (positive integers and tmin < tmax)
   if(!is.numeric(tmin))
      stop("Please provide valid 'tmin' value. It has to be a positive integer [0:23]")

   if(!is.numeric(tmax))
      stop("Please provide valid 'tmax' value. It has to be a positive integer [1:24]")

   if(is.numeric(tmin))  {

      if(tmin%%1!=0 | tmin < 0 | tmin > 23)
         stop("Please provide valid 'tmin' value. It has to be a positive integer [0:23]")
   }

   if(is.numeric(tmax))  {

      if(tmax%%1!=0 | tmax < 1 | tmax > 24)
         stop("Please provide valid 'tmax' value. It has to be a positive integer [1:24]")
   }

   if(tmin >= tmax)
      stop("Please provide valid 'tmin' and 'tmax' values:
   'tmin' has to be smaller than 'tmax'. (tmin < tmax)")


   # test res value (a positive integer)
   if(!is.numeric(res))
      stop("Please provide valid 'res' value. It has to be a positive integer")

   if(is.numeric(res))  {

      if(res <= 0 | res%%1!=0)
         stop("Please provide valid 'res' value. It has to be a positive integer")

      if(res > (tmax - tmin)*60+1)
         stop(paste("You are about to create an empty data.frame.
   Considering your 'tmin' and 'tmax' parameters, 'res' cannot be larger than",
                    (tmax - tmin)*60+1))

      if((tmax - tmin)*60/res > 300) {

         response_menu <- utils::menu(c("Yes", "No"),
                               title=paste0("You are about to create large data.frame (", (tmax - tmin)*60/res,
                                            " rows). \n Do you want to continue?"))

         if(response_menu == 2)  stop("Please consider larger temporal resolution ('res') or
   smaller time window (difference between 'tmin' and 'tmax')")

      }

   }

   # test if MMDD is bolean (TRUE or FALSE)
   if(!(isTRUE(MMDD) | isFALSE(MMDD))) stop("Parameter 'MMDD' has to be TRUE or FALSE")

   # test if ptw is bolean (TRUE or FALSE)
   if(!(isTRUE(ptw) | isFALSE(ptw))) stop("Parameter 'ptw' has to be TRUE or FALSE")


   # Create data.frame with departure times ----

   # create data.frame with all possible departure times
   Temp <- data.frame(Date = seq(
      from = as.POSIXct(paste(dy,dm,dd, sep = "-")) + as.difftime(tmin, units="hours"),
      to = as.POSIXct(paste(dy,dm,dd, sep = "-")) + as.difftime(tmax, units="hours"),
      by = 60))


   # hybrid sampling method ----
   if(method == "H" | method == "Hybrid") {

      # select departure times applying hybrid sampling method
      fixed_rows <- seq(1, nrow(Temp), res)
      random_rows <- numeric(0)

      n <- 1

      for(i in fixed_rows){
         row_max <- i + res - 1
         if(row_max <= nrow(Temp)){ # it limits time-window to those that have a full coverage
            if(ptw == TRUE){print(paste("time window", paste0(n, ":"), "start:", Temp[i,], "end:", Temp[row_max,]))}
            random_rows <- c(random_rows, sample(i:row_max, 1)) }
         n <- n +1}

      Temp <- Temp[random_rows,]
      # rm(n, i, row_max, fixed_rows, random_rows)
      }

   # simple random method ----
   if(method == "R" |  method == "Random") {
      # random_rows <- sample(1 : nrow(Temp), nrow(Temp)/res)
      Temp <- sort(Temp[sample(1 : nrow(Temp), nrow(Temp)/res), ])
      }

   # systematic method ----
   if(method == "S" |  method == "Systematic") {
      Temp <- Temp[seq(1, nrow(Temp), res),]
      }

   # Constrained Random Walk Sampling ----
   if(method == "W" |  method == "ConstrainedWalk") {

      random_rows <- sample(1:(res), 1)   # select first departure time

      while(utils::tail(random_rows, n=1) <= nrow(Temp)) {
         random_rows <- c(random_rows,
                          sample((round(utils::tail(random_rows, n=1) + res*0.5, 0)):
                                    (round(utils::tail(random_rows, n=1) + res*1.5, 0)), 1) ) }

      random_rows <- random_rows[-length(random_rows)] # remove departure time out of time-window
      Temp <- Temp[random_rows,]

      }

   # convert date encoding (if needed) ----
   if(MMDD == TRUE){
      Temp <- data.frame(
         ID = seq(0,
                  length(Temp)-1),
         Date = paste(paste(substr(Temp, 6,7),
                            substr(Temp, 9, 10),
                            substr(Temp, 1,4), sep = "/"),
                      substr(Temp, 12,16), sep = "  ") ) }
   else {
                         Temp <- data.frame(ID = seq(0, length(Temp)-1),
                                            Date = paste(paste(substr(Temp, 9, 10),
                                                               substr(Temp, 6,7), substr(Temp, 1,4), sep = "/"),
                                                         substr(Temp, 12,16), sep = "  ") ) }

   # return results ----
   return(Temp)

}

