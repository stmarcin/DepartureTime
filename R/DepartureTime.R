#' @title Generate Departure times
#'
#' @description \code{DepartureTimes} generate .dbf file with departure times for accessibility analyses with temporal resolution (e.g GTFS)
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
#' @param path String: path where the output will be saved (absolute or relative paths availables);
#'
#' \strong{default: working directory}
#'
#' @param file String: name of the \code{.dbf} file, where departure times will be saved;
#'
#' \strong{default: "DepTime"}
#'
#' @examples
#' \dontrun{
#' DepartureTime()
#'
#' DepartureTime(method = "S",       # systematic sampling method
#'  dm = 5, dd = 15,                 # user-defined date: 15th May, 2019 (current year)
#'  tmin = 7, tmax = 10,             # user-defined time window (07:00 - 10:00)
#'  res = 15,                        # user-defined temporal resolution (15 minutes)
#'  path = "Data/StartTime",         # user-defined relavive path and file name (next line)
#'  file = "DepTime_H15_0710")}
#'
#' @export
DepartureTime <- function(method = "H",
                          dy = format(Sys.Date(), "%Y"),
                          dm = format(Sys.Date(), "%m"),
                          dd = format(Sys.Date(), "%d"),
                          tmin = 0,
                          tmax = 24,
                          res = 5,
                          MMDD = TRUE,
                          ptw = FALSE,
                          path = getwd(),
                          file = "DepTime") {


      # if (!requireNamespace("foreign", quietly = TRUE)) {
      #       stop("Package \"foreign\" needed for this function to work. Please install it.",
      #            call. = FALSE)
      # }

      # create data.frame
      Temp <- data.frame(Date = seq(from = as.POSIXct(paste(dy,dm,dd, sep = "-")) + as.difftime(tmin, units="hours"),
                                    to = as.POSIXct(paste(dy,dm,dd, sep = "-")) + as.difftime(tmax, units="hours"),
                                    by = 60))

      # hybrid sampling method
      if(method == "H" | method == "Hybrid") {
            # select departure times
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
            rm(n, i, row_max, fixed_rows, random_rows)
      }


      # simple random method
      if(method == "R" |  method == "Random") {
            # random_rows <- sample(1 : nrow(Temp), nrow(Temp)/res)
            Temp <- sort(Temp[sample(1 : nrow(Temp), nrow(Temp)/res), ])
      }

      # systematic method
      if(method == "S" |  method == "Systematic") {
            Temp <- Temp[seq(1, nrow(Temp), res),]
      }

      # Constrained Random Walk Sampling
      if(method == "W" |  method == "ConstrainedWalk") {
            random_rows <- sample(1:(res), 1)   # select first departure time

            while(utils::tail(random_rows, n=1) <= nrow(Temp)) {
                  random_rows <- c(random_rows,
                                   sample((round(utils::tail(random_rows, n=1) + res*0.5, 0)):
                                                (round(utils::tail(random_rows, n=1) + res*1.5, 0)), 1) ) }
            random_rows <- random_rows[-length(random_rows)] # remove departure time out of time-window
            Temp <- Temp[random_rows,]
      }

      if(MMDD == TRUE){
            Temp <- data.frame(ID = seq(0, length(Temp)-1),
                               Date = paste(paste(substr(Temp, 6,7), substr(Temp, 9, 10), substr(Temp, 1,4), sep = "/"),
                                            substr(Temp, 12,16), sep = "  ") ) } else {
                                                  Temp <- data.frame(ID = seq(0, length(Temp)-1),
                                                                     Date = paste(paste(substr(Temp, 9, 10), substr(Temp, 6,7), substr(Temp, 1,4), sep = "/"),
                                                                                  substr(Temp, 12,16), sep = "  ") ) }
      # save output
      foreign::write.dbf(Temp, paste(path, paste0(file, ".dbf"), sep = "/"))

}
