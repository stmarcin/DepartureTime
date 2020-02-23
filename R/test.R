DepartureTime <- function(method = "H",
                          dy = format(Sys.Date(), "%Y"),
                          dm = format(Sys.Date(), "%m"),
                          dd = format(Sys.Date(), "%d"),
                          tmin = 0,
                          tmax = 24,
                          res = 5,
                          MMDD = TRUE,
                          ptw = FALSE)  {
      
      methods <- c("H", "Hybrid", "R", "Random", "S", "Systematic", "W", "ConstrainedWalk")

      if(!(method %in% methods) ) stop(paste("Invalid value to argument 'method'. It must be one of the following:\n  ",
            toString(paste0('"', methods, '"')) ))
}
      
dy <- 201

dy <- format(Sys.Date(), "%Y")

# (is.numeric(dy) & dy%%1==0) | format(Sys.Date(), "%Y")
dy <- "A"      



if(dy != format(Sys.Date(), "%Y"))  {
      
      if(!is.numeric(dy))  print("Argument 'dy' is not a year. Please provide a valid argument 'dy'")
      if(is.numeric(dy))  {
            if(dy%%1!=0)  print("Argument 'dy' is not a year. Please provide a valid argument 'dy'")
            if(dy%%1==0 & (dy < 2015 | dy > as.numeric(format(Sys.Date(), "%Y")))) print(paste("You have selected", dy, "as a year"))
            
      }
      
      
}



(if(is.numeric(dy))  {dy%%1==0}) == TRUE


if(if(is.numeric(dy))  {dy%%1==0}) 

      

DepartureTime(method = "A")     
