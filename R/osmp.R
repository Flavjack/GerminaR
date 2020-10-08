#' Osmotic potential calculator
#'
#' @description Function to calculate the grams of salt or PEG-6000 needed for determinated osmotic potential 
#' @param type Salt or PEG-6000 c("salt", "peg6000"). Default: "salt".
#' @param vol volume (liters)
#' @param pres Pressure (Mpa) in negative values. 1 bar = 0.1 Mpa
#' @param temp Temperature (centigrade)
#' @param mw Molecular weight
#' @param ki Salt dissociation constant (NaCl = 1.8)
#' @return Numeric value (grams)
#' @export

osmp <- function(type = "salt", vol, pres, temp, mw, ki){
  
  if( type == "salt"){
    
    r1 <- (-0.00820574587 * (temp + 273.15) * ki)
    
    r2 <- pres/r1
    
    r3 <- r2 * 1000
    
    r4 <- (mw * r3 * vol * 1000)/10^6
    
    round(r4, 5)
    
  } else if (type == "peg6000"){
    
    a <- (8.39*10^(-7)*temp - 1.18*10^(-4))
    
    b <- (2.67*10^(-4)*temp - 1.18*10^(-2))
    
    c <- -pres*10
    
    dlt <- b^(2)-(4*a*c)
    
    x <- (-b - sqrt(dlt))/(2*a)
    
    round(x*vol, 5)  
    
  }
  
}

