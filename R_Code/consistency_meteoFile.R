SCRIPTPATH <-"c:\\Users\\LM\\Documents\\BROOK90\\Documentation\\Input_data"
meteoFile <-read.table(file.path(SCRIPTPATH,"data_input_WB_1999_point.txt"),dec=".")
#
SOLRAD <- c(meteoFile$V4)
TMAX <- c(meteoFile$V5)
TMIN <- c(meteoFile$V6)
EA <- c(meteoFile$V7)
UW <- c(meteoFile$V8)
PRECIN <- c(meteoFile$V9)
MESFL <- c(meteoFile$V10)
#
subcheckdata <- function(SOLRAD,TMAX,TMIN,EA,UW,PRECIN,MESFL){
  output <- TRUE
  if (any (SOLRAD < 0 | SOLRAD > 80)){
    output <- FALSE
  } else {
    #print("TRUE")
  }
  if (any (TMAX < -50 | TMAX > 50 | TMIN < -50 | TMIN > 50)){
    output <- FALSE
  } else {
    #print("TRUE")
  }
  if (any (TMAX < TMIN)){
    output <- FALSE
  } else {
    #print("TRUE")
  }
  if (any (EA < 0 | EA > 10)){
    output <- FALSE
  } else {
    #print("TRUE")
  }
  if (any (UW < 0 | UW > 50)){
    output <- FALSE
  } else {
    #print("TRUE")
  }
  if (any (PRECIN < 0 | PRECIN > 1000)){
    output <- FALSE
  } else {
    #print("TRUE")
  }
  if (any (MESFL < 0 | MESFL > 1000)){
    output <- FALSE
  } else {
    #print("TRUE")
  }
  return(output)
}
#
subcheckdata(SOLRAD,TMAX,TMIN,EA,UW,PRECIN,MESFL)
  