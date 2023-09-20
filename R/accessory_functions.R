

#add.years
#'
#'Extrapolates future climate
#'
#'This function extrapolate future climate based on the average of the past 10 years (or less if less are presente)
#'averaging day by day
#'
#' @author Lorenzo Menichetti \email{ilmenichetti@@gmail.com}
#'
#' @param dataframe the weather data frame
#' @param new.years the number of years you want to extend the weather data frame
#'
#' @return a table, same structure of \link{weather_testdata}
#'
#' @examples
#'
#'add.years(weather_testdata, 5)
#'
#' @export
add.years<-function(dataframe, new.years)
  {... }

#function to extrapolate missing weather dates from average
add.years<-function(dataframe, new.years){

  dataframe$date<-as.Date(as.character(dataframe$date))
  add_years<-new.years
  date<-as.Date(dataframe$date)
  last_years<-tail(unique(year(date)),10)
  date_vector<-seq(ymd(as.Date(tail(date,1)+1)), ymd(as.Date(paste(tail(last_years,1)+add_years,"-01-01", sep=""))), by = "day")

  newmat<-mat.or.vec((length(date_vector)-1),dim(dataframe)[2])
  colnames(newmat)<-colnames(dataframe)
  newmat<-as.data.frame(newmat)
  which_ones<-year(dataframe$date) %in% last_years

  for(i in 1:(length(date_vector)-1)){
    DATE_TODAY<-date_vector[i]
    which_days<-day(dataframe[which_ones,]$date)==day(DATE_TODAY)
    which_months<-month(dataframe[which_ones,]$date)==month(DATE_TODAY)
    which_months_day<-which_days & which_months
    newmat[i,]$date<-as.character(DATE_TODAY)
    newmat[i,]$year<-year(DATE_TODAY)
    newmat[i,]$month<-month(DATE_TODAY)
    newmat[i,]$day<-day(DATE_TODAY)
    newmat[i,]$air_temp_deg_C<-mean(dataframe[which_months_day,]$air_temp_deg_C)
    newmat[i,]$precipitation_mm<-mean(dataframe[which_months_day,]$precipitation_mm)
    newmat[i,]$windspeed<-mean(dataframe[which_months_day,]$windspeed)
    newmat[i,]$humidity<-mean(dataframe[which_months_day,]$humidity)
    newmat[i,]$Rsolar<-mean(dataframe[which_months_day,]$Rsolar)
  }

  newdataframe<-rbind(dataframe,newmat)

  return(newdataframe)
}






#fill.na
#'
#'Fill NAs in the data with linear interpolation
#'
#'This function fills NAs based on the average of the past 10 years (or less if less are presente)
#'averaging day by day
#'
#' @author Lorenzo Menichetti \email{ilmenichetti@@gmail.com}
#'
#' @param dataframe the weather data frame
#'
#' @return a data frame,, same structure of the input data frame
#'
#'
#' @export
fill.na<-function(dataframe)
{... }


#function to extrapolate missing weather dates from average
fill.na<-function(dataframe){

  dataframe$date<-as.Date(as.character(dataframe$date))

  dataframe.na<-dataframe
    dataframe.na$air_temp_deg_C<-na.approx(dataframe$air_temp_deg_C)
    dataframe.na$precipitation_mm<-na.approx(dataframe$precipitation_mm)
    dataframe.na$windspeed<-na.approx(dataframe$windspeed)
    dataframe.na$humidity<-na.approx(dataframe$humidity)
    dataframe.na$Rsolar<-na.approx(dataframe$Rsolar)

    which.na<-which(is.na(dataframe.na[,6:9]))
    if(length(which.na)>0){cat("impossible to interpolate, NAs in ", which.na,". you mighthave NAs in the initial or final values")}


  return(dataframe.na)
}







#is.leapyear
#'
#'Identifies if a year is leap or not
#'
#'Well... pretty simple one, main heading is self-explanatory
#'
#' @author Lorenzo Menichetti \email{ilmenichetti@@gmail.com}
#'
#' @param year a year
#'
#' @examples
#'
#'is.leapyear(1996)
#'
#' @export
is.leapyear<-function(year)
{... }

#load the function to indentify leap years
is.leapyear<-function(year){#http://en.wikipedia.org/wiki/Leap_year
  return(((year %% 4 == 0) & (year %% 100 != 0)) | (year %% 400 == 0))}
