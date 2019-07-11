
#function to extrapolate missing weather dates from average
add.years<-function(dataframe, new.years){

  dataframe$date<-as.Date(dataframe$date)
  add_years<-new.years
  date<-as.Date(dataframe$date)
  last_years<-tail(unique(year(date)),10)
  date_vector<-seq(ymd(as.Date(tail(date,1)+1)), ymd(as.Date(paste(tail(last_years,1)+add_years,"-01-01", sep=""))), by = "day")

  newmat<-mat.or.vec((length(date_vector)-1),dim(dataframe)[2])
  colnames(newmat)<-colnames(dataframe)
  newmat<-as.data.frame(newmat)
  which_ones<-year(dataframe$date) %in% last_years

  for(i in 1:(length(date_vector)-1)){
    DATE_TODAY<-as.Date(date_vector[i])
    which_days<-day(dataframe[which_ones,]$date)==day(DATE_TODAY)
    which_months<-month(dataframe[which_ones,]$date)==month(DATE_TODAY)
    which_months_day<-which_days & which_months
    newmat[i,]$date<-DATE_TODAY
    newmat[i,]$year<-year(DATE_TODAY)
    newmat[i,]$month<-month(DATE_TODAY)
    newmat[i,]$day<-day(DATE_TODAY)
    newmat[i,]$air_temp_deg_C<-mean(dataframe[which_months_day,]$air_temp_deg_C)
    newmat[i,]$precipitation_mm<-mean(dataframe[which_months_day,]$precipitation_mm)
    newmat[i,]$windspeed<-mean(dataframe[which_months_day,]$windspeed)
    newmat[i,]$humidity<-mean(dataframe[which_months_day,]$humidity)
    newmat[i,]$Rsolar<-mean(dataframe[which_months_day,]$Rsolar)
    newmat[,1]<-as.Date(newmat[,1])
  }

  newdataframe<-rbind(dataframe,newmat)

  return(newdataframe)
}
