

#1. Toth B, Weynants M, Nemes A, Mak? A, Bilas G, T?th G. New generation of hydraulic pedotransfer functions for Europe. Eur J Soil Sci. 2015;66: 226-238. doi:10.1111/ejss.12192


#porosity
#'Internal function for determining the soil porosity from texture.
#'
#'If clay is present the function uses  Toth B, Weynants M, Nemes A, Mako A, Bilas G, Toth G. New generation of hydraulic pedotransfer functions for Europe. Eur J Soil Sci. 2015;66: 226-238
#'otherwise  Katterer T, Andren O, Jansson P-E. Pedotransfer functions for estimating plant available water and bulk density in Swedish agricultural soils. Acta Agric Scand Sect B - Plant Soil Sci. 2006;56: 263–276
#'
#' @author Lorenzo Menichetti \email{ilmenichetti@@gmail.com}
#'
#' @param sand sand %
#' @param clay clay % (optional)
#' @param SOC dSOC %
#'
#' @return a single numerical value with the soil porosity
#'
#' @export
poros <-
  function(sand, clay, SOC)
  { ... }


poros<-function(sand, clay, SOC){

  if(!missing(clay)){
    porosity<-0.4115+0.0409*SOC-0.6089*clay*sand-0.0031*SOC^2*clay+0.2276*clay^3}else{
    porosity<-0.3843+SOC*0.0448+SOC*sand*-0.0204
  }
  #K?tterer et al 2006
  return(porosity)
}


#'WP
#'Internal function for determining the soil wilting point
#'
#'uses  Toth B, Weynants M, Nemes A, Mak? A, Bilas G, T?th G. New generation of hydraulic pedotransfer functions for Europe. Eur J Soil Sci. 2015;66: 226-238. doi:10.1111/ejss.12192
#'
#' @author Lorenzo Menichetti \email{ilmenichetti@@gmail.com}
#'
#' @param sand sand %
#' @param clay clay % (optional)
#' @param SOC dSOC %
#'
#' @return a single numerical value with the soil wilting point
#'
#'
#' @export
WP <-
  function(sand, clay, SOC)
  { ... }

WP<-function(sand, clay, SOC){

  WP<-0.0086+0.4473*clay-0.0157*SOC*clay+0.0123*SOC*sand
  #K?tterer et al 2006
  return(WP)
}


#'FC
#' internal function for determining the soil field capacity, uses  Toth B, Weynants M, Nemes A, Mak? A, Bilas G, T?th G. New generation of hydraulic pedotransfer functions for Europe. Eur J Soil Sci. 2015;66: 226-238. doi:10.1111/ejss.12192
#'
#' @author Lorenzo Menichetti \email{ilmenichetti@@gmail.com}
#'
#' @param sand sand %
#' @param SOC dSOC %
#'
#' @return a single numerical value with the soil field capacity
#'
#'
#' @export
FC <-
  function(sand, SOC)
  { ... }

FC<-function(sand, SOC){

  FC<-0.4384-0.3839*sand+0.0796*SOC*sand
  #K?tterer et al 2006
  return(FC)
}



#'soiltemp
#' internal function for determining the soil temperature from the air temperature
#'
#' @author Lorenzo Menichetti \email{ilmenichetti@@gmail.com}
#'
#' @param L soil depth (in mm)
#' @param GAI ren area index daily values
#' @param date date vector (daily steps)
#' @param temperature (air temperature in degrees C)
#'
#' @return a vector with the daily soil temperature values
#'
#'
#' @export
soiltemp <-
  function(L, GAI, date, temperatur)
  { ... }
soiltemp<-function(L, GAI, date, temperature){

  #This functions comes from K?tterer and Andr?n (2008), where L=thickness of topsoil (mm), Zdepth is defined as the mean depth of the topsoil layer (cm), i., midpoint soil depth & and the relationship using LAI = 0.8 x GAI can be seen in Fig. 1 of this publication
  Zdepth=L/20;


  LAI=0.8*GAI;
  Tsurface<-c()
  soilT<-c()
  soilT[1]<-0
  for(i in 1:length(temperature)){
    if (temperature[i]<0) {Tsurface[i]=0.20*temperature[i]}
    if (temperature[i]>=0) {Tsurface[i]=temperature[i]*(0.95+0.05*exp(-0.4*(LAI[i]-3)))}
      soilT[i+1]=soilT[i] + (Tsurface[i] - soilT[i])*0.24*exp(-Zdepth*0.017)*exp(-0.15*GAI[i])
      }

  return(head(soilT,-1))
  }




#'waterbalance
#'Internal function for the water balance model
#' @author Lorenzo Menichetti \email{ilmenichetti@@gmail.com}
#'
#' @param twilt wilting point (0 to 1)
#' @param tfield field capacity (0 to 1)
#' @param precipitation daily precipitations (mm)
#' @param GAI gren area index daily values
#' @param date date vector
#' @param ET0 Evapotranspiration (calculated based on PET and GAI)
#' @param L soil depth (mm)
#'
#' @return a data frame with water balance and date (days)
#'
#' @examples
#'
#'
#' @export
waterbalance <-
  function(twilt, tfield, precipitation, GAI, date, ET0, L)
  { ... }

waterbalance<-function(twilt, tfield, precipitation, GAI, date, ET0, L){

  length_sim<-length(precipitation)

  alfa=0.7
  water<-c()
  actevapo<-c()
  bypass<-c() # this is the vector where to store the percolation

  water[1] = tfield*L #setting initial water content to max
    for (i in 1:length_sim){
      kc=1.3-0.5*exp(-0.17*GAI[i]);
      ETc=ET0[i]*kc;
      inter=min(precipitation[i],ETc,0.2*GAI[i]) #intercepted water
      Epot=(ETc-inter) #potential evapotraspiration

      #percolation option 1, direct percolation. Water is lost when above field capacity
        #if (water[i] >= tfield*L)  {water[i]=tfield*L} #percolation. If the water is more than soil total field capacity, water is lost
      #percolation option 2, saturation is allowed for one day
        bypass[i] = max(0, water[i]-(tfield*L)) #this is the water that will percolate the following day

      theta=water[i]/L;
      Kr=max(0,(1-(0.9*tfield-theta)/(0.9*tfield-alfa*twilt))^2);
      if (Kr>1){Kr=1}
      actevapo[i]=Epot*Kr #actual evapotranspiration
      water[i+1]=water[i]+precipitation[i]-actevapo[i]-inter-bypass[i]
    }
  #
  if(any(water<0)){
    cat("WARNING: some water content values are below zero, forcing them to zero...but have a look at the data just in case")
      water[water<0]=0}

  result<-data.frame(head(water,-1), date, actevapo)
  result$date<-as.Date(result$date)
  colnames(result)<-c("water", "date", "actevapo")
  return(result)
  }


#'re_temperature
#' internal function for determining the dependence of decompositono over soil temperature, sources from
#'
#' @author Lorenzo Menichetti \email{ilmenichetti@@gmail.com}
#'
#' @param soilT soil temperature daily values
#'
#' @return a vector with the daily water reduction values
#'
#'
#' @export
re_temperature <-
  function(soilT)
  { ... }

#dependence of decompositon over temperature, Katterer
re_temperature<-function(soilT){

  tmin=-3.78;
  re_temp<-c()
  for(i in 1:length(soilT)){
  re_temp[i]=(soilT[i]-tmin)^2/(30-tmin)^2
  }

  if(any(is.na(re_temp))){
    NAs<-which(is.na(re_temp))
    cat("WARNING- there are some NAs:", NAs, "
        Compensating by Stineman interpolation")
    water<-na.interpolation(water, option="stine")
  }
  re_temp[soilT<tmin]<-0
  return(re_temp)
}



#'re_water
#' internal function for determining the dependence of decompositono over soil moisture, sources from
#' Moyano FE, Manzoni S, Chenu C. Responses of soil heterotrophic respiration to moisture availability: An exploration of processes and models. Soil Biol Biochem. Elsevier Ltd; 2013;59: 72–85. doi:10.1016/j.soilbio.2013.01.002
#'
#' @author Lorenzo Menichetti \email{ilmenichetti@@gmail.com}
#'
#' @param twilt wilting point (0 to 1)
#' @param tfield field capacity (0 to 1)
#' @param water water balance (in mm)
#' @param porosity soil porosity /0 to 1)
#' @param L soil depth (in mm)
#'
#' @return a vector with the daily water reduction values
#'
#'
#' @export
re_water <-
  function(twilt, tfield, water, porosity, L)
  { ... }

#dependence of decompositono over soil moisture
re_water<-function(twilt, tfield, water, porosity, L){

  rs=0.5;
  topt = 0.2+1.26*tfield**2.03;
  tth= 0.0965*log(twilt) + 0.3;
  theta=water/L;
  re_wat<-c()
  for(i in 1:length(water)){
    if (theta[i]<tth) {re_wat[i]=0}; end;
    if (tth<=theta[i] & theta[i]<min(topt,tfield)){
      re_wat[i]=(theta[i]-tth)/(min(topt,tfield)-tth)}
    if (min(topt,tfield)<= theta[i] & theta[i] <= max(topt,tfield)) {
      re_wat[i]=1}
    if (theta[i] >max(topt,tfield)) {
      re_wat[i]=1-(1-rs)*(theta[i]-max(topt,tfield))/(porosity-max(topt,tfield))}
    }
  re_wat[re_wat>1]=1
  re_wat[re_wat<0]=0
  return(re_wat)
}



