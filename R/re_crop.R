

#1. Toth B, Weynants M, Nemes A, Mak? A, Bilas G, T?th G. New generation of hydraulic pedotransfer functions for Europe. Eur J Soil Sci. 2015;66: 226-238. doi:10.1111/ejss.12192
#porosity
poros<-function(sand, clay, SOC){

  if(!missing(clay)){
    porosity<-0.4115+0.0409*SOC-0.6089*clay*sand-0.0031*SOC^2*clay+0.2276*clay^3}else{
    porosity<-0.3843+SOC*0.0448+SOC*sand*-0.0204
  }
  #K?tterer et al 2006
  return(porosity)
}


WP<-function(sand, clay, SOC){

  WP<-0.0086+0.4473*clay-0.0157*SOC*clay+0.0123*SOC*sand
  #K?tterer et al 2006
  return(WP)
}


FC<-function(sand, SOC){

  WP<-0.4384-0.3839*sand+0.0796*SOC*sand
  #K?tterer et al 2006
  return(WP)
}


#soil temperature
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


#water balance model

waterbalance<-function(twilt, tfield, precipitation, GAI, date, ET0, L){

  length_sim<-length(precipitation)

  alfa=0.7
  water<-c()
  water[1] = tfield*L #setting initial water content to max
    for (i in 1:length_sim){
      kc=1.3-0.5*exp(-0.17*GAI[i]);
      ETc=ET0[i]*kc;
      inter=min(precipitation[i],ETc,0.2*GAI[i]) #intercepted water
      Epot=(ETc-inter) #potential evapotraspiration
        # theta = water[i]/L
        #if (theta >= tfield | theta==0)  {theta=tfield-0.000001}
        #percolation. Water is lost when above field capacity
        if (water[i] >= tfield*L)  {water[i]=tfield*L-0.000001} #percolation. If the water is more than soil total field capacity, water is lost
        theta=water[i]/L;
        Kr=max(0,(1-(0.9*tfield-theta)/(0.9*tfield-alfa*twilt))^2);
        if (Kr>1){Kr=1}
      actevapo=Epot*Kr #actual evapotranspiration
      water[i+1]=water[i]+precipitation[i]-actevapo-inter
    }
  #
  if(any(water<0)){
    cat("WARNING: some water content values are below zero, forcing them to zero...but have a look at the data just in case")
      water[water<0]=0}

  result<-data.frame(head(water,-1), date)
  result$date<-as.Date(result$date)
  colnames(result)<-c("water", "date")
  return(result)
  }



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


