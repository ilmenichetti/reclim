

# ICBM accessory functions
# GAI function
# author: Lorenzo Menichetti, on specifications from Martin Bolinder
# Year 2018, August


#load the function to indentify leap years
is.leapyear<-function(year){#http://en.wikipedia.org/wiki/Leap_year
  return(((year %% 4 == 0) & (year %% 100 != 0)) | (year %% 400 == 0))}



#GAI functions with GAImax values calculated from aboveground NPP
GAI<-function(yield, crop, year, variance, seeding, harvest, tillage, minimum_cover, yield2=NULL, harvest2=NULL){


  GAI_list<-list()
  crop_list<-c("spring_small_grains", "spring_oil_seeds","winter_small_grains", "winter_oil_seeds","root_crop")
  # exception crops: "fodder", "missing", "ley"

  for(j in 1:length(yield)){

    ###checking if the year is a leap year
    if(is.leapyear(year[j])){day=seq(from=1, to=366, by=1)}else{day=seq(from=1, to=365, by=1)}

    ###The GAI function core when the crops are in the list
    if(crop[j] %in% crop_list){
      GAImax=0.0129*(yield[j]/1000)^2 + 0.1964*yield[j]/1000;
      if(crop[j]=="root_crop"){GAImax=5.6}
      middle=seeding[j]+(harvest[j]-seeding[j])/2;
      GAI=GAImax*exp(-((day-middle)^2)/(2*variance[j]));
      day_max<-which(GAI==max(GAI))
      GAI[day<seeding[j]]<-minimum_cover[j]
      #the following is a filling between the forced minimum for each crop and the day when such min is reached
      GAI[day<day_max][GAI[day<day_max]<minimum_cover[j]]<-minimum_cover[j]

      if(crop[j]=="root_crop"){ GAI[day>harvest[j]]=0} #root crops at harvest lose all the aboveground product..
      # GAI[day>harvest[j]]=max(GAI)*0.2 #all crops at harvest lose most AG product, but retains 20% until tillage[j]. Modification by Lorenzo Menichetti
      # GAI[day>tillage[j]]=0
    }else if(crop[j] == "fodder"){
      #The GAI function core in the case of fodder, such as silage maize, it also loses all the biomass at harves
      GAImax=min(10,0.0018*yield[j]);
      GAI<-c()
      for(i in 1:length(day)){
        if (day[i]>seeding[j]){GAI[i]=(GAImax)/(1+exp(-((day[i]-seeding[j])-(harvest[j]-seeding[j])/2)/10))}
        if (day[i]<=seeding[j]){GAI[i]=0}
        if (day[i]>harvest[j]){GAI[i]<-0}
        GAI[day<day_max][GAI[day<day_max]<minimum_cover[j]]<-minimum_cover[j]
        # if (day[i]>harvest[j]){GAI[i]<-max(GAI)*0.2}
        # if (day[i]>tillage){GAI[i]=0}
      }
    }else if (crop[j]=='ley'){

        if(exists("harvest2") & harvest2[j]>0){ # in case there is a second harvest
          GAI<-rep(minimum_cover[j], length(day))
          GAImax1=min(10,0.0018*yield[j])
          GAImax2=min(10,0.0018*yield2[j])
          for(i in 1:length(day)){
            if(day[i]<harvest[j]){
              GAI[i]=(GAImax)/(1+exp(-((day[i]-seeding[j])-(harvest[j]-seeding[j])/2)/10))}
          }
          GAI_production<-GAI[seeding[j]:length(day)]
          GAI[harvest[j]:harvest2[j]]<-GAI_production[1:((harvest2[j]-harvest[j])+1)]

            }else { #in case harvest2 does not exist
              GAI<-c()
              GAImax=min(10,0.0018*yield[j])
              for(i in 1:length(day)){
                if (day[i]>seeding[j]){GAI[i]=(GAImax)/(1+exp(-((day[i]-seeding[j])-(harvest[j]-seeding[j])/2)/10))}
                if (day[i]<=seeding[j]){GAI[i]=0}
                if (day[i]>harvest[j]){GAI[i]<-0}
              }
              GAI[day<seeding[j]]<-minimum_cover[j]
              GAI[day<day_max][GAI[day<day_max]<minimum_cover[j]]<-minimum_cover[j]
          }# end of the environment where harvest2 does not exist

    } else if (crop[j]=='missing'){
      for(i in 1:length(day)){
        GAI<-c()
        GAI[i]<-0
      }
    }
    else{stop(paste(" position",j, ", crop:",crop[j],"
                   There are values in the crop list not following the specifications
                   Only admissible values are:
                   -NA
                   -spring_small_grains
                   -spring_oil_seeds
                   -winter_small_grains
                   -winter_oil_seeds
                   -root_crop
                   -fodder
                   -ley"))
    }

    GAI_date<-as.Date(x = integer(0), origin = paste(year[j],"-01-01", sep=""))
    for(i in 1:length(day)){GAI_date[i]<-as.Date(day[i]-1, origin = paste(year[j],"-01-01", sep=""))}
    #create a long list of repreated crops and yields
    crop_long<-rep(crop[j], length(day))
    yield_long<-rep(0, length(day))
    yield_long[j]<-yield[j]
    GAI_list[[j]]<-data.frame(GAI,GAI_date, crop_long, yield_long)
    }


  GAI_long<-c()
  GAI_date_long<-c()
  GAI_yield_long<-c()
  GAI_crop_long<-c()
  for(i in 1:length(GAI_list)){
    GAI_long<-append(GAI_long, GAI_list[[i]][,1])
    GAI_date_long<-append(GAI_date_long, GAI_list[[i]][,2])
    GAI_yield_long<-append(GAI_yield_long, GAI_list[[i]][,4])
    GAI_crop_long<-append(GAI_crop_long, as.character(GAI_list[[i]][,3]))
  }
  GAI_DF<-data.frame(GAI_date_long, GAI_long, GAI_crop_long, GAI_yield_long)
  colnames(GAI_DF)<-c("date","GAI", "crop", "yield_at_harvest")

  return(GAI_DF)
  }



