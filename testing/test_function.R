library(re)

Test_reclim_out<-reclim(weather=weather_testdata,
                        aboveground=aboveground_testdata,
                        latitude=58,
                        altitude=83,
                        sand=8,
                        clay=43,
                        ave_SOC=1.9,
                        depth=20,
                        sun.mode="Rsolar")

# weather=weather_testdata
# aboveground=aboveground_testdata
# latitude=58
# altitude=83
# sand=8
# clay=43
# ave_SOC=1.9
# depth=20
# sun.mode="Rsolar"



str(Test_reclim_out$GAI)

crop_id_used<-unique(Test_reclim_out$crop_id)
palette_crop_id_used<-brewer.pal(length(crop_id_used), "Dark2")

plot(Test_reclim_out$PET$date, Test_reclim_out$GAI[2,], type="l", ylim=c(0, max(Test_reclim_out$GAI[2,])*1.3), xlab="date", ylab="GAI")

#coloring the output based on the crop
for(i in 1:length(crop_id_used)){
  which_ones<-which(!Test_reclim_out$crop_id==crop_id_used[i])
  GAI_crop<-Test_reclim_out$GAI[2,]
  GAI_crop[which_ones]<-NA
  lines(Test_reclim_out$PET$date, GAI_crop, col=palette_crop_id_used[i])
}
legend("topright", as.character(crop_id_used), col=palette_crop_id_used, bty="n", lty=1)



#calculate annual re-clim values
Test_annual<-reclim_annual(Test_reclim_out$results_daily)
where_re_crop<-grepl("re_crop.", as.character(colnames(Test_annual)))
Test_annual_re_crop<-(Test_annual[where_re_crop])


plot(Test_reclim_out$PET$date, Test_reclim_out$re_crop[1,], type="l",xlab="date", ylab="r", main="First treatment (CONVENTIONAL)")
plot(Test_annual_re_crop$re_crop_treat.CONVENTIONAL, type="l",xlab="year", ylab="r", main="First treatment (CONVENTIONAL)")
