
#Kr

tfield=0.9
twilt=0.2
alpha=0.7
Theta=seq(0, 1, by=0.01)

Kr<-c()
Kr2<-c()
for(i in 1:length(Theta)){
  Kr[i]=(1-(0.9*tfield-Theta[i])/(0.9*tfield-alpha*twilt))^2
}

Kr[which(Kr>=1)]=1
Kr2=Kr
Kr2[1:which(Kr2==0)]=0

png(filename="./man/figures/Kr_function.png", width = 600, height = 500)
plot(Theta, Kr, type="l")
abline(v=0.2, col="violet")
lines(Theta, Kr2, col="red", lty=2)
dev.off()


#GAI
data(aboveground_testdata)

selected_aboveground<-aboveground_testdata[aboveground_testdata$treat=="CONVENTIONAL",]

GAI_test<-GAI(yield=selected_aboveground$total_dm_kg_ha, crop=selected_aboveground$crop_id, year=selected_aboveground$year, variance=selected_aboveground$variance,
  seeding=selected_aboveground$seeding, harvest=selected_aboveground$harvest, tillage=selected_aboveground$tillage, minimum_cover=selected_aboveground$minimum_cover)

plot(GAI_test$date, GAI_test$GAI, type="l")
lines(GAI_test$date, GAI_test$LAI, type="l", lty=2)

selected_aboveground<-selected_aboveground[4,]
GAI_test<-GAI(yield=selected_aboveground$total_dm_kg_ha, crop=selected_aboveground$crop_id, year=selected_aboveground$year, variance=selected_aboveground$variance,
              seeding=selected_aboveground$seeding, harvest=selected_aboveground$harvest, tillage=selected_aboveground$tillage, minimum_cover=selected_aboveground$minimum_cover)

png(filename="./man/figures/GAI_function.png", width = 600, height = 500)
plot(GAI_test$date, GAI_test$GAI, type="l", xlab="date", ylab="GAI", main="Example of GAI calculation, spring small cereal")
abline(v=GAI_test$date[selected_aboveground$seeding], col="darkgreen", lty=4)
text( GAI_test$date[selected_aboveground$seeding]-10, 0.4,"seeding",srt=90,pos=1, col="darkgreen")
abline(v=GAI_test$date[selected_aboveground$tillage], col="brown", lty=3)
text( GAI_test$date[selected_aboveground$tillage]-10, 0.4,"tillage",srt=90,pos=1, col="brown")
abline(v=GAI_test$date[selected_aboveground$harvest], col="red", lty=2)
text( GAI_test$date[selected_aboveground$harvest]-10, 0.4,"harvest",srt=90,pos=1, col="red")
dev.off()

names(GAI_test)

