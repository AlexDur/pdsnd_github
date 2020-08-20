
library(ggplot2)

ny = read.csv('new_york_city.csv')
wash = read.csv('washington.csv')
chi = read.csv('chicago.csv')

head(ny)

head(wash)

head(chi)

library(ggplot2)

ny7=subset(ny,select = -c(8,9))
dim(ny7)

chi7=subset(chi,select = -c(8,9))
dim(chi7)

#Solution code
NyChiData= rbind(ny,chi)
AllFTrips= NyChiData$Trip.Duration[NyChiData$Gender == 'Female']

# To receive the mean Trip.Duration for women
FTrip= mean(NyChiData$Trip.Duration[NyChiData$Gender == 'Female'], na.rm = TRUE)
FTrip

# To receive the mean Trip.Duration for men
AllMTrips= NyChiData$Trip.Duration[NyChiData$Gender == 'Male']
MTrip= mean(NyChiData$Trip.Duration[NyChiData$Gender == 'Male'], na.rm = TRUE)
MTrip

#Data visualization

ggplot(aes(x=Trip.Duration, y=Gender), data = NyChiData) +
  geom_point(alpha=1/100, position=position_jitter(h=0), na.rm = TRUE)+
  scale_x_continuous(limits = c(0,10000), breaks=seq(0, 10000, 1000)) +
  ggtitle('Distribution of trips duration per sex in seconds')


summary(AllFTrips)
#Womens' trips lasted from 60.0 to 77928.9 seconds. On average, a trip lasted 863.3 seconds for women

summary(AllMTrips)
#Mens' trips lasted from 60.0 to 1088634.0 seconds. On average, a trip lasted 755.1 seconds for men

#The larger range for trip duration for men can also be seen in the diagram. The bar in black colour
#is larger for men than for women.

### Question 2

**What is the total travel time for users in different cities?**

# Your solution code goes here
AllData = rbind(ny7, chi7, wash)
getOption('max.print')

sum(ny$Trip.Duration)
sum(chi$Trip.Duration)
sum(wash$Trip.Duration)

#Total travel time in New York:    269.905.248
#269905248/60/60 = 74.973,68 hrs

#Total travel time in Chicago:     280.871.787
#280871787/60/60 = 78.019,94 hrs

#Total travel time in Washington:  371.183.985
#371183985/60/60 = 103.106,70 hrs


#The chart shows that by far most trips lasted almost 100.000 seconds. 
#The next larger category reached a duration of about 30.000 seconds.
ggplot(aes(x=AllData$Trip.Duration), data=AllData) +
  geom_histogram() +
  scale_x_continuous(name="Trips", limits = c(0,15000)) +
  scale_y_continuous(name="Duration in seconds", limits = c(0,100000))+
  ggtitle("Trips duration across the three cities")

summary(ny$Trip.Duration)
#NY users traveled on average 899.7 seconds
summary(chi$Trip.Duration)
#Chi user traveled on average 936.2 seconds
summary(wash$Trip.Duration)
#Wash users traveled on average 1237.3 seconds

#Total travel time for users in New York is    74.973,67hrs.
#Total travel time for users in Chicago is     78.019,94hrs.
#Total travel time for users in Washington is 103.106,70hrs

AgeAnalysis= rbind(ny,chi)
as.data.frame(AgeAnalysis)
#To receive the mean value for male customers
mean(AgeAnalysis$Birth.Year[AgeAnalysis$Gender == 'Male'], na.rm = TRUE)

#Data visualization

ggplot(aes(x=Birth.Year), data = AgeAnalysis) +
  geom_histogram(binwidth = 1) +
  scale_x_continuous(limits = c(1940,2005)) +
  ggtitle('General age structure for both sexes by birth year')

summary(AgeAnalysis$Birth.Year)
#1885 is indicated as the earliest birth year. 2016 is given as the latest birth year. The plot, however, is limited to a range from
#1940 to 2005 as outside of these boundaries, the occurrences are very low. 
#The average male customer was born in 1979 and is thus, 40-41 years old

system('python -m nbconvert Explore_bikeshare_data.ipynb')
