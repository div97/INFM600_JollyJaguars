# Title: R Script Draft
# Last Modified: 11/17/2016
# Description: The commands in the script can be used on any sample of our dataset to analyse our research questions.

# Loading the dataset containing about 2 million records into R
mydata=read.csv('Final Subset Data CSV.csv')

 
# Creating a random subset of 50000 rows for our analysis. 
# This new subset is stored in 'subsetcalls' variable
subsetcalls <- mydata[sample(1:nrow(mydata), 50000, replace=FALSE),]

# load the xlsx package.
library(xlsx)

# Write the subset into an Excel file for sharing with all the team members
write.xlsx(x = subsetcalls.dataframe, file = "test.excelfile.xlsx",sheetName = "TestSheet", row.names = FALSE)

# Load the sqldf library. This package provides an easy way to perform SQL selects on R data frames
library(sqldf)

# This command gives us the list of all the unique description values in our sample
distinctDescriptionValues <- sqldf('SELECT DISTINCT subsetcalls.Description FROM subsetcalls')

############################################################################################################################################################
#For Question 1: Are more 911 calls related to burglary made during the night? Or is that just a preconceived notion? 
#Independent Variable - Time of the Day (Since we are categorizing the time of the day into day and night, it essentially is a norminal variable with two levels)
#Dependent Variable - 911 calls related to burglary (Nominal variable)
# This command gives us a count of all the different types of burglary descriptions given in the list
allburglary<-sqldf('SELECT DISTINCT subsetcalls.Description, COUNT(Description) 
  FROM subsetcalls 
 WHERE Description in 
("ATTEMP BURGLARY", "BURGLARPY", "Burglary", "BURGLARY", "POSS BURGLARY", "POSS STOLEN", "Robbery Armed", "ROBBERY ARMED", "Robbery Unarmed", "ROBBERY UNARMED") 
GROUP BY Description')

# Replacing all the Burglary related descriptions with a common label 'BURGLARY'
subsetcalls$Description[subsetcalls$Description="ATTEMP BURGLARY"]<-"BURGLARY"
subsetcalls$Description[subsetcalls$Description="BURGLARPY"]<-"BURGLARY"
subsetcalls$Description[subsetcalls$Description="Burglary"]<-"BURGLARY"
subsetcalls$Description[subsetcalls$Description="POSS BURGLARY"]<-"BURGLARY"
subsetcalls$Description[subsetcalls$Description="POSS STOLEN"]<-"BURGLARY"
subsetcalls$Description[subsetcalls$Description="Robbery Armed"]<-"BURGLARY"
subsetcalls$Description[subsetcalls$Description="ROBBERY ARMED"]<-"BURGLARY"
subsetcalls$Description[subsetcalls$Description="Robbery Unarmed"]<-"BURGLARY"
subsetcalls$Description[subsetcalls$Description="ROBBERY UNARMED"]<-"BURGLARY"

# Calculating the count of Descriptions with label 'BURGLARY'
finalburglary<-sqldf('SELECT subsetcalls.Description, COUNT(Description) FROM subsetcalls WHERE Description = "BURGLARY"')

# Fetching the results of Descriptions with label 'BURGLARY' and the 'CallDateTime' column 
finalburglary<-sqldf('SELECT subsetcalls.Description, subsetcalls.CallDateTime FROM subsetcalls WHERE Description = "BURGLARY"')

# Ordering the records by CallDateTime
sortedburglary<-sqldf('SELECT finalburglary.Description, finalburglary.CallDateTime FROM finalburglary order by finalburglary.CallDateTime')

# Selecting the records that have CallDateTime values for nighttime from 8 pm to 5 am  
burglaryatnight<-sqldf('SELECT finalburglary.Description, finalburglary.CallDateTime FROM finalburglary where ((CallDateTime >= "20:00:00") or (CallDateTime <= "5:00:00")) order by finalburglary.CallDateTime')

# Selecting the records that have CallDateTime values for daytime from 5 am to 8 pm
burglaryatday<-sqldf('SELECT finalburglary.Description, finalburglary.CallDateTime FROM finalburglary where (CallDateTime > "5:00:00" and CallDateTime < "20:00:00")  order by finalburglary.CallDateTime')

############################################################################################################################################################
# Question 2: Whether the number of blank calls increase/decrease with the arrival of the weekend? 
#Independent Variable = The day of the week (Since we are categorizing the days of the week into weekdays and weekend, it essentially is a norminal variable with two levels)
#Dependent Variable = 911 calls related to no voice and hangups (Nominal Variable)
# This command gives us a count of all the different types of blank call descriptions given in the list
allblankcalls<-sqldf('SELECT DISTINCT subsetcalls.Description, COUNT(Description) 
FROM subsetcalls 
WHERE Description in 
("911/HANGUP", "911/HANGUP.", "911/NO  VOICE", "911/NO Voice") 
 GROUP BY Description')

# Replacing all the No VOICE related descriptions with a common label 'NO_VOICE'
# Replacing all the HANGUP related descriptions with a common label 'HANGUP'
subsetcalls$Description[subsetcalls$Description="911/HANGUP"]<-"HANGUP"  
subsetcalls$Description[subsetcalls$Description="911/HANGUP."]<-"HANGUP"
subsetcalls$Description[subsetcalls$Description="911/NO  VOICE"]<-"NO_VOICE"
subsetcalls$Description[subsetcalls$Description="911/NO Voice"]<-"NO_VOICE"

#Gives a count of all the different types of "NO_VOICE" and "HANGUP" call descriptions
sqldf('SELECT subsetcalls.Description, COUNT(Description) FROM subsetcalls WHERE Description = "NO_VOICE"')
sqldf('SELECT subsetcalls.Description, COUNT(Description) FROM subsetcalls WHERE Description = "HANGUP"')

# Extracting the date part from 'CallDateTime' column. 
subsetcalls$CallDateTime = format(as.POSIXct(subsetcalls$CallDateTime,format='%m/%d/%Y'),format='%m/%d/%Y')


# The date part is in factor format. So converting it into date format
subsetcalls$CallDateTime =as.Date(subsetcalls$CallDateTime, format = '%m/%d/%Y')

# Loading the timeDate package for using the isWeekend function
library(timeDate)

# is Weekend tells us whether the date is a weekend or not. It considers the days 'Saturday' and 'Sunday' as a weekend
# Returns True for weekends and False for weekdays
subsetcalls$CallDateTime = isWeekend(subsetcalls$CallDateTime)

##########################################################################################################################################################
#Question 3: Find the top 5 dangerous reasons for calls (e.g. Aggravated assault). Plot them based on the locations.
#Independent Variable = Location (Nominal Variable)
#Dependent Variable = Top 5 dangerous 911 calls (Nominal Variable)

distinctDescriptionValues<-sqldf('SELECT DISTINCT subsetcalls.Description FROM subsetcalls')

# Replacing all the assault descriptions with a common label 'ASSAULT'
subsetcalls$Description[subsetcalls$Description="COMMON ASSAULT"]<-"Assault"
subsetcalls$Description[subsetcalls$Description="Common Assault"]<-"Assault"
subsetcalls$Description[subsetcalls$Description="AGGRAV ASSAULT"]<-"Assault"
subsetcalls$Description[subsetcalls$Description="Aggrav Assault"]<-"Assault"

# Replacing all the destruction of property descriptions with a common label 'DESTRUCTION OF PROP'
subsetcalls$Description[subsetcalls$Description="DESTRUCT PROP"]<-"Destruction of property"
subsetcalls$Description[subsetcalls$Description="DESTRUCT PROPTY"]<-"Destruction of property"
subsetcalls$Description[subsetcalls$Description="Destruct Propty"]<-"Destruction of property"

# Replacing all the firearm related descriptions with a common label 'Discharge Firearm/Shooting'
subsetcalls$Description[subsetcalls$Description="DISCHRG FIREARM"]<-"Discharge Firearm/Shooting"
subsetcalls$Description[subsetcalls$Description="Dischrg Firearm"]<-"Discharge Firearm/Shooting"
subsetcalls$Description[subsetcalls$Description="SHOOTING"]<-"Discharge Firearm/Shooting"
subsetcalls$Description[subsetcalls$Description="Shooting"]<-"Discharge Firearm/Shooting"

# Replacing all the narcotics related descriptions with a common label 'Narcotics'
subsetcalls$Description[subsetcalls$Description="NARCOTICS INSIDE"]<-"Narcotics"
subsetcalls$Description[subsetcalls$Description="Narcotics Inside"]<-"Narcotics"
subsetcalls$Description[subsetcalls$Description="NARCOTICSOutside"]<-"Narcotics"
subsetcalls$Description[subsetcalls$Description="NarcoticsOutside"]<-"Narcotics"
subsetcalls$Description[subsetcalls$Description="NARCOTICS ONVIEW"]<-"Narcotics"
subsetcalls$Description[subsetcalls$Description="Narcotics OnView"]<-"Narcotics"

# Replacing all the robbery related descriptions with a common label 'Roobbery'
subsetcalls$Description[subsetcalls$Description="ROBBERY ARMED"]<-"Robbery"
subsetcalls$Description[subsetcalls$Description="Robbery Armed"]<-"Robbery"






