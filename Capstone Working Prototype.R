#By David Woolf 500399541
rm(list = ls())
setwd("C:/Users/Dhwoo/Desktop/Coding Folder/Lapstone/Upload CSV")
#Intial setup and import.
data20052009 = read.csv("Upload 2005-2009.csv", header = TRUE, colClasses = c(Transit.Route = "character"))
data20062010 = read.csv("Upload 2006-2010.csv", header = TRUE, colClasses = c(Transit.Route = "character"))
data20072011 = read.csv("Upload 2007-2011.csv", header = TRUE, colClasses = c(Transit.Route = "character"))
data20082012 = read.csv("Upload 2008-2012.csv", header = TRUE, colClasses = c(Transit.Route = "character"))
data20092013 = read.csv("Upload 2009-2013.csv", header = TRUE, colClasses = c(Transit.Route = "character"))
data20102014 = read.csv("Upload 2010-2014.csv", header = TRUE, colClasses = c(Transit.Route = "character"))
data20112015 = read.csv("Upload 2011-2015.csv", header = TRUE, colClasses = c(Transit.Route = "character"))
data20122016 = read.csv("Upload 2012-2016.csv", header = TRUE, colClasses = c(Transit.Route = "character"))
data20132017 = read.csv("Upload 2013-2017.csv", header = TRUE, colClasses = c(Transit.Route = "character"))

library(dplyr)

df20052009 <- data.frame(data20052009)
df20062010 <- data.frame(data20062010)
df20072011 <- data.frame(data20072011)
df20082012 <- data.frame(data20082012)
df20092013 <- data.frame(data20092013)
df20102014 <- data.frame(data20102014)
df20112015 <- data.frame(data20112015)
df20122016 <- data.frame(data20122016)
df20132017 <- data.frame(data20132017)



dfc20052009<-df20052009[, c(17, 5:16)]
dfc20062010<-df20062010[, c(17, 5:16)]
dfc20072011<-df20072011[, c(17, 5:16)]
dfc20082012<-df20082012[, c(17, 5:16)]
dfc20092013<-df20092013[, c(17, 5:16)]
dfc20102014<-df20102014[, c(17, 5:16)]
dfc20112015<-df20112015[, c(17, 5:16)]
dfc20122016<-df20122016[, c(17, 5:16)]
dfc20132017<-df20132017[, c(17, 5:16)]

#Adding various documents together
FullRun1<-full_join(dfc20052009, dfc20062010, by = "Transit.Route")

FullRun2<-full_join(FullRun1, dfc20072011, by = "Transit.Route")

FullRun3<-full_join(FullRun2, dfc20082012, by = "Transit.Route")

FullRun4<-full_join(FullRun3, dfc20092013, by = "Transit.Route")

FullRun5<-full_join(FullRun4, dfc20102014, by = "Transit.Route")

FullRun6<-full_join(FullRun5, dfc20112015, by = "Transit.Route")

FullRun7<-full_join(FullRun6, dfc20122016, by = "Transit.Route")

FullRunPure<-full_join(FullRun7, dfc20132017, by = "Transit.Route")

FullRunPure<-as.data.frame(FullRunPure)

#Consolidating State Name A, County Name A, State Name B, County Name B.
SNA<-FullRunPure[, c(1, 2, 14, 26, 38, 50, 62, 74, 86, 98)]
CNA<-FullRunPure[, c(1, 3, 15, 27, 39, 51, 63, 75, 87, 99)]
SNB<-FullRunPure[, c(1, 4, 16, 28, 40, 52, 64, 76, 88, 100)]
CNB<-FullRunPure[, c(1, 5, 17, 29, 41, 53, 65, 77, 89, 101)]

FRT<-FullRunPure[, c(1, 6:13, 18:25, 30:37, 42:49, 54:61, 66:73, 78:85, 90:97, 102:109)]
FRT<-as.matrix(FRT)
FRT[is.na(FRT)]   <- ""

SNA<-as.matrix(SNA)
SNA[is.na(SNA)]   <- ""
dim(SNA)
#Pulling indices is easier in Excel, Go L column, use "=INDEX(C2:K2,MATCH(MAX(LEN(C2:K2)),LEN(C2:K2),0))"

write.csv(SNA, "SNAT.csv")

SNAI= read.csv("SNAT indexed.csv", header = TRUE)

SNAI<-as.matrix(SNAI)

#Rinse and repeat for CNA, SNB, CNB

CNA<-as.matrix(CNA)
CNA[is.na(CNA)]   <- ""

write.csv(CNA, "CNAT.csv")

CNAI = read.csv("CNAT indexed.csv", header = TRUE)


CNAI<-as.matrix(CNAI)


SNB<-as.matrix(SNB)
SNB[is.na(SNB)]   <- ""



write.csv(SNB, "SNBT.csv")

SNBI = read.csv("SNBT indexed.csv", header = TRUE)

SNBI<-as.matrix(SNBI)


CNB<-as.matrix(CNB)
CNB[is.na(CNB)]   <- ""

write.csv(CNB, "CNBT.csv")

CNBI = read.csv("CNBT indexed.csv", header = TRUE)


CNBI<-as.matrix(CNBI)
#Using Excel code above, we pull the longest word in each row, giving us 1 column that has correct name.
Triproute<-cbind(SNAI[,c(12)], CNAI[,c(12)], SNBI[,c(12)], CNBI[,c(12)])

FRTlabeled<-cbind(FRT[,c(1)],Triproute[,c(1:4)])
FRTDone<-cbind(FRTlabeled,FRT[,c(2:73)])

write.csv(FRTDone, "FRTDone.csv")

#Adding CII excel file to FRTDone.csv
#CC column becomes Match, formula (=0&0&C2), then after (=0&C88595)
#CD =INDEX(CII!$A$1:$L$3194,MATCH($CC2,CII!$L$1:$L$3194,0), 4) labeled Index2007
#CE =INDEX(CII!$A$1:$L$3194,MATCH($CC2,CII!$L$1:$L$3194,0), 5) labeled Index2012
#CF =INDEX(CII!$A$1:$L$3194,MATCH($CC2,CII!$L$1:$L$3194,0), 6) labeled Index2018

FRTCII = read.csv("FRT CII.csv", header = TRUE, colClasses = c(Transit = "character", Code.A =  "character", Code.B =  "character",  Match =  "character", Index2007 = "character", Index2012 = "character", Index2018 = "character"))

FRTCII<-as.data.frame(FRTCII)

#Reorganizing data, Tran
FRTCIIclean<-FRTCII[, c(2, 81, 4:8, 13, 15, 21, 23, 29, 31, 37, 39, 45, 47, 53, 55, 61, 63, 69, 71, 77, 79, 82:84)]
names(FRTCIIclean)<- c("Transit ID", "Code.A", "Code.B", "State.Name.A", "County.Name.A","State.Name.B", "County.Name.B", "Net0509", "Gross0509","Net0610", "Gross0610","Net0711", "Gross0711", "Net0812", "Gross0812", "Net0913", "Gross0913", "Net1014", "Gross1014", "Net1115", "Gross1115", "Net1216", "Gross1216", "Net1317", "Gross1317", "Index2007", "Index2012", "Index2018")

#Filtering out counties not in CII, 002013, 002060,002105,002164,002230, 002282, 006003, 008053,
#008057, 008079, 008111, 013265, 016025, 016033, 020071, 020083, 028055, 030011, 030033, 030037, 
#030069, 030079, 030103, 030107, 030109, 031005, 031007, 031009, 031075, 031085, 031103, 031113, 
#031115, 031117, 031165, 031171, 031183, 032009, 035021, 038007, 038083, 038087, 046017, 046075, 
#046095, 046137, 048033, 048155, 048261, 048263, 048269, 048301,048311, 048345, 048393, 048431, 
#048443,049009, 049031,

FRTfilt<-subset(FRTCIIclean, Code.A!="002013")  
FRTfilt<-subset(FRTfilt, Code.A!="002060")
FRTfilt<-subset(FRTfilt, Code.A!="002105")
FRTfilt<-subset(FRTfilt, Code.A!="002164")
FRTfilt<-subset(FRTfilt, Code.A!="002230")
FRTfilt<-subset(FRTfilt, Code.A!="002282")
FRTfilt<-subset(FRTfilt, Code.A!="006003")
FRTfilt<-subset(FRTfilt, Code.A!="008053")
FRTfilt<-subset(FRTfilt, Code.A!="008057")
FRTfilt<-subset(FRTfilt, Code.A!="008079")
FRTfilt<-subset(FRTfilt, Code.A!="008111")
FRTfilt<-subset(FRTfilt, Code.A!="013265")
FRTfilt<-subset(FRTfilt, Code.A!="016025")
FRTfilt<-subset(FRTfilt, Code.A!="016033")
FRTfilt<-subset(FRTfilt, Code.A!="020071")
FRTfilt<-subset(FRTfilt, Code.A!="020083")
FRTfilt<-subset(FRTfilt, Code.A!="028055")
FRTfilt<-subset(FRTfilt, Code.A!="030011")
FRTfilt<-subset(FRTfilt, Code.A!="030033")
FRTfilt<-subset(FRTfilt, Code.A!="030037")
FRTfilt<-subset(FRTfilt, Code.A!="030069")
FRTfilt<-subset(FRTfilt, Code.A!="030079")
FRTfilt<-subset(FRTfilt, Code.A!="030103")
FRTfilt<-subset(FRTfilt, Code.A!="030107")
FRTfilt<-subset(FRTfilt, Code.A!="030109")
FRTfilt<-subset(FRTfilt, Code.A!="031005")
FRTfilt<-subset(FRTfilt, Code.A!="031007")
FRTfilt<-subset(FRTfilt, Code.A!="031009")
FRTfilt<-subset(FRTfilt, Code.A!="031075")
FRTfilt<-subset(FRTfilt, Code.A!="031085")
FRTfilt<-subset(FRTfilt, Code.A!="031103")
FRTfilt<-subset(FRTfilt, Code.A!="031113")
FRTfilt<-subset(FRTfilt, Code.A!="031115")
FRTfilt<-subset(FRTfilt, Code.A!="031117")
FRTfilt<-subset(FRTfilt, Code.A!="031165")
FRTfilt<-subset(FRTfilt, Code.A!="031171")
FRTfilt<-subset(FRTfilt, Code.A!="031183")
FRTfilt<-subset(FRTfilt, Code.A!="032009")
FRTfilt<-subset(FRTfilt, Code.A!="038007")
FRTfilt<-subset(FRTfilt, Code.A!="038083")
FRTfilt<-subset(FRTfilt, Code.A!="038087")
FRTfilt<-subset(FRTfilt, Code.A!="046017")
FRTfilt<-subset(FRTfilt, Code.A!="046075")
FRTfilt<-subset(FRTfilt, Code.A!="046095")
FRTfilt<-subset(FRTfilt, Code.A!="046137")
FRTfilt<-subset(FRTfilt, Code.A!="048033")
FRTfilt<-subset(FRTfilt, Code.A!="048155")
FRTfilt<-subset(FRTfilt, Code.A!="048261")
FRTfilt<-subset(FRTfilt, Code.A!="048263")
FRTfilt<-subset(FRTfilt, Code.A!="048269")
FRTfilt<-subset(FRTfilt, Code.A!="048301")
FRTfilt<-subset(FRTfilt, Code.A!="048311")
FRTfilt<-subset(FRTfilt, Code.A!="048345")
FRTfilt<-subset(FRTfilt, Code.A!="048393")
FRTfilt<-subset(FRTfilt, Code.A!="048431")
FRTfilt<-subset(FRTfilt, Code.A!="048443")
FRTfilt<-subset(FRTfilt, Code.A!="049009")
FRTfilt<-subset(FRTfilt, Code.A!="049031")

#Now removing Puerto Rico to and from, as well immigration from non-states

FRTfilt<-subset(FRTfilt, State.Name.A!="Puerto Rico")
FRTfilt<-subset(FRTfilt, State.Name.B!="Puerto Rico")
FRTfilt<-subset(FRTfilt, State.Name.B!="Africa")
FRTfilt<-subset(FRTfilt, State.Name.B!="Asia")
FRTfilt<-subset(FRTfilt, State.Name.B!="Caribbean")
FRTfilt<-subset(FRTfilt, State.Name.B!="Central America")
FRTfilt<-subset(FRTfilt, State.Name.B!="Europe")
FRTfilt<-subset(FRTfilt, State.Name.B!="Northern America")
FRTfilt<-subset(FRTfilt, State.Name.B!="Soouth America")
FRTfilt<-subset(FRTfilt, State.Name.B!="U.S. Island Areas")
FRTfilt<-subset(FRTfilt, State.Name.B!="Oceania and At Sea")
FRTfilt<-subset(FRTfilt, County.Name.B!="-")

#Filtering to an individual State, in this case Alabama.

State1<-subset(FRTfilt, State.Name.A == "Alabama")

#Using gross for prediction.
State01<-State1[, c(1:7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 26:28)]

#Replacing NA values with 0s.

State01<-as.matrix(State01)
State01[is.na(State01)]   <- "0"
State01<-as.data.frame(State01)


#Converting Indices into numeric, in preparation for regression.

State01$Index2007 <- as.numeric(as.character(State01$Index2007))
State01$Index2012 <- as.numeric(as.character(State01$Index2012))
State01$Index2018 <- as.numeric(as.character(State01$Index2018))

#Training Alabama
train01<-lm(Index2012~Index2007 + Gross0509 +  Gross0610 + Gross0711+ Gross0812, data = State01)
summary(train01)

test01<-lm(Index2018~Index2012 + Gross0913 +  Gross1014 + Gross1115+ Gross1216 + Gross1317, data = State01)
summary(test01)


library(broom)
library(knitr)

train01tidy<-tidy(train01)
test01tidy<-tidy(test01)

kable(train01tidy)
kable(test01tidy)