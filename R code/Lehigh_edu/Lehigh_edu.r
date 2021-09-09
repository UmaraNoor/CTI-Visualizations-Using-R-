setwd("E:/Projects/STIX/CSV Response/Rev 5/")
temp_abuse_ch <- read.csv("Lehigh_edu_2019-04-01_16_24_02_430000.csv",stringsAsFactors=FALSE)
abuse_ch <- subset(temp_abuse_ch,select = c(Indicator.Title,TTP.Type,TTP.Name,Domain,Received.Time))
abuse_ch$Indicator.Title <- as.factor(abuse_ch$Indicator.Title)
abuse_ch$Indicator.Types <- as.factor(abuse_ch$Indicator.Types)
abuse_ch$Observable.Type <- as.factor(abuse_ch$Observable.Type)
abuse_ch$TTP.Type <- as.factor(abuse_ch$TTP.Type)
abuse_ch$TTP.Name <- as.factor(abuse_ch$TTP.Name)

