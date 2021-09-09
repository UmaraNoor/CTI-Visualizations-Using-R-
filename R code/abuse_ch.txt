setwd("E:/Projects/STIX/CSV Response/Rev 5/")
abuse_ch <- read.csv("Abuse_ch_2019-04-01_15_29_51_976001.csv",stringsAsFactors=FALSE)
abuse_ch$Indicator.Title <- as.factor(abuse_ch$Indicator.Title)
abuse_ch$Indicator.Types <- as.factor(abuse_ch$Indicator.Types)
abuse_ch$Observable.Type <- as.factor(abuse_ch$Observable.Type)
abuse_ch$TTP.Type <- as.factor(abuse_ch$TTP.Type)
abuse_ch$TTP.Name <- as.factor(abuse_ch$TTP.Name)
abuse_ch$URI.Type <- as.factor(abuse_ch$URI.Type)
abuse_ch$IP.Category <- as.factor(abuse_ch$IP.Category)
abuse_ch$IP.isSource <- as.factor(abuse_ch$IP.isSource)
abuse_ch$File.Format <- as.factor(abuse_ch$File.Format)
abuse_ch$File.Hash.Type <- as.factor(abuse_ch$File.Hash.Type)
abuse_ch$Producer.Name <- as.factor(abuse_ch$Producer.Name)


abuse_ch %>% group_by(Indicator.Title,TTP.Name) %>%
summarize(n=n()) %>%
ggplot(.,aes(TTP.Name,n,fill=Indicator.Title)) + 
geom_bar(stat="identity",position="dodge") + 
theme(axis.text.x = element_text(angle = 90, hjust = 1))

############### Frequent Domains with TTP
onlyDomain <- abuse_ch[abuse_ch$Domain!='',]
freqDomain <- onlyDomain %>% group_by(Domain) %>% filter(n()>1)
# 305 rows
freqDomain <- freqDomain[freqDomain$TTP.Name!='',]
# 71 rows
ggplot(freqDomain,aes(x=Domain,fill=TTP.Name))+theme(axis.text.x = element_text(angle = 90, hjust = 1))+ geom_bar()
export_freqDomain <- freqDomain %>% select(Package.Id,Indicator.Title,Observable.Type,Indicator.Types,TTP.Name,Domain,Domain.isFQDN,URI,URI.Type,File,File.Hash,File.Format,File.Hash.Type,Producer.Name,Indicator.Description,Observable.Description,TTP.Description,Received.Time,New_Received_Time,Day,Hour)

############### Frequent IPs with TTP
onlyIP <- abuse_ch[abuse_ch$IP!='',]
freqIP <- onlyIP %>% group_by(IP) %>% filter(n()>1)
# 376 rows
freqIP <- freqIP[freqIP$TTP.Name!='',]
# 57 rows
ggplot(freqIP,aes(x=IP,fill=TTP.Name))+theme(axis.text.x = element_text(angle = 90, hjust = 1))+ geom_bar()
# Convert time
freqIP <- freqIP %>% mutate(New_Received_Time = ymd_hms(Received.Time))
# Get Day
freqIP <- freqIP %>% mutate(Day = wday(New_Received_Time,label=T))
# Get Hour
freqIP <- freqIP %>% mutate(Hour = lubridate::hour(New_Received_Time))
# Get Country
freqIP <- freqIP %>% mutate(country= IP_lookup(IP_integer(IP)))
# Plot
ggplot(freqIP,aes(country,fill=IP))+labs(title='Frequent IPs with Countries')+geom_bar()+theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave('freqIP_country.png')
export_IP <- freqIP %>% select(Package.Id,Indicator.Title,Observable.Type,Indicator.Types,TTP.Name,URI,URI.Type,country,IP,IP.Category,IP.isSource,File,File.Hash,File.Format,File.Hash.Type,Producer.Name,Indicator.Description,Observable.Description,TTP.Description,Received.Time,New_Received_Time,Day,Hour)
# plot point
ggplot(freqIP, aes(x=New_Received_Time,y=country,color=IP))+geom_point()
# popular IPs
popular_ip <-freqIP%>% group_by(IP) %>% summarize(IPcount = n()) %>% ungroup() %>% arrange(desc(IPcount))
head(popular_ip,10)%>% ggplot(aes(x = reorder(as.factor(IP), IPcount), y = IPcount,fill=as.factor(IP))) +
  geom_bar(stat = 'identity') + 
  theme(legend.position = "none")+
  labs(y = 'Occurence', x = 'IP', title = 'Highest Occuring IPs') +
  coord_flip()

############# Frequent URI with TTP
onlyURI <- abuse_ch[abuse_ch$URI!='',]
freqURI <- onlyURI %>% group_by(URI) %>% filter(n()>1)
# 74 rows
freqURI <- freqURI[freqURI$TTP.Name!='',]
# 68 rows
ggplot(freqURI,aes(x=URI,fill=TTP.Name))+theme(axis.text.x = element_text(angle = 90, hjust = 1))+ geom_bar()
# Convert time
freqURI <- freqURI %>% mutate(New_Received_Time = ymd_hms(Received.Time))
# Get Day
freqURI <- freqURI %>% mutate(Day = wday(New_Received_Time,label=T))
# Get Hour
freqURI <- freqURI %>% mutate(Hour = lubridate::hour(New_Received_Time))

####### Most Frequent File Format
file_abuse_ch <- abuse_ch[abuse_ch$File.Format!='',]
# 2081 rows
popular_file_format <-file_abuse_ch %>% group_by(File.Format) %>% summarize(count = n()) %>% ungroup() %>% arrange(desc(count))
head(popular_file_format,10)%>% ggplot(aes(x = reorder(as.factor(File.Format), count), y = count,fill=as.factor(File.Format))) +
  geom_bar(stat = 'identity') + 
  theme(legend.position = "none")+
  labs(y = 'Occurence', x = 'File Format', title = 'Highest Occuring File Format') +
  coord_flip()

###### Most Frequent Attack Dates
date_abuse_ch <- abuse_ch %>% mutate(simple_date = ymd_hms(Received.Time))
date_abuse_ch <- date_abuse_ch %>% mutate(date = date(simple_date))
popular_attack_dates <-date_abuse_ch %>% group_by(date) %>% summarize(count = n()) %>% ungroup() %>% arrange(desc(count))
head(popular_attack_dates,15)%>% ggplot(aes(x = reorder(as.factor(date), count), y = count,fill=as.factor(TTP.Name))) +
  geom_bar(stat = 'identity') + 
  theme(legend.position = "none")+
  labs(y = 'Occurence', x = 'Attack Date', title = 'Highest Attacks by Date') +
  coord_flip()
#####

# subset the daataset with useful columns
abuse_ch <- subset(abuse_ch , select = c(Indicator.Title,Observable.Type,Indicator.Types,TTP.Type,TTP.Name,Domain,URI,IP,File.Format,Indicator.Description,Observable.Description,TTP.Description,Received.Time))

# Convert Time
abuse_ch <- abuse_ch %>% mutate(New_Received_Time = ymd_hms(Received.Time))
# get year
abuse_ch <- abuse_ch %>% mutate(Year = lubridate::year(New_Received_Time))
# get month
abuse_ch <- abuse_ch %>% mutate(Month = lubridate::month(New_Received_Time,label=T))
# ttp abuse ch
ttp_abuse_ch <- abuse_ch[abuse_ch$TTP.Name!='',]
# 569
# get country
ttp_abuse_ch <- ttp_abuse_ch %>% mutate(Country = 
case_when(IP!='' ~ IP_lookup(IP_integer(IP)),
        Domain!='' ~ CountryGeolocate(Domain)
        ))

############### Frequent Domains with TTP
onlyDomain <- ttp_abuse_ch[ttp_abuse_ch$Domain!='',]
freqDomain <- onlyDomain %>% group_by(Domain) %>% filter(n()>1)
# 305 rows
ggplot(freqDomain,aes(x=Domain,fill=TTP.Name))+theme(axis.text.x = element_text(angle = 90, hjust = 1))+labs(title='Frequent Domains',fill="TTPs")+geom_bar()
export_freqDomain <- freqDomain %>% select(Package.Id,Indicator.Title,Observable.Type,Indicator.Types,TTP.Name,Domain,Domain.isFQDN,URI,URI.Type,File,File.Hash,File.Format,File.Hash.Type,Producer.Name,Indicator.Description,Observable.Description,TTP.Description,Received.Time,New_Received_Time,Day,Hour)

CountryGeolocate <- function(domain){
    resp <- geolocate(domain)
    return(resp[,'country'])
}
hist(abuse_ch$Indicator.Title, 
     main="Histogram for Indicator Types", 
     xlab="Indicator Title", 
     border="blue", 
     col="green",
     las=1, 
     breaks=3)

uniqueIndicatorTypes <- function(data){
    types = unique(data$Indicator.Types)
    empty <- data.frame(Type = factor(), Count = numeric())
    print("Starting")
    for(type in types){
        print(paste(type,nrow(data[data$Indicator.Types==type,])))
        empty <- rbind(empty, data.frame(Type = factor(type), Count = nrow(data[data$Indicator.Types==type,]))
    }
    str(empty)
    print("End")
}

uniqueObservableTypes <- function(data){
    types = unique(data$Indicator.Types)
    Domain = URL = File = IP = 0
    print("Starting")
    for(type in types){
        if(grepl("IP",type)){
            IP = IP + nrow(data[data$Indicator.Types==type,])
        }
        if(grepl("URL",type)){
            URL = URL + nrow(data[data$Indicator.Types==type,])
        }
        if(grepl("Domain",type)){
            Domain = Domain + nrow(data[data$Indicator.Types==type,])
        }
        if(grepl("File",type)){
            File = File + nrow(data[data$Indicator.Types==type,])
        }
    }
    print(paste("IP",IP))
    print(paste("URL",URL))
    print(paste("File",File))
    print(paste("Domain",Domain))
    print("End")
}

uniqueTTPNames <- function(data,val){
    types = unique(data$TTP.Name)
    names = c()
    for(type in types){
        count = nrow(data[data$TTP.Name==type,])
        print(paste(type,count))
        if(val==0)
            names = append(names,type)
        else if(val==1)
            names = append(names,count)
        else if(val==2)
            names = append(names,round(count/nrow(data)*100,2))
    }
    return(names)
}

domainWithCounts <- function(data){
    count <- table(unlist(data$Domain))
    perc <- 100*count/sum(count)
    result <- data.frame(code = names(count),
                        count = as.integer(count), perc = as.numeric(perc))
    result <- result[-1,]
    return(result)
}

library("ggplot2")  # Data visualization
library("dplyr")    # Data manipulation
uniqueTTP <- uniqueTTPNames(ttp_abuse_ch,0)
unique_ttp_count <- uniqueTTPNames(ttp_abuse_ch,1)
unique_ttp_prop <- uniqueTTPNames(ttp_abuse_ch,2)
count.data <- data.frame(
  TTP = uniqueTTP,
  count = unique_ttp_count,
  prop = unique_ttp_prop
)
count.data
count.data <- count.data %>%
  arrange(desc(TTP)) %>%
  mutate(lab.ypos = cumsum(prop) - 0.5*prop)
count.data
mycols <- c("#0073C2FF", "#EFC000FF", "#868686FF", "#CD534CFF")
ggplot(count.data, aes(x = 2, y = prop, fill = TTP)) +
  geom_bar(stat = "identity", color = "white") +
  coord_polar(theta = "y", start = 0)+
  geom_text(aes(y = lab.ypos, label = prop), color = "white")+
  scale_fill_manual(values = mycols) +
  theme_void()+
  xlim(0.5, 2.5)

