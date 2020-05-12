
library(tidyverse)
library(curl)
library(RCurl)
library(readODS)

#setwd("C:/Users/c-carney/OneDrive - DFID/Documents/R/edu_dashboard")


#################################################################################
#                                 get data
#################################################################################


#sid18<-read_ods(URL(https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/854348/Data-Underlying-SID-Dec19rev.ods))

# accessed https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/854348/Data-Underlying-SID-Dec19rev.ods
# save as .csv
sid17_18<-read.csv("Data-Underlying-SID-Dec19rev.csv")
str(sid17_18$HeadlineMeasureofODA..thousands.)

# accessed https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/695435/data-underlying-the-sid2017-revision-March.ods
# save as csv
sid09_16<-read.csv("data-underlying-the-SID2017-revision-March09-16.csv")

str(sid09_16$NetODA..thousands.)

#################################################################################
#                                rename variables
#################################################################################

# for 17-18 data
sid17_18$oda18<-as.numeric(sid17_18$HeadlineMeasureofODA..thousands.)
str(sid17_18$oda18)
sid17_18$sector<-as.factor(sid17_18$SIDsector)
sid17_18$channel<-sid17_18$BilateralMultilateralBreakdown
sid17_18$country<-sid17_18$RecipientCountryText
sid17_18$region<-sid17_18$RegionText
sid17_18$gov<-sid17_18$Cross.GovernmentFundSpendingAgency
sid17_18$dac5<-sid17_18$BroadSectorCode.DAC5code.
sid17_18$year<-sid17_18$Year
sid17_18$ChannelParent<-(as.factor(sid17_18$ChannelParent))

# for 09 - 16 data
sid09_16$oda16<-as.numeric(as.character(sid09_16$NetODA..thousands.))
str(sid09_16$oda16)

sid09_16$sector<-as.factor(sid09_16$SIDsector)
sid09_16$channel<-sid09_16$BilateralMultilateralBreakdown
sid09_16$country<-sid09_16$RecipientCountryText
sid09_16$region<-sid09_16$RegionText
sid09_16$gov<-sid09_16$Cross.GovernmentFundSpendingAgency
sid09_16$dac5<-sid09_16$BroadSectorCode.DAC5code.
sid09_16$year<-sid09_16$Year
sid09_16$AmountsExtended..thousands.<-(as.numeric(sid09_16$AmountsExtended..thousands.))
sid09_16$AmountsReceived..thousands.<-(as.numeric(sid09_16$AmountsReceived..thousands.))

summary(as.factor(sid09_16$SectorPurposeCode.CRScode.))




# bring all Edu Oda SID together in one dataframe
all_sid<-full_join(sid09_16, sid17_18)



# combine the two oda columns to one             
paste_noNA <- function(x,sep=", ") { # function to paste ignoring missing data
  gsub(", " ,sep, toString(x[!is.na(x) & x!="" & x!="NA"] ) ) }
sep=""

all_sid$oda<-apply( all_sid[ , c(23,34) ] , 1 , paste_noNA , sep=sep)
str(all_sid$oda)

all_sid$oda <-(as.numeric(as.character(all_sid$oda)))
summary(all_sid$oda)

all_sid<- all_sid %>% replace_na(list(oda = 0))
summary(all_sid$oda)

##get all oda for edu



# edu dashbaord

all_sid$sector
choice.sector <- as.list(all_sid$sector)
choice.sector<-unique(choice.sector)
choice.sector

names(choice.sector) <- all_sid$sector
choice.sector


sector<-unique(all_sid$sector)
sector
choice.sector <- as.list(sector)
names(choice.sector) <- sector

choice.sector_v2 <- as.list(sector)
names(choice.sector_v2) <- sector


extendingagency<-unique(all_sid$ExtendingAgency)
extendingagency
choice.agency <- as.list(extendingagency)
names(choice.agency) <- extendingagency
all_sid$sector<-as.factor(all_sid$sector)


sidsector<- all_sid %>%  
  select (Year, sector, ProjectTitle, ExtendingAgency, oda) %>% 
  # filter( Year=="2018") %>% 
  group_by(sector, Year) %>%  # group by
  summarise(oda_year=((sum(oda))/1000))  %>% #  year total
  na.omit 
       sidsector$sector<-as.factor(sidsector$sector)
       
       
       
      summary(all_sid$ExtendingAgency)
     
                
  all_sid <- all_sid %>% 
    mutate(dept = recode(ExtendingAgency, "Colonial Pensions administered by DFID" = "Pensions",
               "Department for Business, Energy & Industrial Strategy" = "BEIS",
               "Department for Business, Energy and Industrial Strategy" = "BEIS",
               "Department for Culture, Media and Sports" = "DCMS",
               "Department for Digital, Culture, Media and Sports" = "DCMS",
               "Department for Education" = "DfE",
               "Department for Education " = "DfE",
               "Department for Environment Food and Rural Affairs" ="DEFRA",
               "Department for International Development" = "DFID",
               "Department for Work and Pensions" = "DWP",
               "Department of Health and Social Care" = "Depart of Health",
               " Department of Health" = "Depart of Health",
               "Department of Health" = "Depart of Health",
               "Export Credit Guarantee Department" = "Export Credit",
               "Foreign & Commonwealth Office" = "FCO",
               "IMF Poverty Reduction and Growth Trust(PRGT)" = "IMF PRGT",
               "Ministry of Defence" = "MoD",
               "Miscellaneous" = "Misc." ,
               "Non-DFID EC Attribution " = "Non-DFID EC",
               "Non-DFID EC Attribution" = "Non-DFID EC",
               "Scottish Government" = "Scottish Gov",
               "Other In-Donor Refugee Costs" = "Other Refugee Costs",
               "Welsh Assembly Government" = "Welsh Assembly Gov"))
     
  
summary(all_sid$dept)

extendingagency<-unique(all_sid$dept)
extendingagency
choice.agency <- as.list(extendingagency)



extendingagency_s<-unique(all_sid$dept)
extendingagency_s
choice.agency_s <- as.list(extendingagency_s)


summary(all_sid$dac5)
all_sid$dac5<-as.factor(all_sid$dac5)
all_sid <- all_sid %>% 
  mutate(sub_sector = recode(dac5, "111" = "Unspecified",
                             "112" = "Basic",
                             "113" = "Secondary",
                             "114" = "Post-Secondary"))
                             
                      summary(all_sid$sub_sector)

                      
                      subsector<-unique(all_sid$sub_sector)
                     subsector<-c("Unspecified","Basic","Secondary","Post-Secondary")
                      choice.subsector <- as.list(subsector)   
                      
                      choice.subsector_v2 <- as.list(subsector)              
                      
                      
                      
   ### make list for detailed categories                    
                      
                      summary(all_sid$SectorPurposeCode.CRScode.)
                      
all_sid$crs_sector<-as.factor(all_sid$SectorPurposeCode.CRScode.)
summary(all_sid$crs_sector)
all_sid <- all_sid %>% 
    mutate(crs = recode(crs_sector, "11120"	=	"Facilities & training",
  "11130"		= "Teacher training",
"11182"	=	"Edu research",
"11220"	= 	"Primary edu",
"11240"	=	"Early childhood edu",
"11250"		= "School feeding",
"11320"	=	"Secondary edu",
"11330"	=	"Vocational",
"11420"	=	"Higher edu",
"11430"		= "Advanced tech & managerial"))

crslist<-c("Facilities & training","Teacher training","Edu research","Primary edu", "Early childhood edu",
    "School feeding",	"Secondary edu",	"Vocational","Higher edu","Advanced tech & managerial")
choice.crs <- as.list(crslist)     
choice.crs
                    

### list of recipenet countries


summary(all_sid$country)


countrylist<-unique(all_sid$country)
countrylist
choice.country <- as.list(countrylist)
names(choice.country) <- extendingagency

