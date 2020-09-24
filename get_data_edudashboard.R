
setwd("~/desktop/r/myrepo")

library(tidyverse)

#################################################################################
#                                 get data
#################################################################################


#sid18<-read_ods(URL(https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/854348/Data-Underlying-SID-Dec19rev.ods))

# accessed https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/854348/Data-Underlying-SID-Dec19rev.ods
# save as .csv
sid19<-read.csv("Data_Underlying_SID_2019.csv")
str(sid19)
str(sid19$HeadlineMeasureofODA..thousands.)

# accessed https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/695435/data-underlying-the-sid2017-revision-March.ods
# save as csv
sid09_16<-read.csv("data-underlying-the-SID2017-revision-March09-16.csv")

str(sid09_16$NetODA..thousands.)

#################################################################################
#                                rename variables
#################################################################################

# for 17-18 data
sid19$oda19<-as.numeric(sid19$HeadlineMeasureofODA..thousands.)
str(sid19$oda19)
sid19$sector<-as.factor(sid19$SIDsector)
summary(sid19$sector)
sid19 <- sid19 %>% 
  mutate(sector = recode(sector, "Administrative Costs of Donors" = "Admin",
                         "Action Relating to Debt"= "Debt relief",
                         "Economic Infrastructure and Services" = "Economic Infrastructure & Services",
                         "Humanitarian Aid" = "Humanitarian",
                         "Multisector / Cross-Cutting" = "Multisector",
                         "Other Social Infrastructure and Services" = "OTHER SOCIAL INFRASTRUCTURE AND SERVICES",
                         "Production Sectors" = "Production Services",
                         "Refugees in Donor Countries" = "Refugee costs",
                         "Unallocated / Unspecified" = "Unallocated",
                         "Water Supply and Sanitation" = "WASH"))

summary(sid19$sector)


sid19$channel<-sid19$BilateralMultilateralBreakdown
sid19$country<-sid19$RecipientCountryText
sid19$region<-sid19$RegionText
sid19$gov<-sid19$Cross.GovernmentFundSpendingAgency
sid19$dac5<-sid19$BroadSectorCode.DAC5code.
sid19$year<-sid19$Year
sid19$ChannelParent<-(as.factor(sid19$ChannelParent))


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
summary(sid09_16$sector)
sid09_16 <- sid09_16 %>% 
  mutate(sector = recode(sector, "Humantarian" = "Humanitarian"))
summary(as.factor(sid09_16$SectorPurposeCode.CRScode.))
summary(sid09_16$sector)



# bring all Edu Oda SID together in one dataframe
all_sid<-full_join(sid09_16, sid19)
all_sid$sector<-as.factor(all_sid$sector)
summary(all_sid$sector)


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

all_sid$dac5<-as.factor(all_sid$dac5)
all_sid <- all_sid %>% 
  mutate(sub_sector = recode(dac5, "111" = "Unspecified",
                             "112" = "Basic",
                             "113" = "Secondary",
                             "114" = "Post-Secondary"))

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


  
  write.csv(all_sid, 'all_sid.csv')

# lists 
summary(all_sid$sector)

# edu dashbaord - lists

year<-unique(all_sid$year)
year
choice.year <- as.list(year)
names(choice.year) <- year

summary(all_sid$sector)
sector<-unique(all_sid$sector)
sector
choice.sector <- as.list(sector)
names(choice.sector) <- sector

choice.sector_v2 <- as.list(sector)
names(choice.sector_v2) <- sector

choice.sector_v3 <- as.list(sector)
names(choice.sector_v3) <- sector


extendingagency<-unique(all_sid$ExtendingAgency)
extendingagency
choice.agency <- as.list(extendingagency)
names(choice.agency) <- extendingagency


extendingagency<-unique(all_sid$dept)
extendingagency
choice.agency <- as.list(extendingagency)

extendingagency_s<-unique(all_sid$dept)
extendingagency_s
choice.agency_s <- as.list(extendingagency_s)

subsector<-unique(all_sid$sub_sector)
subsector<-c("Unspecified","Basic","Secondary","Post-Secondary")
choice.subsector <- as.list(subsector)   

choice.subsector_v2 <- as.list(subsector)              

### make list for detailed categories                    

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
