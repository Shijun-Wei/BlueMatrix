setwd("~/UCD Drive/Blue Matirx")
library(stringr)

Company_id<- read.csv('Document.csv')
Company_id <- as.data.frame(cbind(Company_id$m_id..,Company_id$unt...))
names(Company_id)<- c("CompanyId",'DocNumber')
Company_id$DocNumber<- gsub("\\s+","",Company_id$DocNumber)

Company_id$DocNumber

Company_id$DocNumber <- as.numeric(Company_id$DocNumber)
Company_id<- na.omit(Company_id)

summary(Company_id$DocNumber)
hist(Company_id$Docnumber)

### document id distribution by each company

Company_id_Distinct<- read.csv('DocumentDistinct.csv')
Company_id_Distinct <- as.data.frame(cbind(Company_id_Distinct$bm_provider_firm_id,Company_id_Distinct$count.distinct.bm_doc_id..))
names(Company_id_Distinct)<- c("CompanyId",'DocNumber')
Company_id_Distinct$DocNumber<- gsub("\\s+","",Company_id_Distinct$DocNumber)

Company_id_Distinct$DocNumber

Company_id_Distinct$DocNumber <- as.numeric(Company_id_Distinct$DocNumber)
Company_id_Distinct<- na.omit(Company_id_Distinct)

hist(log10(Company_id_Distinct$DocNumber),breaks = 5,main= 'Histogram of Document number by company_Id', xlab = 'log Document Number')


summary(Company_id_Distinct$DocNumber)

### Report publish year by year span
ReportsByYear<- read.csv('Year span.csv')
ReportsByYear <- as.data.frame(cbind(ReportsByYear$year.bm_doc_publish_time.,ReportsByYear$count.bm_doc_id..))
names(ReportsByYear)<- c("Year",'DocNumber')
ReportsByYear$DocNumber<- gsub("\\s+","",ReportsByYear$DocNumber)
ReportsByYear$DocNumber<- as.numeric(ReportsByYear$DocNumber)

ReportsByYear$Year<- gsub("\\s+","",ReportsByYear$Year)

ReportsByYear <- na.omit(ReportsByYear)

ReportsByYearR<- subset(ReportsByYear,ReportsByYear$Year >2004 & ReportsByYear$Year <= 2020 )

barplot(ReportsByYearR$DocNumber~ReportsByYearR$Year, main="Report Distribution", 
        xlab="Year", ylab = 'Report Number')

### Report publish year by month span
ReportsByMonth<- read.csv('MonthSpan.csv')
ReportsByMonth <- as.data.frame(cbind(ReportsByMonth$X,ReportsByMonth$X.1,ReportsByMonth$X.2))
names(ReportsByMonth)<- c("Year",'Month','DocNumber')
ReportsByMonth$DocNumber<- gsub("\\s+","",ReportsByMonth$DocNumber)
ReportsByMonth$DocNumber<- as.numeric(ReportsByMonth$DocNumber)

ReportsByMonth$Year<- gsub("\\s+","",ReportsByMonth$Year)
ReportsByMonth$Year <- as.numeric(ReportsByMonth$Year)

ReportsByMonth$Month<- gsub("\\s+","",ReportsByMonth$Month)
ReportsByMonth$Month <- as.numeric(ReportsByMonth$Month)

ReportsByMonth <- na.omit(ReportsByMonth)

ReportsByMonthR$Date <- paste(ReportsByMonthR$Year,'/',ReportsByMonthR$Month)

ReportsByMonthR<- subset(ReportsByMonth,ReportsByMonth$Year >2004 & ReportsByMonth$Year <= 2020)

barplot(ReportsByMonthR$DocNumber, main="Report Distribution",xlab="Month")

### Report publish year by monthly
ReportsMonthly<- read.csv('Monthly.csv')
ReportsMonthly <- as.data.frame(cbind(ReportsMonthly$month.bm_doc_publish_time.,ReportsMonthly$count.bm_doc_id.))
names(ReportsMonthly)<- c("Month",'DocNumber')
ReportsMonthly$DocNumber<- gsub("\\s+","",ReportsMonthly$DocNumber)
ReportsMonthly$DocNumber<- as.numeric(ReportsMonthly$DocNumber)

ReportsMonthly$Month<- gsub("\\s+","",ReportsMonthly$Month)
ReportsMonthly$Month <- as.numeric(ReportsMonthly$Month)

ReportsMonthly <- na.omit(ReportsMonthly)

barplot(ReportsMonthly$DocNumber~ReportsMonthly$Month, main="Report Distribution",xlab="Month", ylab = 'Report Number')

### Report publish year by Quarterly

Quarter <- c(1,2,3,4)
DocNumerQ1<- sum(ReportsMonthly$DocNumber[ReportsMonthly$Month >1 & ReportsMonthly$Month<= 3])
DocNumerQ2<- sum(ReportsMonthly$DocNumber[ReportsMonthly$Month >3 & ReportsMonthly$Month<= 6])
DocNumerQ3<- sum(ReportsMonthly$DocNumber[ReportsMonthly$Month >6 & ReportsMonthly$Month<= 9])
DocNumerQ4<- sum(ReportsMonthly$DocNumber[ReportsMonthly$Month >9 & ReportsMonthly$Month<= 12])

DocNumber<- c(DocNumerQ1,DocNumerQ2,DocNumerQ3,DocNumerQ4)

barplot(DocNumber~Quarter, main="Report Distribution",xlab="Quarter", ylab = 'Report Number')





