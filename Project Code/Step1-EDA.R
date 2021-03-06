# Exploratary analysis
library(ggplot2) # Data visualization
library(RColorBrewer)
library(data.table)
library(treemap)
library(kableExtra)
library(dplyr)
library(colormap)
library(ggridges)
system("ls ../input")
loans <- read_csv("../input/data-science-for-good-kiva-crowdfunding/kiva_loans.csv")
locations <- read_csv("../input/data-science-for-good-kiva-crowdfunding/kiva_mpi_region_locations.csv")
theme_ids <- read_csv("../input/data-science-for-good-kiva-crowdfunding/loan_theme_ids.csv")
region <- read_csv("../input/data-science-for-good-kiva-crowdfunding/loan_themes_by_region.csv")

# Let us compare the mean of the amount dispersed by KIVA to the the field agent(funded amount) 
# and the amount which is dispersed by the field agaent to the borrower(loan amount)
USD<-c(round(mean(loans$loan_amount),digits=2), round(mean(loans$funded_amount),digits = 2))
region <- as.data.table(region)
COMP<-c(mean(loans$loan_amount),mean(loans$funded_amount))
COMP<-data.frame(USD, row.names = c("Loan Mean", "Funded Mean"))
COMP

# 1
# Number of loans per country compared to the average
loans_dt  <- as.data.table(loans)
nloans <- loans_dt[, .N, by=country]
nloans$N_z <- round((nloans$N - mean(nloans$N))/sd(nloans$N), 2)
nloans$type <- ifelse(nloans$N_z < 0, "below", "above")
nloans <- nloans[order(N_z),]
nloans$country <- factor(nloans$country, levels = nloans$country)
nloans <- tail(nloans, 50)

ggplot(nloans, aes(x=country, y=N_z, label=N_z)) +
  geom_bar(stat='identity', aes(fill=type), width=.5)  +
  scale_fill_manual(name="Loans",
                    labels = c("Above Average", "Below Average"),
                    values = c("above"="#42b9ff", "below"="#42f4ee")) +
                    labs(subtitle="Normalised number of loans",   
                    title= "Loans Comparisons-country") + coord_flip()

# 2
# loan_amounts
# Distribution of loan amount 
options(repr.plot.width = 8, repr.plot.height = 4)
ggplot(aes(loan_amount),data = loans) + geom_histogram(binwidth = 0.1, color="white", fill=rgb(0.2,0.7,0.1,0.4) ) + 
  scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  labs(x = 'Loan Amount' ,y = 'Count', title = paste('Loan amount distribution'))
# Distribution of loan amount per country
ggplot(region, aes(x =log(amount), y=country, fill=country)) +  
  geom_density_ridges() + guides(fill=FALSE)

# sectors
# Distribution of sectors
loan_by_sector = loans %>%
  group_by(sector) %>%
  summarise(n = n(),
            mean_funded = mean(funded_amount),
            median_funded = median(funded_amount),
            mean_loan = mean(loan_amount),
            median_loan = median(loan_amount),
            mean_lender = mean(lender_count),
            median_lender = median(lender_count),
            total_loan = sum(lender_count)) %>%
            arrange(desc(n)) %>%
            head(20)
options(repr.plot.width = 8, repr.plot.height = 4)
ggplot(aes(reorder(sector,n),n),data = loan_by_sector) + geom_bar(aes(fill=sector),stat='identity') +
  geom_text(aes(x = sector, y = 1, label = paste0("(",n,")",sep="")),hjust=0,colour = 'black',fontface = 'bold') +
  coord_flip() + labs(x = 'Sector',y = 'Count', title = 'Sector Distribution')  
# Distribution of sectors per country
t10 <- droplevels(tail(nloans$country,10))
t10_loans <- loans[loans$country %in% t10,]
t10_loans$country <- as.factor(t10_loans$country)
t10_loans$sector <- as.factor(t10_loans$sector)
ggplot(t10_loans, aes(country)) +
  geom_bar(aes(fill=sector), width = 0.5, col='black') + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))

# Tree map-countries with the highest frequencies
treemap(region[, .N, by=list(country)],index=c("country"),vSize="N",type="index")
head(region[, .N, by=list(country)][order(-N),],3)
#       country    N
#1: Philippines 3467
#2:     Armenia 1064
#3:    Colombia  824

# Now lets us shift attendion to a particular country that is the country with most loan amounts
# Philippines
phil <- region[country == "Philippines",]
phil_loans <- loans_dt[country == "Philippines",]
Philippines <- phil_loans[, .N, by = c('sector','activity')]
# Distribution of loan amount
ggplot(phil, aes(x =log(amount), y=country, fill=country)) +  
  geom_density_ridges() + guides(fill=FALSE)
# Distribution of sector per country
ggplot(phil_loans, aes(x =log(loan_amount), y=sector, fill=sector)) +
  geom_density_ridges() +
  guides(fill=FALSE)

# Phillippines-Plot between the sector distribution depending on year and borrower gender  
phil_loans$year <- year(phil_loans$funded_time)
d <- with(phil_loans, phil_loans[order(year, sector, borrower_genders),])
d <- d[!is.na(year)]

ggplot(data=d, aes(x=borrower_genders, y=loan_amount, fill=sector)) +
  geom_bar(stat="identity") +
  facet_grid(~year)

# Similarly we can have a look at various countries

# NOTE:(SOME OTHER PLOTS)
# loan amount
qplot(loan_amount, data = loans, geom="histogram", col=I("steelblue"), fill=I("blue"), alpha=I(.4))

# term in months
qplot(term_in_months, data = loans, geom="histogram", col=I("steelblue"), fill=I("blue"), alpha=I(.4))
# Most of the loans are concentrated with 5 to 15 months as their term of months.

# loan theme type
t <- table(region$loan_theme_type)  # make table
ptab<-prop.table(t)
ptab<-ptab*100 # Convert to percentages 
barplot(ptab, main = "Bar Plot", xlab = "Loan Theme Type", ylab = "Proportion", col=c("orange", "steelblue"))
