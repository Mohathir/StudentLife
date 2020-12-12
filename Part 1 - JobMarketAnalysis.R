
# We will need the dplyr and the magrittr package to use the pipe operator
library(dplyr)
library(magrittr)

# Retrieving URL's from Government of Canada Website and saving them into the environment
urlemployment <- "http://www.edsc-esdc.gc.ca/ouvert-open/labour-travail/Summary_sommaire_2019_2028.csv"
Employment <- read.csv(urlemployment, stringsAsFactors = FALSE)
Employment <- Employment %>%
        # Removing french columns
        extract(,-1) %>%
        extract(,-2) %>%
        extract(,-12) %>%
        extract(,-13) 

# Change the column names
names(Employment) <- c("Occupation", "Employment.2018", "Growth", "Retirements", 
                       "Turnover", "Openings", "EnteringUniveristyGraduates", 
                       "Entering.Immigrants", "Other.Job.Seekers", "Total.Job.Seekers", 
                       "Current.Condition", "Future.Condition")

# Using Chi Squared to determine independence between new graduates and the total market
# Create a data frame with only the columns we need
NewGradsVSMarket <- data.frame(NewGrads = Employment$EnteringUniveristyGraduates, 
                               TotalMarket = Employment.2018) 

# Use the data frame we created to run a chi squared test
chisq.test(NewGradsVSMarket)

# Retrieving URL's from Government of Canada Website and saving them into the environment
urlretirements <- "http://www.edsc-esdc.gc.ca/ouvert-open/labour-travail/industrial_retirements_retraites_par_secteur_2019_2028.csv"
Industry.retirements <- read.csv(urlretirements, stringsAsFactors = FALSE)
Industry.retirements <- Industry.retirements %>%
        # Remove french columns and first aggregate row for all industries. 
        extract(,-1) %>%
        extract(,-2) %>%
        filter(Industry != 'All Industries')

# Retrieving URL's from Government of Canada Website and saving them into the environment
urljobsavailable <- "http://www.edsc-esdc.gc.ca/ouvert-open/labour-travail/industrial_jo_pe_par_secteur_2019_2028.csv"
Jobs.Available.per.Industry <- read.csv(urljobsavailable, stringsAsFactors = FALSE)

Jobs.Available.per.Industry <- Jobs.Available.per.Industry %>%
        # Remove french columns and first aggregate row for all industries. 
        extract(,-1) %>%
        extract(,-2) %>%
        filter(Industry != 'All Industries')

#Install necessary packages to create tree map, then load them with library
install.packages("treemap")
library(treemap)
install.packages('RColorBrewer')
library(RColorBrewer)

#Use tree map function to visualize "Jobs.Available.per.Industry"
treemap(Jobs.Available.per.Industry,
        index="Industry",
        vSize="X2020",
        type="index",
        fontsize.labels=8,
        fontfamily.labels = "sans",
        palette = brewer.pal(n=42, "OrRd"),
        title = "Jobs Available per Industry in 2020"
)

#Use tree map function to visualize "Industry.retirements"
treemap(Industry.retirements,
         index="Industry",
         vSize="X2020",
         type="index",
        fontsize.labels=8,
        fontfamily.labels = "sans",
        palette = brewer.pal(n=42, "Blues"),
        title = "Retirements per Industry in 2020"
)