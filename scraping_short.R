# This script scrapes information from the Ofsted website
# including reports and school characteristics

# As the website has changes I had to rewrite the code.
# Search engine
# Only choose 'secondary' and 'primary'
# https://reports.ofsted.gov.uk/search?q=&location=&lat=&lon=&radius=&level_1_types=1&level_2_types%5B%5D=1&level_2_types%5B%5D=2
# On 6 April 2022 this gave 23297 results
# On 18 May 2022 this gave 23292 results
# It's easiest for now to hardcode this number (see below)

# The list can provide:
# - URL
# - Name
# - Category
# - Address
# - Latest inspection. 
# - URN. The last three might be useful to check against the page you get to when clicked on.

# Clicking on one hit then gives a page with:
# - Name
# - URN. This URN could be used elsewhere to get data about the institution
# - Address
# - Latest inspection
# - All inspections - choose 'standard inspection' with data and link to pdf

url="https://reports.ofsted.gov.uk/search?q=&location=&lat=&lon=&radius=&level_1_types=1&level_2_types%5B0%5D=1&level_2_types%5B1%5D=2&start=0&status%5B0%5D=1&rows=50"
# url="https://reports.ofsted.gov.uk/search?q=&location=&lat=&lon=&radius=&level_1_types=1&level_2_types%5B0%5D=1&level_2_types%5B1%5D=2&start=0&status%5B0%5D=1&rows=50"

# Sources used:
# Use this one https://zenscrape.com/web-scraping-r/
# It has feeding through to a new page AND multiple pages
# It is useful to use https://selectorgadget.com/ when selecting css codes

# Don't think all these packages are really needed
install.packages('rvest')
library(rvest)
install.packages('dplyr')
library(dplyr)
library("plyr")
library(tidyverse)

# This function get the reports from one school page
get_reports = function(ofsted_link) {
  
  school_page = read_html(ofsted_link)
  school_name = school_page %>% html_nodes(".heading--title") %>% html_text()
  school_urn = school_page %>% html_nodes(".title-block__subheading") %>% html_text()
  school_address = school_page %>% html_nodes(".title-block__address") %>% html_text()
  reports_titles = school_page %>% html_nodes(".publication-link") %>% html_text()
  reports_time = school_page %>% html_nodes(".timeline__date") %>% html_text()
  reports_urls = school_page %>% html_nodes(".publication-link") %>% html_attr("href")
  # Prevent schools with no reports to cause an error
  if(is_empty(reports_titles)) { length(reports_titles) <- 1}
  if(is_empty(reports_time)) { length(reports_time) <- 1}
  if(is_empty(reports_urls)) { length(reports_urls) <- 1}
  # Force number of dates down to number of URLs
  reports_time <- reports_time[1:length(reports_urls)]
  reports_df<-data.frame(Name = school_name, Urn=school_urn, Address=school_address, Reports=reports_titles, Date=reports_time, Urls=reports_urls)
  reports_df = rbind(reports_df,empty_df)
  return(reports_df)
}

# Create TOTAL data frame
empty_df <- data.frame(matrix(ncol = 6, nrow = 0))
empty_df <-  plyr::rename(empty_df, c("X1"="Name", "X2"="Urn", "X3"="Address", "X4"="Reports", "X5"="Date", "X6"="Urls"))
total <- empty_df

# Get total
start_page_of_search = read_html(url)
totalhits = strtoi(start_page_of_search %>% html_nodes(".js-results-count") %>% html_text())[1]
x = round(totalhits/10)

# Cut in portions of 1/10 of total - hardcoded
total1 <- empty_df
total2 <- empty_df
total3 <- empty_df
total4 <- empty_df
total5 <- empty_df
total6 <- empty_df
total7 <- empty_df
total8 <- empty_df
total9 <- empty_df
total10 <- empty_df
totall <- empty_df


# WARNING - this loop severely strains CPU power and the Ofsted website
# HANDLE WITH CARE
# Run manually 3/6/2022

for (i in 1:10)
{
    # Go through the pages and collect all the URLS
    # Per portion
    f = (i-1)*x
    t = i * x
    # By 10 is easier for debugging
    for (page_result in seq(from = f, to = t, by = 50)){
      totall <- empty_df 
      link = paste0("https://reports.ofsted.gov.uk/search?q=&location=&lat=&lon=&radius=&level_1_types=1&level_2_types%5B0%5D=1&level_2_types%5B1%5D=2&start=", page_result , "&status%5B0%5D=1&rows=50")
       ofsted <- read_html(link)
       ofsted_links = ofsted %>% html_nodes(".search-result a") %>% html_attr("href") %>% paste("https://reports.ofsted.gov.uk", ., sep = "")
       reports_per_page = lapply(ofsted_links, FUN = get_reports)
       colltest = bind_rows(reports_per_page)
       totall = rbind(totall,colltest)
    }

  }

# Create TOTAL data frame
empty_df_schools<- data.frame(matrix(ncol = 4, nrow = 0))
empty_df_schools <-  plyr::rename(empty_df_schools, c("X1"="Name", "X2"="Category", "X3"="Rating", "X4"="LreportURN"))
totalschools <- empty_df_schools

# Now just the schooldata separately
# One by one is easier in this case (well, maybe it would have been easier to use from the beginning :-)
# WARNING - very inefficient atm, even slower than the other part which downloads MORE data :-P

for (page_result in seq(from = 14000, to = totalhits, by = 1)){
  link = paste0("https://reports.ofsted.gov.uk/search?q=&location=&lat=&lon=&radius=&level_1_types=1&level_2_types%5B0%5D=1&level_2_types%5B1%5D=2&start=", page_result , "&status%5B0%5D=1&rows=1")
  school_info <- read_html(link)
  school_name = school_info %>% html_nodes(".search-result__title") %>% html_text()
  school_inf = school_info %>% html_nodes(".search-result__provider-info") %>% html_text()
  schools_df<-data.frame(Name = school_name, Category=school_inf[1], Rating=school_inf[2], LreportURN=school_inf[3])
  totalschools <- rbind(totalschools,schools_df)
}

ts1 <- totalschools
ts2 <- totalschools
ts3 <- totalschools
ts4 <- totalschools

# Combine portions
#put all data frames into list
df_list_schools <- list(ts1,ts2,ts3,ts4)   
#merge all data frames together
totalschools <-  Reduce(function(x, y) merge(x, y, all=TRUE), df_list_schools)  
# The above method seems to automatically remove 'doubles'


# Write schools to CSV
write.csv(totalschools, "totalschools.csv")

# Combine portions
#put all data frames into list
df_list <- list(total1, total2, total3, total4, total5, total6, total7, total8, total9, total10, totall)   
#merge all data frames together
total <-  Reduce(function(x, y) merge(x, y, all=TRUE), df_list)  
# The above method seems to automatically remove 'doubles'

# Write to CSV
write.csv(total, "Ofstedreports.csv")

# Extract ID filename from URL
total$fileid = lapply(total$Urls, basename)


# From https://stackoverflow.com/questions/64526589/error-when-downloading-multiple-pdf-files-from-list-of-urls-in-r
# making the filenames
# Before downloading remove those with no downloads


# Probably best to download in batches by making batches of 'total'
# Remove NA
totaldl <- na.omit(total)

# Gave 404 error in batch 18 - could use 'catch' but quicker manually
# 93908	The Stables Independent School	URN: 145960	A
# Pre-registration inspection, pdf - 17 August 2018"	09-Jul-18	https://files.ofsted.gov.uk/v1/file/50012987
totaldl <- subset(totaldl, Urls != "https://files.ofsted.gov.uk/v1/file/50012987")
ndownloads <- nrow(totaldl)

# Let's download in 20 portions so
batch <- round(ndownloads/20)

for (i in 18:18) 
{
  #i=1 # done 3/6/22 16u
  #i=2 # done 3/6/22 16u
  #i=3 # test 3/6/22 23u - worked - so from 4 in loop
  #i=20 # check last few
  from = (i-1)*batch
  to = (i*batch)
  totalsubset <- totaldl[from:to,]
  paste0("Downloading batch ", i, " of ", batch, " downloads")
  dir.create(paste0("run_030622/batch", i))
  downloaded = lapply(totalsubset$Urls, function(url)
  {
  # extract the last part of the url to make the filename
  destination = unlist(strsplit(url, '/'))
  destination = destination[length(destination)]
  destination = paste0 (getwd(), '/run_030622/batch', i, '/', destination, '.pdf')
  # Add URL number to data frame for reference
  # total
  # download the file
  download.file(url = url, destfile=destination, mode="wb")
    })
  paste0("Finished downloading batch ", i, " of ", batch, " downloads")
  Sys.sleep(10)
}

# zip batches
for(i in 5:20)
{
  d <- paste0('run_030622/batch',i,'/')
  n <- paste0('batch',i)
  files2zip <- dir(d, full.names = TRUE)
  zip(zipfile = n, files = files2zip)  
} 
              