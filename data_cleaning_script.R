#importing packages
pacman::p_load(
  rio,        # importing data  
  here,       # relative file pathways  
  janitor,    # data cleaning and tables
  lubridate,  # working with dates
  matchmaker, # dictionary-based cleaning
  epikit,     # age_categories() function
  tidyverse,   # data management and visualization
  stringr     # string processing
)

#import data
Dengue <- import(here("data", "sequences.csv"))
skimr::skim(Dengue)
names(Dengue)
Dengue <- Dengue %>% select(-c("Submitters", "Organization", 
                               "Org_location", "Release_Date")) # dropping unwanted columns
Dengue <- Dengue %>% filter(Length >= 1000) # filter out sequences with less than 1000 nucleotides

Dengue <- Dengue %>% drop_na(Collection_Date) # drop observations missing collection date

# Assuming 'date_var' is your date column and 'Dengue' is your data frame
Dengue <- Dengue %>%
  mutate(Collection_Date = case_when(
    str_detect(Collection_Date, "^\\d{4}-\\d{2}-\\d{2}$") ~ ymd(Collection_Date),
    str_detect(Collection_Date, "^\\d{4}-\\d{2}$") ~ ym(Collection_Date),
    str_detect(Collection_Date, "^\\d{4}$") ~ ymd(paste0(Collection_Date, "-01-01")),
    TRUE ~ as.Date(NA)
  ))
#inspecting problem rows and dropping them
problem_rows <- Dengue %>% filter(is.na(Collection_Date))
print(problem_rows)
Dengue <- Dengue %>% drop_na(Collection_Date) #Removing the rows missing dates
Dengue$Collection_Date <- as.Date(Dengue$Collection_Date) #convert the column into date

# Create a new column 'Collection_Year' that contains the year of the collection date
Dengue <- Dengue %>%
  mutate(Collection_Year = year(Collection_Date))

#filter sequences that were collected before 2010
Dengue <- Dengue %>% filter(Collection_Year >= 2010)


#creating Tanzania data set
Tanzania_Dengue <- Dengue %>% filter(Country == "Tanzania")
Global_Dengue <- Dengue %>% filter(Country != "Tanzania")


print(table(Global_Dengue$Country))

print(table(Global_Dengue$Country, Global_Dengue$Collection_Year))



