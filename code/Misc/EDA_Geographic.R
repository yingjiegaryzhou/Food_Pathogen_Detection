########### Setup ########### 
library(tidyverse)
library(knitr)
library(gridExtra)
library(grid)
library(tableone)
library(lubridate) 
library(table1)
library(kableExtra)

setwd('~/downloads/php2550/pda_project')
isolates <- read.csv('isolates_filtered_collect_date_imputed.csv')
dim(isolates)

# Replace missing values with NA
for (i in 1:dim(isolates)[2]) {
  isolates[isolates[,i] %in% c('',' ', '  '), i] <- NA
}

# Create a date column using imputed month:
isolates <- isolates %>% 
  mutate(date.impute = paste0(paste(isolates$collection.yr, isolates$collection.mon, sep = '-'), '-01'),
         date.impute = as.Date(date.impute))


##################################################################
####### Geographic information Data clean and related plots ######
##################################################################

# Initial clean up is finished in 'isolates_cleaned_location.csv', here we did a bit more clean up to extract US states and create region:
# Extract Region and US state level:
isolates <- isolates %>% mutate(region = ifelse(substr(location_new,1,3) =='USA', 'USA', as.character(location_new)),
                                US_state = ifelse(substr(location_new, 1, 3) == 'USA', 
                                                  substr(location_new, 5,6), 'unknown'),
                                US_state = ifelse(US_state %in% c('', NA,'unknown'),'unknown', US_state)
)


########## Visualize by region ######### 
# NOTE: The visualization code could be streamlined and function-ized
# by region:
byRegion <- isolates %>% 
  group_by(region) %>% 
  summarise(n = n(), n_strain = n_distinct(Strain)) %>%
  filter(region != 'NA') %>%
  ggplot() +
  geom_bar(aes(x = reorder(region, n), y = n), 
           stat = 'identity', alpha=0.8)  +
  geom_label(aes(x = reorder(region, n), y = n, label = n), alpha=0.5, hjust=0.6)+
  labs(x='Region', y = 'Number of Isolates') +
  coord_flip() 


# by US states:
byState <-  isolates %>% filter(region=='USA') %>%
  group_by(US_state) %>% 
  summarise(n = n(), n_strain = n_distinct(Strain)) %>%
  filter(US_state != 'unknown') %>%
  ggplot() +
  geom_bar(aes(x = reorder(US_state, n), y = n), 
           stat = 'identity', alpha=0.8)  +
  labs(x='US States', y = 'Number of Isolates') +
  coord_flip() +
  theme(axis.text = element_text(size = 7))

grid.arrange(byRegion, byState, ncol=2, top=textGrob("Figure: Number of Isolates by Region and US States (NA removed)"))




######### Visualize by Year and Region: ######### 
# Add year_grp to differentiate before and after 2010 (the actual cut-off should be May 2010 though)
isolates <- isolates %>% mutate(year = (substr(Collection.date,1,4)), year_grp = ifelse(year<=2010, 'prior2010', 'post2010'))

# Isolates over year since 2010:
fig_byregion <- isolates %>% 
  group_by(region,year_grp, year) %>% 
  summarise(n = n(), n_strain = n_distinct(Strain)) %>%
  filter(region != 'NA' & !is.na(year) & year_grp == 'post2010') %>%
  ggplot() +
  geom_bar(aes(x = year, y = n, fill=region), stat = 'identity', alpha=0.8) +
  labs(x='Collection Year', y = 'Number of Isolates',  title='Number of Isolates Over Year by Region')

# YoY trend by region
fig_yoybyregion <- isolates %>% 
  group_by(region,year_grp, year) %>% 
  summarise(n = n(), n_strain = n_distinct(Strain)) %>%
  filter(region != 'NA' & !is.na(year) & year_grp == 'post2010') %>%
  ggplot() +
  geom_line(aes(x=year, y = n, group = region, color=region)) +
  geom_point(aes(x=year, y = n, group = region, color=region)) +
  labs(x='Collection Year', y = 'Number of Isolates', title='Year over Year Trend by Region')

grid.arrange(fig_byregion, fig_yoybyregion, nrow=2, top=textGrob("Figure: Number of Isolates by Region (NA removed)"))

# Monthly trend by region:
fig_MoMbyregion <- isolates %>% 
  filter(region != 'NA' & !is.na(collection.mon) & !is.na(year) & year_grp == 'post2010') %>%
  group_by(region,year_grp, collection.mon) %>% 
  summarise(n = n(), n_strain = n_distinct(Strain)) %>%
  ggplot() +
  geom_line(aes(x=as.factor(collection.mon), y = n, group = region, color=region)) +
  geom_point(aes(x=as.factor(collection.mon), y = n, group = region, color=region)) +
  labs(x='Collection Month', y = 'Number of Isolates', title='Number of Isolates by Month and Region (2010-2022)')

fig_MoMbyregion



# We can also create a historical plot (by month and year):
fig_historybyregion <- isolates %>% 
  filter(region != 'NA' & !is.na(collection.mon) & !is.na(year) & year_grp == 'post2010') %>%
  group_by(region, date.impute,collection.mon) %>% 
  summarise(n = n(), n_strain = n_distinct(Strain)) %>%
  ggplot() +
  geom_line(aes(x=date.impute, y = n, group = region, color=region)) +
  labs(x='Collection Date', y = 'Number of Isolates', title='Historical Number of Isolates by Region')

fig_historybyregion
grid.arrange(fig_MoMbyregion, fig_historybyregion, nrow=2)


######### YoY trend by US states #########
# create list of states that have highest N_isolates:
highest_state <- byStatestbl %>% arrange(desc(n)) %>% slice_max(n=11, order_by = n) %>% select(US_state)

# Plot only the higest_state:
# Bar chart: show proportion:
fig_states_bar <- isolates %>%
  filter(region=='USA' &US_state != 'unknown' & !is.na(year) & year_grp == 'post2010' 
         & US_state %in% highest_state$US_state) %>%
  group_by(US_state,year_grp, year) %>% 
  summarise(n = n(), n_strain = n_distinct(Strain)) %>%
  ggplot() +
  geom_bar(aes(x = year, y = n, fill=US_state), stat = 'identity', alpha=0.8) +
  labs(x='Collection Year', y = 'Number of Isolates',  title='Number of Isolates Over Year by States')

# Line chart: show trend over time.
fig_states_line <- isolates %>% 
  filter(region=='USA' &US_state != 'unknown' & !is.na(year) & year_grp == 'post2010' 
         & US_state %in% highest_state$US_state) %>%
  group_by(US_state,year_grp, year) %>% 
  summarise(n = n(), n_strain = n_distinct(Strain)) %>%
  ggplot() +
  geom_line(aes(x=year, y = n, group = US_state, color=US_state)) +
  geom_point(aes(x=year, y = n, group = US_state, color=US_state)) +
  labs(x='Collection Year', y = 'Number of Isolates', title='Year over Year Trend by States')

grid.arrange(fig_states_bar, fig_states_line, nrow=2, top=textGrob("Figure: Number of Isolates by US States (NA removed)"))





##################################################################
############## Missing pattern for isolation source ##############
##################################################################

########################
#Load Libraries
library(lubridate)
library(tidyverse)
library(forcats)
source("~/Downloads/DataClean_IsolationSource_Level1.R")

# Add strata column:
isolates <- isolates %>% 
  mutate(testt_iso, 
         missing_source = ifelse(is.na(testt_iso) | testt_iso=='Missing', 1,0))

CTO <- CreateTableOne(data=isolates, vars=c('region', 'Min.diff','Min.same'), strata ='missing_source')


kableone(CTO, caption='Variable Comparison by Sissing Source')  %>% kable_styling()



