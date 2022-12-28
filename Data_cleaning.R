#Run this whole document before running any of the graphing scripts

###INSTALL PACKAGES AND LIBRARIES#####
# install.packages("readxl")
# install.packages("dplyr")
# install.packages("writexl")
# install.packages("haven")
# install.packages("ggplot2")
# install.packages("grid")
# install.packages("reshape")
# install.packages("gsubfn")
# install.packages("ggrepel")
library("readxl") #for read xl
library("dplyr")
library("writexl") 
library("haven")
library(ggplot2)
library(grid)
library(reshape) #for melt
library(scales) # for percent
library(forcats) #for reorder
library(rlang)
library(gsubfn) #for list
library(ggrepel)


#######IMPORT DATA######

#AHRQ DATA
#Data downloaded directly from https://www.ahrq.gov/sdoh/data-analytics/sdoh-data.html and used 2016-2020 data files
ahrq_sdoh_2020 <- read_excel("./data_files/AHRQ_SDOH/ARHQ_SDOH_2020_county.xlsx")
ahrq_sdoh_2019 <- read_excel("./data_files/AHRQ_SDOH/ARHQ_SDOH_2019_county.xlsx")
ahrq_sdoh_2018 <- read_excel("./data_files/AHRQ_SDOH/ARHQ_SDOH_2018_county.xlsx")
ahrq_sdoh_2017 <- read_excel("./data_files/AHRQ_SDOH/ARHQ_SDOH_2017_county.xlsx")
ahrq_sdoh_2016 <- read_excel("./data_files/AHRQ_SDOH/ARHQ_SDOH_2016_county.xlsx")

#COUNTY HEALTH RANKINGS DATA
#Data downloaded directly from https://www.countyhealthrankings.org/explore-health-rankings/oregon/data-and-resources and used the "Ranked Measure Data" sheet for 2016-2022
chr_2022 <- read_excel("./data_files/County_Health_Rankings/Measure_Data_2022_County_Health_Rankings.xlsx")
chr_2021 <- read_excel("./data_files/County_Health_Rankings/Measure_Data_2021_County_Health_Rankings.xlsx")
chr_2020 <- read_excel("./data_files/County_Health_Rankings/Measure_Data_2020_County_Health_Rankings.xlsx")
chr_2019 <- read_excel("./data_files/County_Health_Rankings/Measure_Data_2019_County_Health_Rankings.xlsx")
chr_2018 <- read_excel("./data_files/County_Health_Rankings/Measure_Data_2018_County_Health_Rankings.xlsx")
chr_2017 <- read_excel("./data_files/County_Health_Rankings/Measure_Data_2017_County_Health_Rankings.xlsx")
chr_2016 <- read_excel("./data_files/County_Health_Rankings/Measure_Data_2016_County_Health_Rankings.xlsx")

#OHA CHRONIC CONDITIONS DATA
#Data downloaded directly from https://www.oregon.gov/oha/PH/DISEASESCONDITIONS/CHRONICDISEASE/DATAREPORTS/Pages/Adult-Prevalence.aspx (click "Adult Chronic Conditions")
oha_chronic <- read_excel("./data_files/OHA/OHA_chronic_conditions_BRFSS_county.xlsx")


######NON-GRAPH FUNCTIONS######
#Combines CHR annual datasets and selects relelvant variables
CHR_clean <- function(df, year){
  colnames(df) <- gsub("#","Number",colnames(df))
  colnames(df) <- gsub("%","Percent",colnames(df))
  df$County[1] <- "Oregon"
  df %>%
    select(-contains(c("95","Z-Score","Unreliable","Medicare"))) %>% #the - selects all columns that don't start with these.
    select(-contains(c("Black","White","Hispanic","Asian","AIAN"))) %>%
    mutate("Year" = as.numeric(year), .after = FIPS) %>% #as.numeric(year) so that the year is stored as a number (vs char) and works with the continuous ggplot calls
    rename_with(make.names)
}

#Rename CHR columns to be consistent between the older (2016-2019) and newer datasets (2020-2022)
#Defaulted to newer names because they're more intuitive
CHR_change_names <- function(df) {
  df %>%
    dplyr::rename(Percent.Fair.or.Poor.Health = Percent.Fair.Poor,
                  Average.Number.of.Physically.Unhealthy.Days = Physically.Unhealthy.Days,
                  Average.Number.of.Mentally.Unhealthy.Days = Mentally.Unhealthy.Days,
                  Percent.Adults.with.Obesity = Percent.Obese,
                  Percent.With.Access.to.Exercise.Opportunities = Percent.With.Access, 
                  Percent.Driving.Deaths.with.Alcohol.Involvement = Percent.Alcohol.Impaired,
                  Primary.Care.Physicians.Rate = PCP.Rate,
                  Primary.Care.Physicians.Ratio = PCP.Ratio,
                  Mental.Health.Provider.Rate = MHP.Rate,
                  Mental.Health.Provider.Ratio = MHP.Ratio,
                  Preventable.Hospitalization.Rate = Preventable.Hosp..Rate,
                  Social.Association.Rate = Association.Rate,
                  Presence.of.Water.Violation = Presence.of.violation,
                  Percent.Drive.Alone.to.Work = Percent.Drive.Alone
    )
}

#Function to calculate the total population growth rate for data from AHRQ dataset
growth_rate <- function(df){
  df %>%
    arrange(YEAR) %>%
    mutate(growth_rate = 100*(ACS_TOT_POP_WT - lag(ACS_TOT_POP_WT))/lag(ACS_TOT_POP_WT)) #lag takes the entry before to calaculate change since prior year
}

#Function to get the percentages on a state level with AHRQ dataset
state_perc_totals <- function(cols_needed, pop_column){
  ahrq_sdoh_full %>%
    filter(YEAR==2020) %>%
    #this first calculates a total population (total_pop)
    #for all of the columns designated, it then divides each value by 100 to get the percentage in decimal format, and multiplies it by the population to get the number of affected people
    #and then sums the number of people for each county to get the total number for the state
    summarise(total_pop = sum({{pop_column}}), across(c(all_of(cols_needed)), 
                                                      ~ sum((.x/100)*{{pop_column}}))) %>%
    #finally take the total number of people affected in the state in cols_needed (calculated in previous lines, indicated as .x) and divides it by the total_pop
    #this gives the decimal format, multiply by 100 to get back to percentage format. Do this for all columns
    summarise(across(c(cols_needed),
                     ~(.x/total_pop))*100)
  #syntax:
  #the double curly brackets indicate that pop_column is a variable not the name of the column
  #the tilde connects the assigned columns and the command and the .x is all of the cells in each cols_needed
}

#Makes df for CHR data to be used in bar charts with percentages. Can take both county and state df inputs. Year is desired year to calc/visualize.
chr_perc_bar <- function(df, col_names, year){
  df <- df[c("Year",col_names)] %>%
    filter(Year==year)
  df <- df[,2:ncol(df)] #remove the first column because it now has the year isn't needed
  df
}

#Function to extract the number of people affected from percentages to include as text in bar charts
pop_num_func <- function(df, pop){
  pop_num <- df %>%
    filter(YEAR == "2020") %>%
    select(!!pop)
  pop_num <- pop_num[[1,1]]
  return(pop_num)
}

#Function to make a df for the county with desired variables from AHRQ dataset
county_perc_df <- function(col_names){
  df <- ahrq_county[c("YEAR",col_names)] %>%
    filter(YEAR==2020)
  df <- df[,2:ncol(df)] #remove the first column because it now has the year which we don't need
  df
}

#Function to transform data in the format of columns with one row of percent info
quality_transform <- function(df,desired_column_names,category_name){
  df <- stack(df)
  df$category <- c(desired_column_names) #adds a new column with desired names
  df <- df[,c(3,1,2)] #swap column order
  colnames(df) <- c(category_name,"Values",paste("Old",category_name))
  df
}

#Function to transform the individual data (state and county) and combine them
quality_transform_combo <- function(df1,df2,desired_column_names,category_name){
  df1 <- quality_transform(df1,desired_column_names,category_name)
  df2 <- quality_transform(df2,desired_column_names,category_name)
  combo_df <- merge(df1[c(category_name,"Values")],df2[c(category_name,"Values")],by = category_name,
                    suffixes = c(" Oregon",paste("",county,"County")))
  combo_df <- melt(combo_df, id=c(category_name))
}

#Function to make a df for the county across years (2016-2020) for percentage data (AHRQ)
county_perc_acrossyears <- function(cols_needed){
  df <- ahrq_county %>%
    select(YEAR,cols_needed)
  colnames(df) <- c("Year","value")
  return(df)
}

#Function to make a df for the state across years (2016-2020) for percentage data (AHRQ)
#Same logic as state_perc_totals, but group by year to do this for each year, instead of filtering by one year
state_perc_acrossyears <- function(cols_needed, pop_column){
  df <- ahrq_sdoh_full %>%
    group_by(YEAR) %>%
    #first multiply the percentages by the total population and sum accross counties to get total people in state
    summarise(total_pop = sum({{pop_column}}), across(c(all_of(cols_needed)),
                                                      ~ sum((.x/100)*{{pop_column}}))) %>%
    #divide by sum of population across state to get percentage
    summarise(across(c(cols_needed),
                     ~(.x/total_pop))*100) %>%
    mutate(YEAR = c(2016, 2017, 2018, 2019, 2020)) %>%
    relocate(YEAR)
  colnames(df) <- c("Year","value")
  return(df)
}

#Function to make a df for CHR multi-year percentage data. Due to data format, works with state and county dfs
chr_perc_acrossyears <- function(df, var){
  new_df <- df %>%
    select(Year,var)
  colnames(new_df) <- c("Year","value")
  return(new_df)
}

#Function to combine 2 dfs across years into a graphable format
quality_transform_combo_acrossyears <- function(df1,df2){
  combo_df <- merge(df1[c("Year","value")],df2[c("Year","value")],by = "Year",
                    suffixes = c(paste(county,"County"),"Oregon"))
  combo_df <- melt(combo_df, id=c("Year"))
  combo_df$variable <- substr(combo_df$variable,6, 500) #removes prefix of "Values" to only include state/county, 500 is arbitrary
  combo_df
}

#Function to make a df for the county across years (2016-2020) for non-percentage data (AHRQ)
county_val_acrossyears <- function(cols_needed){
  df <- ahrq_county %>%
    select(YEAR,cols_needed)
  colnames(df) <- c("Year","value")
  return(df)
}

#Function to make a df for the state across years (2016-2020) for non-percentage data (AHRQ)
state_val_acrossyears <- function(cols_needed, pop_column){
  df <- ahrq_sdoh_full %>%
    group_by(YEAR) %>%
    summarise(total_pop = sum(!!cols_needed)) %>%
    mutate(YEAR = c(2016, 2017, 2018, 2019, 2020)) %>%
    relocate(YEAR)
  colnames(df) <- c("Year","value")
  return(df)
} #not using this

#OHA chronic conditions dataset functions - function to make chronic conditions dataframe
oha_chronic_df <- function(county,risk_factors){
  df <- oha_chronic %>%
    filter(County.name %in% c(county, "Oregon State"),
           Disease.or.risk.factor %in% risk_factors,
           BRFSS.Year == "2016 to 2019") %>%
    arrange(County.name) %>%
    select(County.name, Disease.or.risk.factor, Number.of.adults, Percent.of.adults)
  df
}

#function to combine dfs on matching column names (it excludes any that are not included in both)
#note: this is the only function that I didn't write
rbind.match.columns <- function(input1, input2) {
  n.input1 <- ncol(input1)
  n.input2 <- ncol(input2)
  
  if (n.input2 < n.input1) {
    TF.names <- which(names(input2) %in% names(input1))
    column.names <- names(input2[, TF.names])
  } else {
    TF.names <- which(names(input1) %in% names(input2))
    column.names <- names(input1[, TF.names])
  }
  
  return(rbind(input1[, column.names], input2[, column.names]))
  
}

######GRAPHING FUNCTIONS#####

#GRAPH COMPONENTS
#Only used these in a few graphs, wasn't as convenient/effective as I'd hoped, still keeping
g_line <- geom_line(size = 0.4, linetype = 1, color = line_color, alpha = 0.6)
g_theme <- theme(axis.title.y = element_blank(),
                 axis.title.x = element_text(size = 14),
                 axis.text = element_text(size=12), 
                 plot.title=element_text(size=20))
g_theme_combo <- g_theme + theme(legend.title = element_blank())
g_scale_y_perc <- scale_y_continuous(labels = percent_format(scale = 1, accuracy = 0.1), #accuracy is how many decimals
                                     expand = expansion(mult=(c(0.02,0.15))))

#function to save each graph (find text options in "things for all graphs" section)
ggsave_graph <- function(section, g_title, text){
  ggsave(paste(paste("graphs/",county,"/",section,"/",g_title, sep = ""),county,text),width = 15, height = 6)
}


#BAR CHARTS
#Function to make a bar plot of multiple categories in percentage form for one entity (one county or full state)
graph_county_perc <- function(df, category_name, col_names, graph_title, graph_subtitle, graph_caption, year, section){
  df %>%
    ggplot(aes(x=!!category_name, y=Values)) + 
    geom_bar(stat = "identity", color = "black", fill = county_color) +
    labs(y= "Percent", title = paste(county,"County:",graph_title, "in", year),
         subtitle = graph_subtitle, caption = graph_caption) +
    theme(axis.title = element_blank(), 
          axis.text = element_text(size=12), plot.title=element_text(size=20)) +
    coord_flip() +
    scale_y_continuous(labels = percent_format(scale = 1),expand = expansion(mult=(c(0.02,0.1))))+
    xlim(col_names)+
    geom_label(aes(label=percent(Values/100,accuracy =0.01)),hjust = -0.06)
  ggsave_graph(section, g_title, county_text_save)
}

#Function to make a bar plot of multiple categories in percentage form for multiple entities (state vs. county)
graph_combo_perc <- function(df, category_name, col_names, graph_title, graph_subtitle, graph_caption, year, section){
  df %>%
    ggplot(aes(x=!!category_name, y=value, fill = variable)) +
    geom_bar(stat = "identity", position ="dodge") +
    labs(y= "Percent", 
         title = paste("Oregon and", county,"County:",graph_title, "in", year), 
         fill = "",
         subtitle = graph_subtitle, 
         caption = graph_caption) +
    theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
          plot.title=element_text(size=20),axis.text = element_text(size=12, color = "black")) +
    scale_y_continuous(labels = percent_format(scale = 1),expand = expansion(mult=(c(0.02,0.1)))) +
    xlim(col_names) + 
    scale_fill_manual(labels=c(paste(county,"County"),"Oregon"), values = c(county_color,oregon_color)) +
    geom_label(aes(!!category_name, label=percent(value/100, accuracy =0.01),group = variable),fill = "white", position = position_dodge(width = 1), vjust = -0.1, size = 4)
  ggsave_graph(section, g_title, combo_text_save)
}

#Function to make a bar plot of multiple categories in percentage form for one entity (one county or full state) and includes the number of people affected in text format
graph_county_perc_num_text <- function(df, category_name, col_names, graph_title, graph_subtitle, graph_caption, year, pop, section){
  df %>%
    ggplot(aes(x=!!category_name, y=Values)) + 
    geom_bar(stat = "identity", color = "black", fill = county_color) +
    labs(y= "Percent", title = paste(county,"County:",graph_title, "in", year),
         subtitle = graph_subtitle, caption = graph_caption) +
    theme(axis.title = element_blank(), 
          axis.text = element_text(size=12), plot.title=element_text(size=20)) +
    coord_flip() +
    scale_y_continuous(labels = percent_format(scale = 1),expand = expansion(mult=(c(0.02,0.1))))+
    xlim(col_names)+
    geom_label(aes(label=percent(Values/100,accuracy =0.01)),hjust = -0.06) +
    geom_text(aes(label = paste(comma(Values*0.01*pop),"people"), y = 0.2),
              color = "white", hjust = 0)
  ggsave_graph(section, g_title, county_num_text_save)
}

#Function to make a bar plot of multiple categories in non-percentage form for one entity (one county or full state)
#This one has the "no data available" text and I love it
graph_county_val <- function(df, category_name,col_names,graph_title, graph_subtitle, graph_caption, accu, year, xaxis, section){
  df %>%
    ggplot(aes(x=!!category_name, y=Values)) + 
    geom_bar(stat = "identity", color = "black", fill = county_color) +
    labs(title = paste(county,"County",graph_title, "in", year),
         subtitle = graph_subtitle, caption = graph_caption) +
    ylab(xaxis) + #this is because of coord flip, but it's a brain-bender
    theme(axis.title.y = element_blank(), 
          axis.text = element_text(size=12), 
          plot.title=element_text(size=20)) +
    coord_flip() +
    scale_y_continuous(label = comma_format(accuracy = accu),
                       expand = expansion(mult=(c(0.02,0.1))))+
    xlim(col_names) +
    geom_label(aes(label=comma(Values, accuracy = accu)),
               hjust = -0.3)+
    geom_text(aes(label = ifelse(is.na(Values), "No data available", ""), y = 3), col = "black", hjust = 0)
  ggsave_graph(section, g_title, county_text_save)
}

#Function to make a bar plot of multiple categories in dollar form for one entity (one county or full state)
graph_county_val_dollar <- function(df, category_name, col_names, graph_title, graph_subtitle, graph_caption, xaxis, section){
  df %>%
    ggplot(aes(x=!!category_name, y=Values)) + 
    geom_bar(stat = "identity", color = "black", fill = county_color) +
    labs(title = paste(county,"County",graph_title, "in 2020"),
         subtitle = graph_subtitle, caption = graph_caption) +
    ylab(xaxis)+
    theme(axis.title.y = element_blank(), 
          axis.text = element_text(size=12), 
          plot.title=element_text(size=20)) +
    coord_flip() +
    scale_y_continuous(labels = dollar_format(scale = 1),expand = expansion(mult=(c(0.02,0.1))))+
    xlim(col_names) +
    geom_label(aes(label=scales::dollar(Values, 1)),hjust = -0.06)+
    geom_text(aes(label = ifelse(is.na(Values), "No data available", ""), y = 3), col = "black", hjust = 0)
  ggsave_graph(section, g_title, county_text_save)
}

#Function to make bar plot with multiple categories of the county only, for the OHA chronic conditions dataset specifically
#Includes total number of people affected, which is cool
graph_county_perc_oha_chronic <- function(df, graph_title, graph_caption, section){
  df %>%
    ggplot(aes(x=Disease.or.risk.factor, y=Percent.of.adults)) + 
    geom_bar(stat = "identity", color = "black", fill = county_color) +
    labs(y= "Percent", 
         title = paste(county,"County:",graph_title, "(2016-2019)"),
         caption = graph_caption) +
    theme(axis.title = element_blank(), 
          axis.text = element_text(size=12), 
          plot.title=element_text(size=20)) +
    coord_flip() +
    scale_y_continuous(labels = percent_format(scale = 1),
                       expand = expansion(mult=(c(0.02,0.1))))+ 
    geom_label(aes(label=percent(Percent.of.adults/100,accuracy =0.01)),
               hjust = -0.06) +
    geom_text(aes(label = paste(comma(Number.of.adults),"people"), y = 0.5),col = "white", hjust = 0)
  
  ggsave_graph(section, g_title, county_text_save)
}

#Function to make a bar plot of Oregon and county from OHA chronic conditions dataset
graph_combo_perc_oha_chronic <- function(df, graph_title, graph_caption, section){
  df %>%
    ggplot(aes(x=Disease.or.risk.factor, y=Percent.of.adults, fill = County.name)) +
    geom_bar(stat = "identity", position ="dodge") +
    labs(y= "Percent", 
         title = paste("Oregon and", county,"County: Percentage of",graph_title, "(2016-2019)"), 
         fill = "",
         caption = graph_caption) +
    theme(axis.title.y = element_blank(), 
          axis.title.x = element_blank(),
          plot.title=element_text(size=20),
          axis.text = element_text(size=12, color = "black"),
          axis.text.x=element_text(angle = 45, hjust = 1)) +
    scale_y_continuous(labels = percent_format(scale = 1),expand = expansion(mult=(c(0.02,0.1)))) + 
    scale_fill_manual(labels=c(paste(county,"County"),"Oregon"), values = c(county_color,oregon_color)) +
    geom_label(aes(Disease.or.risk.factor, label=percent(Percent.of.adults/100, accuracy =0.01),group = County.name),fill = "white", position = position_dodge(width = 1), vjust = -0.1, size = 4)
  ggsave_graph(section, g_title, combo_text_save)
}

#LINE CHARTS
#Function to make line graph over time for the county, single variable in percentage format
graph_county_perc_acrossyears <- function(df,graph_title, graph_subtitle, graph_caption, years, yaxis, section){
  df %>%
    ggplot(aes(x=Year, y=value)) +
    g_line +
    geom_point(shape=21,
               fill=county_color,
               size=4) +
    g_scale_y_perc +
    scale_x_continuous(breaks = seq(df$Year[1],df$Year[length(df$Year)],1), 
                       expand = expansion(mult=c(0.05,0.05)))+
    geom_label(aes(label=percent(value/100,accuracy = 0.01)),
               vjust = -0.5, 
               color = "black",
               fontface = "bold",
               cex = 5)+
    theme(axis.title.x = element_text(size = 13),
          axis.title.y = element_text(size = 13),
          axis.text = element_text(size=12),
          plot.title=element_text(size=20))+
    ylab(yaxis)+
    labs(title = paste(county, "County: Percentage of", graph_title, years),
         subtitle = graph_subtitle ,
         caption = graph_caption)
  ggsave_graph(section, g_title, county_text_save)
}

#Function to make line graph over time for the county and state, single variable in percentage format
graph_combo_perc_acrossyears <- function(df,graph_title, graph_subtitle, graph_caption, years, yaxis, section){
  df %>%
    ggplot(aes(x=Year, y=value, fill = variable)) +
    g_line +
    geom_point(shape=21,
               size=4) + 
    g_scale_y_perc +
    scale_x_continuous(breaks = seq(df$Year[1],df$Year[length(df$Year)/2],1),
                       expand = expansion(mult=c(0.05,0.05)))+
    geom_label(aes(label= percent(value/100,accuracy = 0.01)),
               vjust = -0.5,
               color = "black",
               fill = "white",
               fontface = "bold",
               cex = 5)+
    scale_fill_manual(values = c(county_color,oregon_color))+
    theme(axis.title.x = element_text(size = 13),
          axis.title.y = element_text(size = 13),
          axis.text = element_text(size=12),
          plot.title=element_text(size=20),
          legend.title = element_blank())+
    ylab(yaxis)+
    labs(title = paste("Oregon and", county, "County: Percentage of", graph_title, years),
         subtitle = graph_subtitle,
         caption = graph_caption)
  ggsave_graph(section, g_title, combo_text_save)
}


#Function to make line graph over time for the county, single variable in non-percentage format
graph_county_val_acrossyears <- function(df,graph_title, graph_subtitle, graph_caption, accu, years, yaxis, section){
  df %>%
    ggplot(aes(x=Year, y=value)) +
    g_line +
    geom_point(shape=21, 
               color="black", 
               fill=county_color, 
               size=4) + 
    scale_y_continuous(label = comma_format(accuracy = accu), 
                       expand = expansion(mult=(c(0.02,0.15)))) + 
    scale_x_continuous(breaks = seq(df$Year[1],df$Year[length(df$Year)],1),
                       expand = expansion(mult=c(0.05,0.05)))+
    geom_label(aes(label=comma(value, accuracy = accu)),
               vjust = -0.5, 
               color = "black",
               fontface = "bold",
               cex = 5)+
    theme(axis.title.x = element_text(size = 13),
          axis.title.y = element_text(size = 13),
          axis.text = element_text(size=12),
          plot.title=element_text(size=20))+
    ylab(yaxis)+
    labs(title = paste(county, "County", graph_title, years),
         subtitle = graph_subtitle ,
         caption = graph_caption)
  ggsave_graph(section, g_title, county_text_save)
}

#Function to make line graph over time for the county, single variable in dollar format
graph_county_val_acrossyears_dollar <- function(df,graph_title, graph_subtitle, graph_caption, yaxis, section){
  df %>%
    ggplot(aes(x=Year, y=value)) +
    g_line +
    geom_point(shape=21, 
               color="black", 
               fill=county_color, 
               size=4) +
    scale_y_continuous(label = dollar_format(scale = 1),
                       expand = expansion(mult=(c(0.02,0.15)))) + 
    scale_x_continuous(expand = expansion(mult=c(0.05,0.05)))+
    geom_label(aes(label=dollar(value, 1)),
               vjust = -0.5, 
               color = "black",
               fontface = "bold",
               cex = 5)+
    theme(axis.title.x = element_text(size = 13),
          axis.title.y = element_text(size = 13),
          axis.text = element_text(size=12),
          plot.title=element_text(size=20))+
    ylab(yaxis)+
    labs(title = paste(county, "County", graph_title, "(2016-2020)"),
         subtitle = graph_subtitle ,
         caption = graph_caption)
  ggsave_graph(section, g_title, county_text_save)
}

##Function to make line graph over time for the county and state, single variable in non-percentage format
#Only using for CHR data currently
graph_combo_val_acrossyears <- function(df,graph_title, graph_subtitle, graph_caption, accu, years, yaxis, section){
  df %>%
    ggplot(aes(x=Year, y=value, fill = variable)) +
    g_line +
    geom_point(shape=21,
               size=4) + 
    scale_y_continuous(label = comma_format(accuracy = accu),
                       expand = expansion(mult=(c(0.02,0.15)))) + 
    scale_x_continuous(breaks = seq(df$Year[1],df$Year[length(df$Year)/2],1),
                       expand = expansion(mult=c(0.05,0.05)))+
    scale_fill_manual(values = c(county_color,oregon_color))+
    geom_label(aes(label=comma(value, accuracy = accu)),
               vjust = -0.5, 
               color = "black",
               fill = "white",
               fontface = "bold",
               cex = 5)+
    theme(axis.title.x = element_text(size = 13),
          axis.title.y = element_text(size = 13),
          axis.text = element_text(size=12),
          plot.title=element_text(size=20),
          legend.title = element_blank())+
    ylab(yaxis)+
    labs(title = paste("Oregon and", county, "County", graph_title, years),
         subtitle = graph_subtitle,
         caption = graph_caption)
  ggsave_graph(section, g_title, combo_text_save)
}

#Graphing function notes / syntax reminders
#scale_x_continuous(breaks = seq(df$Year[1],df$Year[length(df$Year)],1), #this sets the x axis markers to include every year. Looks complex, but is just setting breaks to first and last year with an interval of 1
#                                      expand = expansion(mult=c(0.05,0.05))) #this formats the extra space on either side of the x axis (left, right)

# geom_point(shape=21,
#            size=4) + #size controls the inside size, stroke (not used here) controls the border

# scale_y_continuous(labels = percent_format(scale = 1, accuracy = 0.1), #accuracy is how many decimals

# scale_y_continuous(labels = percent_format(scale = 1),expand = expansion(mult=(c(0.02,0.1)))) + #this formats the axis as %s (percent_format) and adjusts the space at the beginning and end of the plot (first number is left, second is right)
  

# geom_label(aes(label=percent(value/100,accuracy = 0.01)),
#            vjust = -0.5, 
#            color = "black",
#            fontface = "bold",
#            cex = 5)+ #cex ajusts label size but scales oddly
                   
          
#xlim(col_names)+ #this is what sets the factor order (and thus the order the bars show up in) for the bar chart         

#geom_text(aes(label = paste(comma(Values*0.01*pop),"people"), y = 0.2), #the y controls where the text is in the bar, smaller number -> left, bigger number -> right

#####DATA PROCESSING#####
#Combining the AHRQ Oregon data into one DF and subsetting for Oregon
ahrq_sdoh_full <- rbind.match.columns(ahrq_sdoh_2019,ahrq_sdoh_2020)
ahrq_sdoh_full <- rbind.match.columns(ahrq_sdoh_full,ahrq_sdoh_2018)
ahrq_sdoh_full <- rbind.match.columns(ahrq_sdoh_full,ahrq_sdoh_2017)
ahrq_sdoh_full <- rbind.match.columns(ahrq_sdoh_full,ahrq_sdoh_2016)
ahrq_sdoh_full <- subset(ahrq_sdoh_full, STATE == "Oregon")

#Apply the CHR clean function to all years of the CHR data
#Also apply the change names function for years 2016-2019 due to changed naming conventions
chr_2016 <- CHR_clean(chr_2016, "2016")
chr_2016 <- CHR_change_names(chr_2016)

chr_2017 <- CHR_clean(chr_2017, "2017")
chr_2017 <- CHR_change_names(chr_2017)

chr_2018 <- CHR_clean(chr_2018, "2018")
chr_2018 <- CHR_change_names(chr_2018)

chr_2019 <- CHR_clean(chr_2019, "2019")
chr_2019 <- CHR_change_names(chr_2019)

#Don't need to change names for these ones
chr_2020 <- CHR_clean(chr_2020, "2020")
chr_2021 <- CHR_clean(chr_2021, "2021")
chr_2022 <- CHR_clean(chr_2022, "2022")

#combine the CHR data across years into one df
chr_full <- rbind.match.columns(chr_2016,chr_2017)
chr_full <- rbind.match.columns(chr_full,chr_2018)
chr_full <- rbind.match.columns(chr_full,chr_2019)
chr_full <- rbind.match.columns(chr_full,chr_2020)
chr_full <- rbind.match.columns(chr_full,chr_2021)
chr_full <- rbind.match.columns(chr_full,chr_2022)

#Cleaning and preparing OHA chronic conditions datasets
oha_chronic <- oha_chronic %>%
  rename_with(make.names) %>%
  filter(Reporting.method == "Unadjusted",
         Demographic.group == "Total",
         Demographic.label == "Overall") %>%
  mutate(Number.of.adults = as.numeric(Number.of.adults), #these as.numeric change these numbers from characters into integers for graphing
         Percent.of.adults = as.numeric(Percent.of.adults))


####THINGS NEEDED FOR MANY GRAPHS AND FUNCTIONS#####
county_color <- "#332288" #purple
oregon_color <- "#999933" #olive
line_color <- "#5A5A5A" #dark grey
ahrq_caption <- "Source: Agency for Healthcare Research and Quality Social Determinants of Health Database (2016-2020)" 
chr_caption <- "Source: County Health Rankings Data (2016-2022)"
oha_chronic_caption <- "Source: Oregon Health Authority Chronic Conditions Dataset (2016-2019)"
ahrq_years <- "(2016-2020)"
chr_years <- "(2016-2022)"
five_and_up <- sym("ACS_TOT_POP_ABOVE5")
total_pop <- sym("ACS_TOT_POP_WT")
eighteen_plus <- sym("ACS_TOT_CIVIL_POP_ABOVE18")
sixteen_plus <- sym("ACS_TOT_POP_ABOVE16")
twenty_five_plus <- sym("ACS_TOT_POP_ABOVE25")
num_households <- sym("ACS_TOT_HH")
num_housing_units <- sym("ACS_TOT_HU")
accu_whole <- 1
accu_decimal <- 0.01
section_demographics <- "demographics"
section_phys_env <- "physical_environment"
section_housing <- "housing"
section_econ <- "economics_labor"
section_education <- "education"
section_food <- "food"
section_healthcare <- "healthcare_system"
section_behaviors <- "health_behaviors"
section_outcomes <- "health_outcomes"
county_text_save <- "County.pdf"
county_num_text_save <- "County w num.pdf"
combo_text_save <- "County and Oregon.pdf"


########DESIGNATE THE COUNTY OF INTEREST - CHANGE HERE & RUN#######
#NOTE: Only Oregon counties work right now. Input the county name only (e.g., "Curry" not "Curry County")
county <- "Curry"

#creates the corresponding dfs for the county - RUN THESE AFTER COUNTY CHANGE
ahrq_county <- ahrq_sdoh_full %>%
  filter(COUNTY == paste(county, "County")) %>%
  arrange(YEAR)

#CHR df for the county
chr_county <- chr_full %>%
  filter(County == county) %>%
  arrange(Year)

#CHR df for the whole state (this doesn't change)
chr_oregon <- chr_full %>%
  filter(County == "Oregon") %>%
  arrange(Year)
