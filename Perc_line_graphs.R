#LINE GRAPHS, PERCENTAGES

# Run all of Data_cleaning first
# Next run the block of interest under either AHRQ or CHR data
# Then, scroll to the bottom of the page and run all data processing and graphing functions for the indicated data source
# Graphs auto-save, to view them directly in R comment out the ggsave line at the bottom of the graphing function located in Data_cleaning

##---------AHRQ DATA------------------#

#PERCENT OF POPULATION THAT SPEAKS SPANISH
cols <- "ACS_PCT_SPANISH"
population <- five_and_up
g_title <- "Spanish Speaking Population"
g_subtitle <- "Percentage of population that speaks Spanish (ages 5 and over)."
section <- section_demographics

#Single parent families
#Don't include the state/county comparison, no accurate population to calculate state values
cols <- "ACS_PCT_CHILD_1FAM"
population <- total_pop
g_title <- "Single-Parent Families"
g_subtitle <- "Percentage of families with children that are single-parent families."
section <- section_housing

#Households with one occupant
cols <- sym("ACS_PCT_HH_1PERS")
population <- num_households
g_title <- "Single Occupancy Households"
g_subtitle <- "Percentage of households with only one occupant."
section <- section_housing

#Vacant housing units
cols <- sym("ACS_PCT_VACANT_HU")
population <- num_housing_units
g_title <- "Vacant Housing Units"
g_subtitle <- "Percentage of housing units vacant."
section <- section_housing

#Housing units built before 1979
cols <- sym("ACS_PCT_HU_BUILT_1979")
population <- num_housing_units
g_title <- "Housing Units Built Before 1979"
g_subtitle <- "Percentage of housing units built before 1979."
section <- section_housing

#Percentage of people in poverty
cols <- sym("SAIPE_PCT_POV")
population <- total_pop
g_title <- "People in Poverty"
g_subtitle <- "Estimated percentage of people of all ages in poverty."
section <- section_econ

#Percentage of children in poverty
#Don't include the state/county comparison, no accurate population to calculate state values
cols <- sym("SAIPE_PCT_POV_0_17")
population <- total_pop
g_title <- "Children in Poverty"
g_subtitle <- "Estimated percentage of people aged 0-17 in poverty."
section <- section_econ

#Civilian unemployment
cols <- sym("ACS_PCT_UNEMPLOY")
population <- sixteen_plus
g_title <- "Civilians Unemployed"
g_subtitle <- "Percentage of the civilian labor force that is unemployed (ages 16 and over)."
section <- section_econ

#Civilian employment
cols <- sym("ACS_PCT_EMPLOYED")
population <- sixteen_plus
g_title <- "Civilians Employed"
g_subtitle <- "Percentage of the civilian labor force that is employed (ages 16 and over)."
section <- section_econ

#Households receiving food stamps
cols <- sym("ACS_PCT_HH_FOOD_STMP")
population <- num_households
g_title <- "Households that Receive Food Stamps"
g_subtitle <- "Percentage of households that received food stamps/SNAP, past 12 months."
section <- section_food

#UNINSURED
cols <- sym("ACS_PCT_UNINSURED")
population <- total_pop
g_title <- "Population Without Health Insurance"
g_subtitle <- "Percentage of population with no health insurance coverage."
section <- section_healthcare

#---------CHR DATA----------------#

#Access to exercise opportunities
cols <- sym("Percent.With.Access.to.Exercise.Opportunities")
g_title <- "Population With Access to Exercise Opportunities"
g_subtitle <- "Percentage of the population with access to places for physical activity."
section <- section_phys_env

#Percent of people who drive alone to work
cols <- sym("Percent.Drive.Alone.to.Work")
g_title <- "Workers who Drive Alone to Work"
g_subtitle <- "Percentage of workers who commute in their car, truck or van alone."
section <- section_phys_env

#Percent Smokers
cols <- sym("Percent.Smokers")
g_title <- "Adults that Smoke Cigarettes"
g_subtitle <- "Percentage of adults that reported currently smoking cigarettes."
section <- section_behaviors

#Obesity
cols <- sym("Percent.Adults.with.Obesity")
g_title <- "Adults with Obesity"
g_subtitle <- "Percentage of adults that report BMI of 30 or greater."
section <- section_outcomes

#Physical Inactivity
cols <- sym("Percent.Physically.Inactive")
g_title <- "Adults that are Physically Inactive"
g_subtitle <- "Percentage of adults that report no leisure-time physical activity."
section <- section_behaviors

#Excessive Drinking
cols <- sym("Percent.Excessive.Drinking")
g_title <- "Adults Who Drink Alcohol Excessively"
g_subtitle <- "Percentage of adults that report excessive drinking."
section <- section_behaviors

# ---- DATA PROCESSING AND GRAPHING ----#
#Run for both AHRQ and CHR to set y-axis label
#Alternatively, set y-axis label (yaxis) for a specific graph and then don't run this if needed
yaxis <- paste("%",g_title)


# ----- AHRQ - RUN FOR EVERYTHING ---- #
county_ahrq <-county_perc_acrossyears(cols)
oregon_ahrq <- state_perc_acrossyears(cols, !!population)
combo_ahrq <- quality_transform_combo_acrossyears(county_ahrq,oregon_ahrq)

graph_county_perc_acrossyears(county_ahrq,g_title,g_subtitle,ahrq_caption, ahrq_years, yaxis, section)
graph_combo_perc_acrossyears(combo_ahrq,g_title,g_subtitle,ahrq_caption, ahrq_years, yaxis, section)

#--- CHR - RUN FOR EVERYTHING ----#
county_chr <-chr_perc_acrossyears(chr_county, cols)
oregon_chr <- chr_perc_acrossyears(chr_oregon, cols)

combo_chr <- quality_transform_combo_acrossyears(county_chr,oregon_chr)

graph_county_perc_acrossyears(county_chr,g_title,g_subtitle,chr_caption, chr_years, yaxis, section)
graph_combo_perc_acrossyears(combo_chr,g_title,g_subtitle,chr_caption, chr_years, yaxis, section)
