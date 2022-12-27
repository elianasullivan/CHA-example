# Community Health Assessment (CHA)
This includes code to create graphs and other visualizations for Oregon Community Health Assessments (CHA) along with the graphics produced for Coos and Curry's CHAs. The code can be run for any of Oregon's 36 counties and includes data that spans 2016-2022.

# Creating Graphs
First, set the desired county at the bottom of the data_cleaning document. Then, run the whole data_cleaning document which contains all of the libraries, data import, cleaning, functions, and pre-set variables for the rest of the code.

The graphing code files are named based on the kind of values they use and the type of graph produced (key: val - values, perc - percentages, bar - bar graphs, line - line graphs) or the data source (e.g., OHA chronic conditions). Within each graphing document there are numerous blocks by category. Simply run a whole block, and then run the specified data processing and graphing functions (usually seperated by data source) at the bottom of the page to produce the graphs. The graphs automatically save, so if you want to see them in R, comment out the ggsave function in the relevant graphing funciton. The map data saves csvs into the map_data folder to be mapped in ArcGIS Online. The function_documentation file has additional information about some functions.

# Data
All of the data used in the code are stored in the data_files folder.

# Viewing Graphs
All of the graphics, created in R and otherwise for this project for Coos and Curry counties, are in the graphs folder. They are seperated by county and category for ease.

# Author
This code was written by me, Eliana Sullivan. Reach out to eliana.sullivan23@gmail.com with any questions.
