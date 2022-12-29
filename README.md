# Community Health Assessment
This code is for a Community Health Assessment (CHA) which is an overview of demographics and social determinants of health for counties in Oregon over the past 7 years. The code can be run for any of Oregon's 36 counties and includes data that spans 2016-2022. This portion of the project involves exploring and visualizing publicly available data that lends insight into the population and health of each county. While there have been past CHAs, there is no standard CHA format, so everything from picking the data sources, to visualization types, to writing it in R instead of Excel was my role on this project.

# What is included
All code is written in R. This is a sample of the code I wrote for this project, but still displays most of the techniques / styles that I used. 
- The **data_cleaning** script contains all the libraries, data import, cleaning, functions, and pre-set variables for the rest of the code. This should be run first before any of the graphing scripts.
- The **perc_line_graphs** script produces all the line graphs, which show change in a variable over time, for data with percentage values. The script includes additional information about how to use it. This is one of multiple graphing scripts, as I have separate scripts for other graph and data types as each requires slightly different processing, and this was the most intuitive way for me to organize it.
- The **data_files** folder has the data needed to run this code.
- The **sample graphics** folder includes a small sample of the graphs and figures produced for this project.

# Author
This code was written by me, Eliana Sullivan, in 2022. I am the only person currently working directly with this code.
