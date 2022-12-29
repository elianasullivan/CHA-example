# Community Health Assessment
This code produces graphs for Oregon's Community Health Assessments (CHAs) from publicly available data. CHAs include information about a county's health status and needs which inform improvement plans. The purpose of this code is to standardize and automate graph production and to streamline making changes across multiple graphs. While there are past CHAs, there is no standard CHA format, so everything from identifying the data sources, to designing visualizations, to writing it in R instead of Excel was part of my role on this project.

# What is included
All code is written in R. This is a sample of the code I wrote for this project, but it still displays most of the techniques and styles that I used. 
- The **data_cleaning** code contains all the libraries, data import, cleaning, functions, and pre-set variables for the rest of the code. This should be run first before any of the graphing scripts.
- The **perc_line_graphs** code produces all the line graphs, which show change in a variable over time, for data with percentage values. The script includes additional information about how to use it. This is one of multiple graphing scripts, as I have separate scripts for other graph and data types as each requires slightly different processing, and this was the most intuitive way for me to organize it.
- The **data_files** folder has the data needed to run this code.
- The **sample graphics** folder includes a small sample of the graphs and figures produced for this project.

# Author
This code was written by me, Eliana Sullivan, in 2022. I am the only person currently working directly with this code.
