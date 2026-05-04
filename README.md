[README.txt](https://github.com/user-attachments/files/27327766/README.txt)[UploadinReplication Package:
Economic Effects of State Level Abortion Restrictions Following Dobbs v. Jackson
Author: Julie Wilson 
Course: ECON-665-01
Date: 5/3/2026

**AI Disclosure: Claude AI was used to build an outline for this README file. All work and content of the page are from the author. 

Project Overview
The purpose of this paper is to examine whether state level abortion restrictions following Dobbs v. Jackson made an impact on safety net programs, like SNAP and TANF. Upon further investigation, one will notices that more restrictive states do not give out as much in assistance funds compared to permissive states. This leads one to believe there are other variables that need to be accounted for before answering the initial research question. 

Software Requirements
* Software: R Studio
* Version: 26.01.0+392
* Packages/Libraries used: pacman, tidyverse, maps, dplyr, did, bacondecomp, fixest, modelsummary, infer, ggplot2, car, plm, readr, broom, kableExtra, corrplot, stargazer

Folder Structure
replication_package/
??? data/
?   ??? [all_data.csv]
??? analysis/
?   ??? [final_r_seminar.qmd]
??? README.md
File Descriptions
* [all_data.csv] � This data set uses all 50 states by month and year between April 2022 and December 2024. 
* [Seminar_Preliminary.qmd] � Using initial data to check how my hypothesis will hold up before writing abstract.
* [Map Attempt.qmd] � Attempting to make US saturation map by abortion rate.
* [final_r_seminar.qmd] � Using codes from previous R scripts to build off of initial hypothesis. New policy groups created, new maps of US created to show which states were in each group, Descriptive Statistics by policy group and pre- and post-Dobbs decision, tested for multicollinearity, parallel trends credibility check, split each variable by policy group on line graph with a pre-Dobbs baseline adjusting for inflation, ran event study plots, and ran a full TWFE model for each dependent variable. 


How to Replicate
1. Open [final_r_seminar.qmd] in R
2. Set the working directory to the replication_package folder
3. Run [final_r_seminar.qmd] � this will produce all tables, figures, and results explained in the description above. 

Data Source
* Abortion rate and Telehealth share comes from a raw number from The Society of Family Planning, converted to a rate per 1000 women age 15-44 by state population. 
* TANF Caseloads, TANF Applications, and SNAP participation stats were also converted the same way. 
o Data comes from the Office of Family Assistance and the US Department of Agriculture. 
* Gestational limit in weeks by each state comes from Guttmacher Institute. 
* Other variables include RPP, CPI, Unemployment Rate, Medicaid Abortion Funding Restrictions, Medicaid Expansion Status, Minimum Wage, Single Mother Median Monthly Income, and Infant Childcare cost burden. 
o All found from free online resources from public sites.
g README.txt…]()
