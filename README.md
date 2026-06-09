# Summary

This repository is for storing and distributing supplementary material for the article "How safe were COVID-19 vaccines? - Summary of published research" published at https://eutheorem.com/2025/covid-19-vaccines/. The repository contains literature metadata, R code, csv files that contain publication data and benefit-risk balance in the publications, and figures. 

# Folders and files

## figures/

The folder contains figures and supplementary figures.

- **fig_BR_balance_CBF.png**: The color-blind friendly version of the Figure 1, used as Figure S3. 

- **fig_BR_balance.png**: Figure 1 of the article. 

- **Supplementary_figures**: Supplementary figures, including the PRISMA flowchart and checklist. 

## nbib_files/ 
Publication metadata in nbib file format. 

## costom_revtools.R
R code of customized functions of revtools. revtools functions do not work on newer versions of R, and on long publication metadata. Changes are made to overcome these limitations.

## COVID_vaccination.R
R code for the analysis and drawing the figures. 

## Table_S1_publications.csv
A csv file of summary of publications used in this analysis. The file lists 37 publications that Newbern et al., 2025 analyzed, plus six additional publications that we identified. Note that the file was created to help us picture the overview of the publications and determine how to analyze the publications. Because of this, most columns are irrelevant to the analysis performed on R.  

*Important columns* 

**reference_number**: Matches reference_number in the BR_balance.csv. 

**include**: The value is FALSE if the publication is not available anymore, is redundant, or does not provide benefit-risk balance. Publications with FALSE value were excluded from our analysis.  

## Table_S2_BR_balance.csv 
A csv file of summary of benefit-risk balance of COVID-19 vaccines in the publications. Each row contains an aggregated assessment on a sex / age groups. 

*Important columns*

**reference_number**: The unique identification number of the publication. The number matches the reference number in Newbern et al., 2025 when it is betwee 11 and 47. Publications with the reference_number above 47 are newly added in this analysis. 

**gender**: The value in the column is one of "f" (female), "m" (male), or "b" (both, meaning the assessment is not stratified by sex). 

**regimen_of_observation**: Showing whether the assessment is on primary vaccine dose, booster, or unstratified by regimen. 

**min_age, max_age**: Showing the age group of the aggregated assessments. 

**BR_high, BR_low**: Showing the range of benefit-risk balance in aggregated assessments. 

**booster**: Details of the booster dose (e.g. coverage) if applicable. 
