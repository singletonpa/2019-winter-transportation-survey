# 2019-winter-transportation-survey

## MPC-559 Investigating travel behavior during poor air quality events in Northern Utah

This repository contains data and scripts associated with the ["Identifying Effective Travel Behavior Change Strategies for Poor Air Quality Events in Northern Utah" research project MPC-559](https://www.mountain-plains.org/research/details.php?id=456), funded by the [Mountain-Plains Consortium](https://www.mountain-plains.org/), and carried out by [Patrick Singleton](https://engineering.usu.edu/cee/people/faculty/singleton-patrick) and the [Singleton Transportation Lab](https://engineering.usu.edu/cee/research/labs/patrick-singleton/) at Utah State University. 

* **Objectives**
   * Understand how measured (or perceived) poor air quality affect individuals’ daily travel amounts. 
   * Identify what factors (personal characteristics, travel behaviors, and measured air quality) affect perceptions of air quality. 
   * Understand patterns of attribution of responsibility of air pollution, the relationship with stated travel behavior changes, and the impact of awareness of consequences, risk perception, self-efficacy, and socio-demographics.
   * Determine whether and how measured (or perceived) area-wide air pollution affects individuals’ daily activity and travel behaviors, as well as how those associations differ by neighborhood type.
* **Methods**
   * Survey data collection, using a longitudinal online questionnaire including information about personal and household characteristics, three two-day travel diaries (about activities, places, and trips), and attitudes and perceptions about transportation and air pollution. 
   * Statistical modeling of associations among activity patterns, travel behaviors, air pollution levels, air quality perceptions, attribution of responsibility over air pollution, and personal/household characteristics. 

## Description of files and folders

* Many of these scripts were written in R. To use, [download R](https://cloud.r-project.org/) and then [download RStudio](https://posit.co/download/rstudio-desktop/#download).
* **2019-winter-transportation-survey.Rproj**: This [RStudio project](https://support.posit.co/hc/en-us/articles/200526207-Using-RStudio-Projects) file allows R users to use relative file paths from this root folder, if R scripts are opened after opening this R project file. 

### Data
These folders contain data collected and assembled as part of this research project.
* **Survey 2019 Winter**: Anoymized survey data collected, along with the survey questionnaires, a data description file, and a data dictionary file. 
   * Note: Many preliminary datasets are not included. They have been removed to protect participant confidentiality. 

### Analysis
These folders contain scripts and outputs associated with the prepartaion and analyses of the research project data.
* **Data cleaning**: Scripts associated with the processing and cleaning of the survey data, along with a script description file.
   * Note: Some code within each script file is not included. Code has been removed to protect participant confidentiality.
* **Descriptive statistics**: Script and tables showing descriptive statistics for the various datasets. 
