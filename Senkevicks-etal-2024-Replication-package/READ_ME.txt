This folder contains a comprehensive set of scripts distributed across three subfolders, each critical for the data processing workflow required for our analysis. It is imperative that the scripts are executed sequentially as follows:

1. Codes in SAS:
This subfolder contains SAS scripts designed to access the corporate database of INEP. Due to the sensitive nature of this data, these scripts can only be executed within the INEP’s Protected Data Access Service framework. It is important to note that these scripts were created prior to the enactment of the General Data Protection Law (LGPD), meaning the current data structure may have been modified to comply with new legal requirements. Consequently, users may encounter discrepancies with the present database configuration.

2. Codes in Stata:
In this subfolder, the datasets generated from the SAS scripts are imported for further processing. These Stata scripts perform data cleaning, manipulation, and merging, ultimately producing a refined dataset. This dataset contains only the essential cases and variables necessary to replicate the findings presented in our article.

3. Codes in R:
The final subfolder houses the R scripts, which are crucial for fully replicating the results of our research, including all graphs and tables as seen in the published article. These scripts take the streamlined dataset from the Stata process and perform the analytical procedures to reproduce the study’s findings.

Please ensure that the scripts are run in the specified order to maintain the integrity of the data analysis process and to successfully replicate the results of the study.