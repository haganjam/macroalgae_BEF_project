
# Macroalgae BEF project

Repository for the macroalgae transplant experiment and field surveys conducted during the summer field season in 2021 at Tjarno marine station.

## Data availability

All raw data along with codebooks are hosted on RESEARCHBOX and can be found at this link:

+ https://researchbox.org/435&PEER_REVIEW_passcode=ECOTGX

## Reproduce the reported analyses

### Download the code repository

To reproduce the analyses reported in the manuscript, download this repository to your computer. This can be done in two ways:

#### 1. with git

in the Terminal:

```cd path/to/local/folder``` 

(on your computer - the folder were you want the repository to live) command on Windows might differ. 


```https://github.com/haganjam/macroalgae_BEF_project.git```

This should download the directory. 

#### 2. without git

If you don't have git installed, you can download the repository as zip file and save it locally. 

--> Clone or download (green button top right)
--> Download Zip

then save and extract the zip where you want the directory to be saved on your computer. To run the code correctly, it is important to create a R-Project.
In R-Studio go to File > New Project... > Existing Directory > Choose the extracted directory.

### Download the raw data

Once you have downloaded the repository to your computer, the next step is to download the raw data from the ResearchBox repository where it is stored. To do this, go to the following link:

+ https://researchbox.org/435&PEER_REVIEW_passcode=ECOTGX

Then, click the red button near the bottom of the page called: 

```Download Entire Box```

This will download a .zip file. Save this .zip file to repository directory. Make sure that it remains *unzipped*.

### Run the code to reproduce the analysis

Once the repository and raw data have been downloaded as detailed above, start by running all the scripts in the folder called: "02_cleaning_scripts". Scripts in the "01_functions" folder are called throughout the analysis but do not need to be run individually. Scripts from the "02_cleaning_scripts" folder should be run in the following order:

+ 01_unzip_raw_data_researchbox.R
+ 02_clean_initial_experiment_data.R
+ 03_clean_post_experiment_data.R
+ 04_process_sea_level_data.R
+ 05_process_tile_experiment_logger_data.R
+ 06_process_species_depth_data.R

The two additional, un-numbered scripts do not need to be run. These scripts were used to perform limited processing of the sea-level data and the logger data before uploading the data to the ResearchBox. The original sea-level data can be downloaded from SMHI (https://www.smhi.se/):

+ Havsvattenstånd RH2000, minutvärde, (station: Kungsvik)
+ https://www.smhi.se/data/oceanografi/ladda-ner-oceanografiska-observationer#param=sealevelMinutes,stations=all,stationid=2130

Running these scripts will output clean, processed data into a folder called analysis data that is created in the directory folder.

Once all the cleaned data have been outputted into the analysis data folder, you can run the scripts in the folder called: "03_analysis_scripts". These scripts do not need to be run in any particular order and will generate all figures and tables in the manuscript. These figures and tables are outputted into a folder called "figures" that is automatically created.

#### A note on package management

In this repository, we use the groundhog package in R for package management: 

+ (https://cran.r-project.org/web/packages/groundhog/groundhog.pdf)

Groundhog works by versions of packages available at a certain date. Thus, if someone tries to run this code in the future, groundhog will download the versions of packages used in the project that were present at the time the code was written.

The R version the code was written in is 4.2.1, groundhog date is 2022-07-17.

There are various reasons why groundhog does not work sometimes. For example, sometimes certain versions of packages are not compatible with different versions of R and this can make it frustrating for someone trying to run the code. If you run into trouble with groundhog, you can simply install the packages normally and it is almost certain that the code will still run.

By default we turned groundhog off in each script. If you want to use groundhog you can simply turn it on by changing it to TRUE:

gh <- TRUE

 
