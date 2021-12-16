# Macroalgae BEF project

Repository for the macroalgae transplant and experiment and field surveys planned for the 2021 field season at Tjarno marine station.

> Data availability

All data and scripts are hosted on RESEARCHBOX and can be found at this link:

+ https://researchbox.org/435&PEER_REVIEW_passcode=ECOTGX

# Reproduce the reported analyses

To reproduce the analyses reported in the manuscript, three preparation steps are required.

Step 1: Download the scripts under the "function" section (which always end with a suffix "function.R") and save it in a folder in the working directory called:

+ functions

Step 2: Download the scripts under the "analysis_scripts" section and save it in the working directory.

Step 3: Download all data under the "analysis_data" section and save it in the working directory in a folder called:

+ functions

Once these preparation steps have been completed, you can run the scripts downloaded from the "analysis_scripts" section and all analyses reported in the manuscript will be reproduced.

Note that these scripts perform analyses on cleaned version of the data. If you would like to reproduce the entire analysis from scratch, see the rest of this ReadMe.


# Reproduce the entire analysis from the raw data

The following description takes you through how we processed all the raw data we collected and outputted dataset ready for analysis into the "analysis_data" folder.

> Experiment data

To clean the initial measurements and final measurements for the experimental data, download the following datasets from the experimental_data section on RESEARCHBOX (link above):

+ tile_experiment_data_plants_pre.csv 
+ tile_experiment_post.csv

Save this file into a folder in the working directory called: 

+ experiment_data

Download the following scripts from the "cleaning_scripts" section of the RESEARCHBOX: 

+ clean_initial_experiment_data.R
+ clean_post_experiment_data.R

Run these scripts which will clean the data and output a cleaned version of the data into an analysis_data folder.

> Sea level data analysis

To process the raw sea-level data, go to the RESEARCHBOX repository (link above). Then, select and download the file from the raw_sea_level_data section:

+ sea_level_data_raw_2015_2021.csv

Save this into a folder called "sea_level_data" in the current working directory. Some limited processing of these data was done before uploading it to the RESEARCHBOX repository. The script where this limited processing was conducted is: 

+ clean_sea_level_data.R. 

In addition, the raw data file can be downloaded from SMHI (https://www.smhi.se/):

+ Havsvattenstånd RH2000, minutvärde, (station: Kungsvik)
+ https://www.smhi.se/data/oceanografi/ladda-ner-oceanografiska-observationer#param=sealevelMinutes,stations=all,stationid=2130

Next, you will need to download two files from the RESEARCHBOX repository from the section called preliminary_supporting_data:

+ transect_data.csv
+ sample_data_biomass_allometry.csv

These files should be saved in a folder in the working directory called: preliminary_supporting_data.

Once these three files have been downloaded and saved into the appropriate folders (see above), open the script from the "cleaning_scripts" section called: 

+ 3_process_sea_level_data.R 

This will clean the raw sea level data and output as a cleaned version into the "analysis_data" folder.

In addition, the script will output a data file that will later be used to calibrate the sea level data taken in the field from an environmental data application (VIVA) with the published sea level data.

> Logger data analysis

To process the raw temperature and light intensity data from the hobo loggers, go to the RESEARCHBOX repository (link above). Then, select and download all 20 files from the raw_logger_data category. Create a folder in your working directory called "tile_logger_data" and save the 20 files into this folder. Note that very limited processing of these data was done before uploading the files to research box using the script: 

+ clean_tile_experiment_logger_data.R

We have included this script for reference but we do not provide the input data to run this script.

Then, download the script from the section called "cleaning_scripts" in the RESEARCHBOX repository called: 

+ 4_process_tile_experiment_logger_data.R

This script cleans and processes the data and outputs a cleaned version of the data into an "analysis_data" folder in the working directory.

> Analysis of the depths at which different species naturally occur (Fig. 1)

To analyse the depth distrbutions of the four species in their natural settings, we used two datasets: dedicated transects and other field collections of the four species where depths were measured. These two datasets can be downloaded from the research box and should be placed in a folder called: preliminary_supporting_data:

+ sample_data_biomass_allometry.csv
+ transect_data.csv

Running the following script found in the "cleaning_scripts" sectoin will then output a cleaned version of these data into the "analysis_data" folder

+ 5_process_species_depth_data.R

> Optional: Calibrate in-situ water-level measurements

Water-level measurements taken in the field using the ViVa application do not perfectly correspond to those published by SMHI. This does not necessarily matter for the Tile Experiment but we want to better correct some of our other studies.

We use the following script to fit models to predict the SMHI water-level using the ViVa water levels:

+ calibrate_viva_published_sea_level.R

Then, there is a script that uses this model to correct the depth using the model predictions:

+ depth_correcter_example.R

This script calls a function that should have been downloaded already if the readme has been properly followed. This function will only run if the model from the calibrate_viva_published_sea_level.R has been run:

+ depth_correcter.R

