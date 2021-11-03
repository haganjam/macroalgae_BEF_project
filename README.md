# Macroalgae BEF project
Repository for the macroalgae transplant and experiment and field surveys planned for the 2021 field season at Tjarno marine station.

> Data availability

All data are hosted on RESEARCHBOX and can be downloaded directly from this link:

+ https://researchbox.org/435&PEER_REVIEW_passcode=ECOTGX

> Preparation

Download all scripts in the RESEARCHBOX repository (link above). Scripts grouped under "functions" section (which always end with a suffix "function.R") should be placed in a separate folder named "functions" in the working directory.

> Logger data analysis

To process the raw temperature and light intensity data from the hobo loggers, go to the RESEARCHBOX repository (link above). Then, select and download all 20 files from the raw_logger_data category. Create a folder in your working directory called "tile_logger_data" and save the 20 files into this folder. Note that very limited processing of these data was done before uploading the files to research box using the clean_tile_experiment_logger_data.R script.

Download the script from the RESEARCHBOX repository called: process_tile_experiment_logger_data.R.

> Sea level data analysis

To process the raw sea-level data, go to the RESEARCHBOX repository (link above). Then, select and download the file from the raw_sea_level_data section:

+ sea_level_data_raw_2015_2021.csv

Save this into a folder called "sea_level_data" in the current working directory. Some limited processing of these data was done before uploading it to the RESEARCHBOX repository. The script where this limited processing was conducted is clean_sea_level_data.R. In addition, the raw data file can be downloaded from SMHI (https://www.smhi.se/):

+ Havsvattenstånd RH2000, minutvärde, (station: Kungsvik)
+ https://www.smhi.se/data/oceanografi/ladda-ner-oceanografiska-observationer#param=sealevelMinutes,stations=all,stationid=2130

In addition, you will need to download two files from the RESEARCHBOX repository from the section called preliminary_supporting_data:

+ transect_data.csv
+ sample_data_biomass_allometry.csv

These files should be saved in a folder in the working directory called: preliminary_supporting_data.

Once these three files have been downloaded and saved into the appropriate folders (see above), open the script called: 

+ process_sea_level_data.R 

This will clean the raw sea level data and output a table with summary variables for each treatment depth used in the experiment. The full cleaned data and the summary table are then outputted as .csv files into a folder called analysis_data. If the folder does not exist, the script will create the folder.

In addition, the script will output a data file that will later be used to calibrate the sea level data taken in the field from an environmental data application (VIVA) with the published sea level data.

> Calibrate in-situ water-level measurements

Water-level measurements taken in the field using the ViVa application do not perfectly correspond to those published by SMHI. This does not necessarily matter for the Tile Experiment but we want to better correct some of our other studies.

We use the following script to fit models to predict the SMHI water-level using the ViVa water levels:

+ calibrate_viva_published_sea_level.R

Then, there is a script that uses this model to correct the depth using the model predictions:

+ depth_correcter_example.R

This script calls a function that should have been downloaded already if the readme has been properly followed:

+ depth_correcter.R

> Experiment data

To clean the initial measurements and final measurements for the experimental data, download the following datasets from the experimental_data section on RESEARCHBOX (link above):

+ tile_experiment_data_plants_pre.csv 
+ tile_experiment_post.csv

Save this file into a folder in the working directory called: 

+ experiment_data

Download the following scripts: 

+ clean_initial_experiment_data.R
+ clean_post_experiment_data.R

Run these scripts which will clean the data and output a cleaned version of the data into an analysis_data folder.




