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

To process the raw sea-level data, go to the RESEARCHBOX repository (link above). Then, select and download the file from the raw_sea_level_data section. Save this into a folder called "sea_level_data" in the current working directory. Some limited processing of these data was done before uploading it to the RESEARCHBOX repository. The script where this limited processing was conducted is clean_sea_level_data.R. In addition, the raw data file can be downloaded from SMHI (https://www.smhi.se/):

+ Havsvattenstånd RH2000, minutvärde, (station: Kungsvik)
+ https://www.smhi.se/data/oceanografi/ladda-ner-oceanografiska-observationer#param=sealevelMinutes,stations=all,stationid=2130

Once the file from the raw_sea_level_data section has been downloaded into a folder in the working directory called sea_level_data, open the script called: process_sea_level_data.R. This will clean the raw sea level data and output a table with summary variables for each treatment depth used in the experiment. The full cleaned data and the summary table are then outputted as .csv files into a folder called analysis_data. If the folder does not exist, the script will create the folder.

> Experiment data

To clean the initial measurements for the experimental data, download the tile_experiment_data_plants_pre.csv under the experimental_data section on RESEARCHBOX (link above). Save this file into a file in the working directory called experiment_data. Download the script called clean_initial_experiment_data.R. Run this script which will clean the data and output a cleaned version of the data into an analysis_data folder.




