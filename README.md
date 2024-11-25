# Tropical Cyclone Tracks (with translational speed)

## Overview:

I calculated the translational speed for synthetic tropical cyclone (TC) tracks for the Philippines for both historical and future-climate conditions.

Two options to download the TC track data with speed:
1 -- Download the full 

Units: Speed in meters per second



## Methodology

As input, I used the tracks generated using the Synthetic Tropical cyclOne geneRation Model (STORM) algorithm (see Bloemendaal et al, Generation of a Global Synthetic Tropical cyclone Hazard Dataset using STORM, in review). In the STORM database, Philippines is in the WP basin. Each track data from STORM has information on the time step and location coordinates.

The translational speed is calculated as the distance divided by the time interval between consecutive time steps. Calculating the translational speed needs to consider the curvature of the Earth. Hence, I use the haversine formula in computing the great-circle distance between consecutive latitude-longitdue pairs. 

The translational speed is calculated for each time step except the first one (since it has no previous data point). Hence, speed is NA at time step = 0.

For each TC track from STORM, I added a column that corresponds to the translational speed of the TC's centroid in meters per second. 


## Data source:

### Future TC tracks
https://data.4tu.nl/articles/dataset/STORM_Climate_Change_synthetic_tropical_cyclone_tracks/14237678

10,000 years of synthetic tropical cyclone tracks, generated using the Synthetic Tropical cyclOne geneRation Model (STORM) algorithm (see Bloemendaal et al (2020)). The dataset is generated by extracting the climate change signal from each of the four general circulation models listed below, and adding this signal to the historical data from IBTrACS. This new dataset is then used as input for STORM, and resembles future-climate (2015-2050; RCP8.5/SSP5) conditions. The data can be used to calculate tropical cyclone risk in all (coastal) regions prone to tropical cyclones.

Climate change information from the following models is used in this study (each model has its own 10.000 years of STORM data):
1) CMCC-CM2-VHR4
2) CNRM-CM6-1-HR
3) EC-Earth3P-HR
4) HAdGEM3-GC31-HM

### Historical TC tracks:
https://data.4tu.nl/articles/dataset/STORM_IBTrACS_present_climate_synthetic_tropical_cyclone_tracks/12706085

Datasets consisting of 10,000 years of synthetic tropical cyclone tracks, generated using the Synthetic Tropical cyclOne geneRation Model (STORM) algorithm (see Bloemendaal et al, Generation of a Global Synthetic Tropical cyclone Hazard Dataset using STORM, in review). The dataset is generated using historical data from IBTrACS and resembles present-climate conditions. The data can be used to calculate tropical cyclone risk in all (coastal) regions prone to tropical cyclones.


## Files:

This repository is organised as follows.

```
TC_Tracks_Speed/
├── README.md                    # overview
│ 
├── CalculateTCSpeed.R           # R Script for calculating translational speed for 1 TC track
├── CalculateTCSpeed_batch.R     # R Script for batch calculation for multiple TC tracks
│ 
├── data/                        # input data files from STORM
│   ├── wp_future/               # raw data: future TC tracks for the WP basin (where PH is located)
│   ├── wp_present/              # raw data: historical TC tracks for the WP basin
│   └── README_future.txt        # Readme file from STORM website about future TC track data
│   └── README_historical.txt    # Readme file from STORM website about historical TC track data
│
├── output/                  # output files 
│   ├── wp_future/           # output: future TC tracks with translational speed 
│   ├── wp_present/          # output: historical TC tracks with translational speed
│ 
└── README.md                # overview
```

*Written by Maricar L Rabonza (Nov 2024)*

