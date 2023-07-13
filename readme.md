<h3>SILC application and download instructions:</h3>
The dataset of the Italian version of the European Union Statistics on Income and Living Conditions (IT-SILC) must be requested to the Istituto Nazionale di Statistica (Istat) (see: https://www.istat.it/it/archivio/216947 and https://www.istat.it/it/archivio/4204)


<h3>File renaming and re-saving as csv instructions:</h3>
Once in hands of the SILC data provided by Istat, in order to reproduce the code contained in this repository, it is necessary to open the datasets for each year and save a copy in .csv with names consistent with time and dataset type. For example, for the year 2007, save the datasets as: "l2007p.csv", "l2007r.csv", "l2007h.csv", "l2007d.csv"

Only the following dataset variables are used 

"p" --> "PB010", "PB030", "PB150", "PE040", "PH010", "PH020", "PH030"

"r" --> "RB010", "RB030", "RB040", "RB060", "RB062", "RB063", "RB150", "RB110", "RX010", "RX020"

"d" --> "DB010", "DB030", "DB040", "DB075", "DB060"





<h3>R scripts description:</h3>
The folder “code” contains the R files to replicate the analysis and, specifically:


“00_functions.R” "A" contains several functions used in subsequent scripts

"01_data_prep_SILC.R" contains steps for preparing SILC raw data to be used for analysis

"01_data_prep_life_tables.R" contains steps to prepare SILC mortality tables to be used for the SILC-estimated probability correction part of the analysis

"02_transitions_point_estimates.R" contains procedures for estimating transition probabilities between disability and death states, the correction method and estimating expectancies

"03_confidence_intervals.R" shows the calculation of confidence intervals for expectancies

"04_decomp_periods.R" contains the steps to break down the DFLE evolution over time

Finally, "05_figures.R" contains the scripts to replicate the figures in the paper







