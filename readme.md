<h2><b> <a href="https://www.demogr.mpg.de/en/publications_databases_6118/publications_1904/mpidr_working_papers/multistate_analysis_and_decomposition_of_disability_free_life_expectancy_trends_in_italy_2004_2019_7857/">Multistate analysis and decomposition of disability-free life expectancy trends at mid-to-older ages in Italy, 2004–19</a> </b></h2>


<h3>Correspondence</h3>
<p><a href="mailto:margherita.moretti@helsinki.fi">margherita.moretti@helsinki.fi</a></p>


<h3>Citation and link to the paper</h3>

Moretti M., Riffe T., Lorenti A. (2025) Multistate analysis and decomposition of disability-free life expectancy trends in Italy 2004-2019 in mid to older ages, Population Studies, 10.1080/00324728.2025.2475435



<h4>This repository hosts the code to replicate the analysis of the paper.</h4>

<h3>SILC application and download instructions:</h3>
The dataset of the Italian version of the European Union Statistics on Income and Living Conditions (IT-SILC) must be requested to the Istituto Nazionale di Statistica (Istat) (see: https://www.istat.it/it/archivio/216947 and https://www.istat.it/it/archivio/4204). To request the data, go on https://contact.istat.it/index.php?Lingua=Inglese. Register to become a user, then fill out a form: https://www.istat.it/en/analysis-and-products/microdata-files#file_ricerca , scroll down to 'standard files', choose the doc or pdf versions of the form. After filling it out, submit it in your registered profile. You'll end up getting access to a zip file.


<h3>File renaming and re-saving as csv instructions:</h3>
Once in hands of the SILC data provided by Istat, in order to reproduce the code contained in this repository, it is necessary to open the datasets for each year and save a copy in .csv with names consistent with time and dataset type. For example, for the year 2007, save the datasets as: "l2007p.csv", "l2007r.csv", "l2007h.csv", "l2007d.csv"

Only the following dataset variables are used 

"p" --> "PB010", "PB030", "PB150", "PE040", "PH010", "PH020", "PH030"

"r" --> "RB010", "RB030", "RB040", "RB060", "RB062", "RB063", "RB150", "RB110", "RX010", "RX020"

"d" --> "DB010", "DB030", "DB040", "DB075", "DB060"





<h3>R scripts description:</h3>
The folder “code” contains the R files to replicate the analysis and, specifically:


“00_functions.R” contains several functions used in subsequent scripts

"01_data_prep_SILC.R" contains steps for preparing SILC raw data to be used for analysis

"01_data_prep_life_tables.R" contains steps to prepare SILC mortality tables to be used for the SILC-estimated probability correction part of the analysis

"02_transitions_point_estimates.R" contains procedures for estimating transition probabilities between disability and death states, the correction method and estimating expectancies

"03_confidence_intervals.R" shows the calculation of confidence intervals for expectancies

"04_decomp_periods.R" contains the steps to break down the DFLE evolution over time; "04_decomp_periods_iter.R" contains the code to computer the confidence intervals of the decomposition results

Finally, "05_figures.R" and "06_tables.R" contains the scripts to replicate the figures and the tables of the paper







