# Calculating Extinction Probabilities from IUCN data using Markov Models

##### Authors: 

- Rachel Bates (r.bates18@imperial.ac.uk)
- Elizabeth Taylor (e.j.taylor@soton.ac.uk)

-------

## Overview

All code files including the overall workflow script are stored in the `Code` directory. 

All code was written in R version 3.6.2 

Packages used:

- rredlist 
- jsonlite
- dplyr

-----

## Scripts

**Workflow.R** - Imports all other scripts and uses the functions to run the entire workflow of the Markov modelling from collecting the IUCN data through to analysis.

### Modules:

1. Data_Collection.R - Collects all required data from the IUCN to be able to run the model. This mainly uses rredlist to access the api.
   - Species_Info_Collect - 
   - Species_History_Collect - 
   - Species_Meta_Collect - 
2. Data_Processing.R
3. 





