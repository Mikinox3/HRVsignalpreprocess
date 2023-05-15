# HRVsignalpreprocess
Short R script for HRV signal preprocess (data acquisition with Biopac device and ACQKNOWLEDGE software).

This script has been created for a very specific experimental paradigm with specific triggers created during ECG recording.
The Main.R file calls all the other files when required. 
It loads the raw ECG data files and preprocess the signal (filters and detects the R peaks and RR intervals to evaluate later various parameters for ECG profile description). The package RHRV was used for the script : http://rhrv.r-forge.r-project.org/ .
