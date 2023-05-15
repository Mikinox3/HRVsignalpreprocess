# HRVsignalpreprocess
Short R script for HRV signal preprocess (data acquisition with Biopac device and ACQKNOWLEDGE software).

This script has been created for a very specific experimental paradigm with precise triggers created during ECG recording.
The Main.R file calls all the other files when required. 
Main.R loads the raw ECG data files and preprocess the signal, it filters and detects the R peaks and RR intervals so the user can then calculate various parameters from the cleaned signal for ECG profile description). 

The package RHRV was used for the script : http://rhrv.r-forge.r-project.org/ .
RHRV provides an artifact detection function using adaptive thresholding for rejecting beats whose RR value differs from previous and following beats. The function also removes values that are not acceptable from a physiological point of view.
The signal interpolation algorithm uses linear or cubic splines interpolation.
For more details about the HRV signal analysis, see :  Heart Rate Variability Analysis with the R package RHRV, C.A. García, A. Otero, X.A. Vila, M.J. Lado, L. Rodríguez-Liñares, J.M. Presedo, and A.J. Médez. Springer International Publishing, 2017. Springer book page
