# Temperature_Allee
Data and MatCont-files to reproduce the main analyses in Lindmark et al (2018).

* Update: Early view is now online in Ecology Letters! 
https://onlinelibrary.wiley.com/doi/10.1111/ele.13235#.XHzzJqZzqpk.twitter

Data: contains simulated data used for creating figures
R: scripts for producing figures based on already simulated data
MatLab_Mat_Cont_model_files: contains model files and text files for implementing the model in MatCont


## If you want to:
### reproduce the figures 
... based on the simulated data, please see the folder "R" which uses already simulated and saved data, stored in folder "Data". The packages used are:

tidyverse 1.2.1 --
v ggplot2 2.2.1     v purrr   0.2.4
v tibble  1.4.2     v dplyr   0.7.4
v tidyr   0.8.0     v stringr 1.3.1
v readr   1.1.1     v forcats 0.3.0

RCurl_1.95-4.10
gridExtra_2.3

### see how the model is implemented in matcont
... see "MatLab_MatCont_model_files_revision" or "MatLab_MatCont_model_files_generic" for the original model formulation, see "msII_Paper_MatCont_implementation.txt" and "User_functions.txt" (which are needed to track non-state variables in MatCont, such as maturation or net biomass production. Note, only in the "empirical" model!)

### open the model in MatCont GUI and re-export or check simulated curves
... follow the instructions below.

#### Navigate to folder MatLab_MatCont_model_files:
The zip-file "MatCont_files" contains files for opening the model in MatCont GUI to redo analysis or re-export data used for plotting (see Data and R folders if you want to reproduce the figures based on already saved data):

The zip-file "MatCont_files" contains the following:
1) The computed curves (Diagrams)
2) msII_Paper.mat
3) msII_Paper.m (This is the model generated by MatCont GUI)

Steps:
Install MatLab and MatCont (https://sourceforge.net/projects/matcont/)
You will need the student version or any version that contains the symbolic package. This is needed to use the heavyside function that is implementeted in the maturation function; see text file "msII_Paper_MatCont_implementation".

1. To open the model, to view the computed equilibrium curves and to use the matlab code (file: msII_Paper_export_data_MATLAB) to export data as .csv do like this: 
2. Navigate to the "Systems" folder in your MATLAB directory (MATLAB -> "matcont" -> Systems) 
3. Unzip the file "MatCont_files"
4. Drag the unzipped contents (1) "msII_Paper" folder, (2) the msII_Paper.mat and (3) the msII_Paper.m files to the "Systems" folder in your MatLab folder, see above
5. Open MatCont by typing matcont in the MATLAB command window.
6. Now you can open all the diagrams and view them in matlab or use the MatLab code in the file "MatLab_MatCont_model_files/msII_Paper_exporting_curves" to export the diagrams as .csv files locally (*Note, change file path!)
7. *Note, the names are very abbreviated! Rather than writing the names out here, I suggest you check first identify which analysis (figure) you want to look at, and then check the data-folder here to find which curves in Matcont correspond to which figure (in cases where this is not obvious!)
