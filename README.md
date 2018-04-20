# Temperature_Allee
Data and files to reproduce the analysis in Lindmark et al 2018

%---- To reproduce the figures in Lindmark et al (2018) based on the simulated data, please see the folder "R" which uses already simulated and saved data, stored in folder "Data"
%---- To see how the model is implemented in matcont, see "MatLab_MatCont_model_files", "msII_Paper_MatCont_implementation.txt"
%---- To open the model in MatCont GUI and re-export or check simulated curves, follow the instructions below.

%-- Navigate to MatLab_MatCont_model_files:
The zip-file contains files for opening the model in MatCont GUI to redo analysis or re-export data used for plotting (see Data and R folders):

The zip-file contains the following:
1) The computed curves (Diagrams)
2) msII_Paper.mat (what is this)
3) msII_Paper.m (what is this)

Steps:
Install MatLab (student version or any version that contains the symbolic package. This is needed to use the heavyside function that is implementeted in the maturation function, see text file with "model implementations") and MatCont

1. To open the model, to view the computed equilibrium curves and to use the matlab code below to export data as .csv do like this: 
2. Navigate to the systems folder in your MATLAB directory (MATLAB -> matcont -> systems) 
3. Unzip the folder with the model name "msII_Paper"
4. Drag the unzipped (1) "msII_Paper" folder, (2) the msII_Paper.mat and (3) the msII_Paper.m files to the systems folder defined above
Open MatCont by typing matcont in the MATLAB command window.
