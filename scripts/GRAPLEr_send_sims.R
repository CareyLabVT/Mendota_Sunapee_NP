# Load GRAPLEr packages
#devtools::install_github("GRAPLE/GRAPLEr")
pacman::p_load(glmtools, httr, RCurl, jsonlite, GRAPLEr)

# Mendota ####
sim_folder  <-  ('/Users/KJF/Desktop/R/Mendota_Sunapee_NP/Mendota/GLM')
setwd(sim_folder)

MyExpRootDir <- paste(sim_folder,sep='/','MyExpRoot') # Met files and nml are here		
MyResultsDir <- paste(sim_folder,sep='/','MyResults') # Outputs will go here		
dir.create(MyResultsDir)  # Create the MyResults directory in your computer		

list.files(MyExpRootDir) 

graplerURL <- "https://graple.acis.ufl.edu" 	

MyExp <- new("Graple", GWSURL=graplerURL, ExpRootDir=MyExpRootDir, ResultsDir=MyResultsDir,		
             ExpName="Mendota_NP", TempDir = tempdir())	# Set up your GRAPLEr experiment	

MyExp <- GrapleCheckService(MyExp)		
print(MyExp@StatusMsg)  # I am alive, and at your service."		

MyExp <- GrapleRunSweepExperiment(MyExp) #	Submit experiment		
print(MyExp@StatusMsg) # Check submission status		
	
MyExp <- GrapleCheckExperimentCompletion(MyExp)	# Check the status of your experiment	 
print(MyExp@StatusMsg)		

# Pull results
MyExp <- GrapleGetExperimentResults(MyExp);		
print(MyExp@StatusMsg)		

#  maximum offset scenarios:		
plot_temp(file=nc_file_1, fig_path=FALSE)		
plot_temp(file=nc_file_8, fig_path=FALSE)		

sim_summary_path <- paste(MyResultsDir,sep='/','Mendota_NP','sim_summary.csv')		
Mendota_sim_summary <- read.csv(file=sim_summary_path,head=FALSE,sep=",")		

for (n in 1:8) {		
  sim_folder_n <- paste(MyResultsDir,sep='/','Mendota_NP','Sims',Mendota_sim_summary$V1[n],'Results')		
  nc_file_n <- file.path(sim_folder_n, 'output.nc')		
  tempoffset <- Mendota_sim_summary$V5[n]		
  simlabel <- paste(Mendota_sim_summary$V1[n], "temperature offset:", tempoffset)		
  print(simlabel)		
  plot_temp(file=nc_file_n, fig_path=FALSE, col_lim = c(0,40))		
}		



# Sunapee ####
sim_folder  <-  ('/Users/kfarrell/Documents/R/Mendota_Sunapee_NP/Sunapee/GLM')
setwd(sim_folder)

MyExpRootDir <- paste(sim_folder,sep='/','MyExpRoot') # Met files and nml are here		
MyResultsDir <- paste(sim_folder,sep='/','MyResults') # Outputs will go here		
dir.create(MyResultsDir)  # Create the MyResults directory in your computer		

list.files(MyExpRootDir) 

graplerURL <- "https://graple.acis.ufl.edu" 	

MyExp <- new("Graple", GWSURL=graplerURL, ExpRootDir=MyExpRootDir, ResultsDir=MyResultsDir,		
             ExpName="Sunapee_NP", TempDir = tempdir())	# Set up your GRAPLEr experiment

MyExp <- GrapleCheckService(MyExp)		
print(MyExp@StatusMsg)  # I am alive, and at your service."		

MyExp <- GrapleRunSweepExperiment(MyExp) #	Submit experiment		
print(MyExp@StatusMsg) # Check submission status		

MyExp <- GrapleCheckExperimentCompletion(MyExp)	# Check the status of your experiment	 
print(MyExp@StatusMsg)		

# Pull results
MyExp <- GrapleGetExperimentResults(MyExp);		
print(MyExp@StatusMsg)		

#  maximum offset scenarios:		
plot_temp(file=nc_file_1, fig_path=FALSE)		
plot_temp(file=nc_file_8, fig_path=FALSE)		

sim_summary_path <- paste(MyResultsDir,sep='/','Sunapee_NP','sim_summary.csv')		
Sunapee_sim_summary <- read.csv(file=sim_summary_path,head=FALSE,sep=",")		

for (n in 1:8) {		
  sim_folder_n <- paste(MyResultsDir,sep='/','Sunapee_NP','Sims',Sunapee_sim_summary$V1[n],'Results')		
  nc_file_n <- file.path(sim_folder_n, 'output.nc')		
  tempoffset <- Sunapee_sim_summary$V5[n]		
  simlabel <- paste(Sunapee_sim_summary$V1[n], "temperature offset:", tempoffset)		
  print(simlabel)		
  plot_temp(file=nc_file_n, fig_path=FALSE, col_lim = c(0,40))		
}		
