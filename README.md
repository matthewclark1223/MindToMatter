# R code Repository for 'From Mind to Matter: Patterns of Innovation in the Archaeological Record and the Ecology of Social Learning'

The code provided here allows interested readers to further examine the agent-based simulation presented in this study and modify the parameter settings to ask new questions.

There are three R scripts included in this repository. The first provides some useful functions for plotting the model outputs. The second is simplified code explaining the model itself. The third is the script what we used to produce all figures used in the study which allows for multiple runs of the model at a given parameter setting.   
Note that when the simulation is set for 10 runs at a population size of 1,000 for 5,000 generations, as is presented in the multi-run script, the run time on the authors' laptop computer (Dell Latitude 7420) takes around 5 hours. Increasing population size, the number of runs, or the number of generations increases the run time.  

### Files:

* PlotFunction.r - Some functions for plotting the model output
* SingleRunCode.R - Code for running and plotting just one run of the model
* MultirunLineplots.R - Code for running and plotting multiple runs of the model

Also note that both scripts require users to have the 'tidyverse' suite of packages installed. 

**Matt Clark, 18 April 2023**
