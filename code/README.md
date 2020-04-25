# Quick explanation of files

To be filled

* set-par-vals.R simple assigns values for all parameters and initial conditions. Results are saved to parvals.RDS 
* set-priors.R uses point estimates from set-par-vals and for those parameters to be estimated, generates prior distributions. Results are saved to prior-dens-object.RDS
* load-clean-XX-data.R scripts load data from various sources and process data so they are ready for use with pomp. Results are saved to clean-XX-data.RDS in the data folder.
* make-pomp-model.R creates the full pomp model for fitting and simulation. Needs to load and attach the cleaned data. Result is saved to pomp-model.RDS
* 

