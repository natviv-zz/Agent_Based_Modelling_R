# solarABm_loadDat.R
# Load data from all households in study area for use in the solar PV ABM
#
#
#-----------------------------#
# Read and Clean
#-----------------------------#
# Read in data from file path stored in params
# tab5rows = read.csv(as.character(params$pathCSV), nrows = 10)
#classes = sapply(tab5rows, class)
classes = c("numeric","integer","numeric","numeric","factor","numeric","numeric",
 "numeric","factor","logical","numeric","numeric","numeric","numeric",
"factor","numeric","factor","numeric","numeric","numeric","numeric",
"factor","factor","integer")
print("Loading Households...")

households = read.csv(as.character(params$pathCSV), colClasses = classes, nrows = 50) # Use nrows = n , where n = number of test agents.


h1 = subset(households, MValue > 5000)
h2 = subset(households, MValue <= 5000)
h2$MValue = exp(log(h2$sqft + 1) * .469736 + 7.913305) # Here regression is used replace missing MValues (R2 = 20%)
households = rbind(h1, h2)

# Clean date column
households[,"Date_Rec_1"] = as.Date(households$Date_Rec_1, "%m/%d/%Y")
#Update number of households
params$numHH = length(households[,1])

#-----------------------------#
# Load Libraries
#-----------------------------#
library(sqldf)
library(RSQLite)
library(igraph)

#-----------------------------#
# Check Parallelization
#-----------------------------#
# Check to see if ABM should be run in parallel
getPar(params$doPar)

#-----------------------------#
# Create new variables
#-----------------------------#
# Create new needed variables 
# from Theory of Planned Behavior 
# and Relative Agreement
households$hhIndex = 1:length(households[ ,1])
households$open_sqft = households$sqft - households$TCsqft 	# Square feet available for solar
households$SIA = apply(households, 1, getSIA)				# Calculated in function
households$U = rbeta(length(households[ ,1]), 3, 3) 		# PLACEHOLDER
households$PP = rlnorm(length(households[ ,1]), 1.89, 0.66) # Random distribution from empirical data
#households$PBC = apply(households, 1, getPBC) 				# Use function to calculate PBC
households$PBC = getPBC(households$MValue,
	households$sqft, households$open_sqft)	

#-----------------------------#
# Create Model Output Logs
#-----------------------------#
if(params$record == TRUE & runCount == 1){
	# myrunlog: individual outcomes
	myrunlog = data.frame(Par_ID = households$PROP_ID, 
		N_Adoptions = rep(0, length(households$hhIndex)), 
		hhIndex = households$hhIndex)
	
	# outcome table (cumulative)
	outTable = data.frame(Run =  outHelper(params$runs, params$ticks),
		Tick = rep(seq(0,params$numTicks), params$runs),
		Quarter = rep(c("2007-4", "2008-1","2008-2","2008-3","2008-4","2009-1","2009-2",
			"2009-3","2009-4","2010-1","2010-2","2010-3","2010-4",
			"2011-1","2011-2","2011-3","2011-4","2012-1","2012-2"), params$runs),
		New_Adopers = rep(0, (params$numTicks + 1) * params$runs),
		Sum_Adopters = rep(sum(households$Adopter), (params$numTicks + 1) * params$runs),
		CAGR_annual = rep(0, (params$numTicks + 1) * params$runs),
		Pct_Market = rep(0, (params$numTicks + 1) * params$runs),
		Pct_NonAdopters_Candidate = rep(0, (params$numTicks + 1) * params$runs),
		Pct_NonAdopters_Over_Payback_Thresh	= rep(0, (params$numTicks + 1) * params$runs),
		Average_num_adopters_in_adopter_net	= rep(0, (params$numTicks + 1) * params$runs),
		Average_num_adopters_in_nonadopter_net = rep(0, (params$numTicks + 1) * params$runs),
		Sum_change_in_x	= rep(0, (params$numTicks + 1) * params$runs),
		Sum_change_in_U	= rep(0, (params$numTicks + 1) * params$runs),
		Convergence = rep(0, (params$numTicks + 1) * params$runs),
		Tick_Median_HomeValue = rep(0, (params$numTicks + 1) * params$run),
		Cumulative_Median_HomeValue = rep(0, (params$numTicks + 1) * params$runs))
	}

#-----------------------------#
# Initialize Adopter values
#-----------------------------#
# THIS IS REALLY SLOW! PARALLELIZE
print ("initializing adopter SIA values") 
for (hh in households$hhIndex) {
	if (households$Adopter[hh] == 1){
		adInit = adopterInit(hh)
		households$SIA[hh] = adInit[1]
		households$U[hh] = adInit[2]
		}
	}



#-----------------------------#
# Check Neighbors
#-----------------------------#
# Check to see if ABM should use real neighbors.

# This is the SLOW way
#if (params$empiricalNei == TRUE){
#	neiDF = read.table(as.character(params$neiPath), header = F, sep = ":", colClasses = "character")
#	}

# This writes the database to a temporary file to keep it out of memory. Much Faster	
if (params$empiricalNei == TRUE){
	cat("Loading Empirical Neighbors...")
	con <- dbConnect(SQLite(),dbname = "neiDB")		# Opens a SQL connection to (new) database
	cat("Writing database to disk")
	if(dbExistsTable(con, "neiDB") == FALSE){		# Check to see if database has already been written
		dbWriteTable(con, name ="neiDB", 			# Database to disk, separate at ":"'s
			as.character(params$neiPath), overwrite = TRUE , sep = ":") #, eol = "\r\n"
		}
	Nstatement = sprintf("SELECT * FROM %s", as.character(params$neiPath))
	cat("Converting DB to data frame...")
	neiDF <- fn$dbGetQuery(con, "SELECT * from neiDB")
	cat("Neighbors Dataframe created as: neiDF")
	}
	
#-----------------------------#
# Connect agents in networks
#-----------------------------#	
households$connections = getConnections(households)
	
#
# Initialization complete
#
print ("Initialization Complete")
