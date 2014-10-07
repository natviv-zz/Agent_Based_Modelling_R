# Solar ABM
# Scott A Robinson
# UT Austin, Energy Systems Transformation Group
#
# The solar model loads data on all households in Austin, TX and initializes them as agents in the model.
# Each agent has a unique ID: "PROP_ID."
# Model uses several helper scripts: 
#	solarABM_loadDat.r ;
#	solarABM_functions.r;
#	
#
 
#-------------------------#
#Define Model Parameters
#-------------------------#
params = data.frame(
	iter = 0, 											# Used as a outfile designation. Remember to change and log as parameters are altered.
	pathCSV = "E:/GIS/Solar/Excel/Households_Full.csv", # Source empirical data
	outPath = "E:/Agent Analyst/ABM_Runs/All/", 		# Outcome directory
	neiPath = "C:/Users/Scott/My Documents/solarABM_neighbors.txt",
	runs = 1, 											# Number of runs in a batch
	ticks = 18, 										# Number of ticks in a run. set to quarters 
	steps = 3, 											# Number of steps in a tick. Set to months (3 months in a quarter, 4 quarters in a year)
	siaThresh = 0.6, 									# Threshold in attitude to become an adopter. Arbitrary
	pbcThresh = 8,										# Threshold for PBC for hh to becomes adopter. Based on log avg of PBC vars
	lambda = 3, 										# Poisson parameter controlling average number of random connections
	mu = 0.6,											# Speed of convergence in Relative Agreement
	randCon = 3,										# Number of random connections in small world networks
	doPar = FALSE,
	record = TRUE,
	numTicks = 18, 
	numHH = 0,
	graphIt = TRUE,
	empiricalNei = TRUE
	)
# generate outcome pathnames: Model Results
outFile1 = sprintf("%smyrunlog_100_b%s.csv", params$outPath, params$iter)
outFile2 = sprintf("%stable_b%s.csv", params$outPath, params$iter)
	
#-------------------------#
# load functions
#-------------------------#	
source("E:/Agent Analyst/Code/solarABM_functions.r")	
	
#-------------------------#
# Run the model
#-------------------------#
# batch run
runCount = 1
if (params$doPar == TRUE){ # run parallel? This is untested!
	source("E:/Agent Analyst/code/solarABM_loadDat.r") # initialize agents 
	foreach(b = 1:params$runs) %dopar% {runmain(params$ticks, params$steps) # use "foreach" structure
		runCount = b + 1
		
		}
	} else { # no parallel? then use for loop structure
		for (b in 1:params$runs){
			source("E:/Agent Analyst/code/solarABM_loadDat.r") # initialize agents
			flush.console()
			runMain(params$ticks, params$steps) # run a batch
			
			runCount = b + 1 # update batches
			print(sprintf("Run number : %s", runCount))
			flush.console()
			}
		# Write to file
		write.csv(myrunlog, outFile1) # Batch Individual adoption outcomes
		write.csv(outTable, outFile2)# Batch cumulative outcomes
		print(sprintf("myrunlog stored as: %s", outFile1))
		print(sprintf("outTable stored as: %s", outFile2))
	}