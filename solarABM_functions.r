#
# solarABM_functions.r
#
#
#Functions for use in the agent based model solar_ABM_R.r 
#are held here and loaded into the model.
#List of functions declared:
#	step() will move the model through one time step. Called after t0.
#	getConnectons()  will create a list of connections for each household.
#	connect() helper function to getConnections(). Connects one household
#	relativeAgreement() exchanges opinion dynamics between two agents

#------------------------#
# Run Main
#------------------------#
runMain = function(ticks, steps){
# Highest level function. Controls the number of steps
# USES GLOBAL ASSIGNMENT (<<-)
	for(i in 1:ticks){
		print(sprintf ("ENTERING QUARTER : %s", i))
		flush.console()									# R waits to print by default	
		for (stp in steps){
			for(hh in households$hhIndex){
				stepVec = step(hh)
				# print(sprintf("using this stepVec for household %s : %s", hh, unlist(stepVec)))
				# Update
				households$Adopter[hh] <<- stepVec[1] 	# Adoption Status
				households$SIA[hh] <<- stepVec[2] 		# Updated SIA
				households$U[hh] <<- stepVec[3] 		# Updated Uncertainty
	#			print(households[hh,])
				}
			}	
		if (params$record == TRUE){ #Check to see if run should be recorded
			tabVec = genOutTable(households, i, runCount)
			outTable[i + 1, ] <<- tabVec # Note, table starts at tick 0, need to skip initial value
			}
		}
	if (params$graphIt == TRUE){
		print("Creating iGraph object from connections")
		from = graphFrom(households)
		to = graphTo(households)
		hhNet <<- createGraph(households, from, to) # Hard save
		}
	flush.console()
	}

#------------------------#
# Step Function
# Step Function
#------------------------#
step = function(hh){
# This is the household-level workhorse. Takes one argument,(hh) which is an individual household.
	# Run relative agreement, return a vector of the outcome
	if (households$Adopter[hh] == 0) { # Make sure household is not an adopter already
		ra = relativeAgreement(hh) # Return two part vector. ra[1] is SIA, ra[2] is Uncertainty
		
		} else {
			print(sprintf("Household with hhIndex: %s is already an adopter: %s", hh, households$Adopter[hh]))
			ra = c(households$SIA[hh], households$U[hh]) # If hh is an adopter, pass current SIA, U values
			}
	pbc = households$PBC[hh]
	# Check to see whether household should become an adopter. 
	# Takes the household number and updated SIA from Relative Agreement ra(), and PBC.
	check = adoptCheck(hh, ra[1], pbc) # ra[1] is SIA.  Returns T,F
	
	# Pass check vector to adopt function, with current attitude, uncertainty stored in ra. 
	adoptVec = adopt(hh, check, ra[1], ra[2]) # Returns vector of Adoption, SIA, and Uncertainty
	#print(sprintf("adoptVec : %s", adoptVec))	
	return(c(adoptVec[1], adoptVec[2], adoptVec[3])) # Adopt, SIA, Uncertainty
	}
	

#------------------------#
# Get Connections
#------------------------#
getConnections = function(households){
print ("Creating connections...")
# Initialize the households into small world networks
	conVec = rep(0, nrow(households)) 					# Create empty vector of length "households" to store connections
	# Add random connections. THIS IS SLOW. PARALLELIZE 
	for(house in households$hhIndex){
		numRandCon = rpois(1, params$lambda) 			# Number of non-Neighborhood connections
		conVec[house] = connect(households, numRandCon) # Use helper function to create a list of  RANDOM households, store in vector "conVec"
		# If empirical neighbors is set to off, create random neighbors
		if(params$empiricalNei == FALSE){
			neighbors = c(sample(households$PROP_ID, params$randCon)) 	# Random sampling of all households added to connections	
			} else {
				tarID = households[house, "PROP_ID"]
				statement = sprintf("SELECT V2 FROM %s WHERE V1 = %s", "neiDF", tarID)
				neighbors = as.character(sqldf(statement))
				neighbors = as.numeric(unlist(strsplit(neighbors, split = ",")))
				}
		conVec[[house]] = c(conVec[[house]], neighbors) # Add neighbors to connections
#		print(sprintf("Update : %s", conVec[[house]]))
		}
	rm(neiDF)
	return(conVec)
	}
	

#------------------------#	
# Connect random households 
#------------------------#
connect = function(dat, numC){
# Inner function of getConnections(). Places a one household into the network of another
# IDEA: calculate corellation from MValue, use only if cor > .7
		connections = list("connections" = 
			sample(dat$PROP_ID, numC, replace = FALSE)) 	# get "numC" random households, add to list
		return (connections)
		}
	
#------------------------#
# Relative Agreement
#------------------------#		

# Allows households to interact, potentially exchanging attitudes depended on uncertainty, "charisma"	
relativeAgreement = function(hh){
#	print(sprintf( "target household hhIndex: %s",(hh)))
#	print(sprintf("target SIA is : %s, U : %s", households$SIA[hh], households$U[hh]))
	priorSIA.j = households$SIA[hh] 					# Attitude of agent i (target)
	priorU.j = households$U[hh] 						# Uncertainty of agent i (target)
	
	#Find a connection
	cCount = 0
	jPROP = sample(households$connections[[hh]], 1) 	# Random connection within agent i's connections
		while(any(households$PROP_ID==jPROP) == FALSE){ # A few connections are bad, dont know why, but causes crash
#		print("Bad connection... trying again")
		
		jPROP = sample(households$connections[[hh]], 1, replace = FALSE)	# If the connection is bad, try another
		cCount = cCount + 1
		if(cCount >= length(households$connections[[hh]])){					# If all connections are bad, go to default
			jPROP = 303625													# This is the default. 2004, rich, well connected
#		print("Connected to default: 303625")
			}
		}
		
#	print(sprintf("connection PROP_ID is: %s", jPROP))
	jhhIndex  = households[households$PROP_ID == jPROP, "hhIndex"] # Search for index by j's PROP_ID
	priorSIA.i = households$SIA[jhhIndex] 				# Attitude of agent j (connection)
	priorU.i = households$U[jhhIndex]					# Uncertainty of agent i (target)
#	print(sprintf( "connection household hhIndex: %s",(jhhIndex)))
#	print(sprintf("connection SIA is : %s, U :  %s", households$SIA[jhhIndex], households$U[jhhIndex]))
	# Put variables into objects to be modified
	x.self = priorSIA.j
	u.self = priorU.j
	x.conn = priorSIA.i
	u.conn = priorU.i
	
	# Relative agreement algorithm: Deffuant et. al  http://jasss.soc.surrey.ac.uk/5/4/1.html
	overlap = (min((x.conn + u.conn),(x.self + u.self)) - max((x.conn - u.conn), (x.self - u.self)))
#	print(sprintf("Overlap: %s", overlap))
	nonOverlap = 2 * u.conn - overlap
	totAgreement = overlap - nonOverlap
	relAgreement = (2 * totAgreement) / (2 * u.conn)
	
	# Check if overlap is greater than uncertainty of the influencing agent (u.conn)
	if (overlap > u.conn){
#		print("Overlap was greater than uncertainty. Updating...")
		# Update target agent opinion and uncertainty
		x.self = x.self + params$mu * relAgreement * (x.conn - x.self)
		u.self = u.self + params$mu * relAgreement * (u.conn - u.self)	
#		print(sprintf("Old attitude: %s... New attitude: %s",priorSIA.j, x.self))
#		print(sprintf("Old uncertainty: %s... New uncertainty: %s",priorU.j, u.self))
		}
	flush.console()	
	return(c(x.self, u.self))
	}

#------------------------#
# Adopt Check
#------------------------#		
# Function sees if a household should be run through the adoption function
# Depends only on SIA and current adoption status	
adoptCheck = function(hh, sia, pbc){
	evaluation = FALSE 									# This is default, changed via "if" statement
	if ((sia >= params$siaThres) & (pbc >= params$pbcThresh)) {
		if (households$Adopter[hh] == 0) {
#			print(sprintf("Household with PROP_ID: %s and hhIndex: %s should adopt", households$PROP_ID[hh], hh))
			evaluation = TRUE
			}
		}
	return(evaluation)
	}
	

#------------------------#
# Adopt
#------------------------#	
# Adoption function. returns adoption status, SIA, and uncertainty. 
adopt = function(hh, check, SIA, U){
#	print ("Running Adopt Function")
	if (check == TRUE){
#	print(sprintf("Is adopter: %s", check))
		a = 1
		sia = 1.0
		u = 0.001
	 print(sprintf("Household with hhIndex: %s became an adopter", hh))
		if (params$record == TRUE){ 					# Record adopter status
			myrunlog$N_Adoptions[hh] <<- myrunlog$N_Adoptions[hh] + 1
			print("recorded adoption to myrunlog")
			}
		} else {
#		print(sprintf("Should adopt: %s", check))	
		a = 0
		sia = SIA 										# Original value passed to function
		u = U 											# Original values passed to function
			}
	if (households$Adopter[hh] == TRUE){
		a = 1
		sia = 1.0
		u = 0.001
#		print(sprintf("Adopt function found that household with hhIndex: %s was already an adopter", hh))
		}
	
	return(c(a, sia, u))
	}
	
#------------------------#
# Parallel Libraries
#------------------------#
# loads parallel libraries and defaults if params$doPar is set to "TRUE"
getPar = function(isParallel){
	if (isParallel == TRUE){
		library(doParallel)
		cl = makeCluster(3)
		registerDoParallel(cl)
		}
	}
	

#-----------------#
# Adopter Initialization
#-----------------#
# Set Adopter values on initialization
adopterInit =  function(hh){
	if (households$Adopter[hh] == 1){
		if (params$record == TRUE){ # Record adopter status
			myrunlog$N_Adoptions[hh] <<- myrunlog$N_Adoptions[hh] + 1 # Permanent assignment
			}
		sia = 1.0
		U = 0.001
		return(c(sia, U))
		}
	}
	
#------------------------#
# Update outTable dataframe
#------------------------#
# Creates a vector of calculated values. Order matches outTable	
genOutTable = function(hhDF, tk, runNum){
	run = outTable[(tk * runNum), "Run"]
	tick = tk
	quarter = outTable[(tick + 1) * runNum, "Quarter"] # This should keep quarter the same...
	sumAd = sum(hhDF$Adopter)
	newAd = sumAd - as.numeric(outTable[(tick) * runNum, "Sum_Adopters"]) # Current sum - previous
	cagr = "NA" # DEFAULT
	pctMar = sumAd / as.numeric(params$numHH)
	pctNAC = "NA" # DEFAULT
	pctNAPP = "NA" # DEFAULT
	nANet = "NA" # DEFAULT
	nNANet = "NA" # DEFAULT
	deltaAt = "NA" # DEFAULT
	deltaU = "NA" # DEFAULT
	conv = "NA" # DEFAULT
	tMedHV = "NA" # DEFAULT
	cMedHV = median(hhDF$MValue[hhDF$Adopter == 1])
	
	return(c(run, tick, as.character(quarter), newAd, sumAd, cagr, pctMar, pctNAC, pctNAPP, nANet, 
		nNANet, deltaAt, deltaU, conv, tMedHV, cMedHV)) # Return calculated values
	}

#-----------------------------#
# Helper for outTable: Runs
#-----------------------------#
# Creates a vector of repeated run numbers
outHelper = function(rns, tks){
	vec1 = c()
	for(i in 1:rns){
		x = rep(i, tks + 1) # add "tick 0"
		vec1 = c(vec1, x)
		}
	return(vec1)
	}
	
#-----------------------------#
# SNA Graph: From Nodes
#-----------------------------#
graphFrom = function(nodes){
	fVec = rep(0, sum(sapply(nodes$connections, length))) # empty vector with length(number of households * sum(connections))
	curIndex = 0
	for(i in nodes$hhIndex){ # THIS IS SLOW PARALLELIZE
		fVec[(curIndex+1) : (curIndex + length(nodes$connections[[i]]))] = i
		curIndex = curIndex + length(nodes$connections[[i]]) # add one so that previous entry is not overwritten
#		print(fVec[(curIndex+1) : (curhhIndex + length(nodes$connections[[i]]))])
#		readline("press enter to continue")
		}
	return(fVec)
	}

#-----------------------------#
# SNA Graph: To Nodes
#-----------------------------#
#sapply(unlist(dat$connections), rbind)	
graphTo = function(nodes){
	tVec = rep(0, sum(sapply(nodes$connections, length))) # empty vector with length(number of households * sum(connections))
	curIndex = 0
	for(i in nodes$hhIndex){ # THIS IS SLOW. PARALLELIZE
		tVec[(curIndex+1) : (curIndex + length(nodes$connections[[i]]))] = nodes$connections[[i]]
		curIndex = curIndex + length(nodes$connections[[i]]) # add one so that previous entry is not overwritten
#		readline("press enter to continue")
		}
	return(tVec)
	}

#-----------------------------#
# SNA Graph: Create iGraph Object
#-----------------------------#
createGraph = function(dat, from, to){
	cat("Writing Edges")
	vertex.attrs = list(name = dat$hhIndex)
	edges = rbind(match(from, vertex.attrs$name),
				match(to, vertex.attrs$name))	
	e2 = c()
	# THIS IS SLOW PARALLELIZE
	for (i in 1:ncol(edges)){
		if(is.na(edges[2,i]) == F){
		e2 = rbind(e2,edges[,i])}
		}
	
	G = graph.empty(n=0, directed = T) # Create the empty graph object
	G = add.vertices(G, length(dat$hhIndex)) # Add a node for each household
	G = add.edges(G, e2) # create a column of 
	return(G)
	}

#-----------------------------#
# Calculated Perceived Behavioral Control
#-----------------------------#
getPBC = function(mv, sq, op){
# This function is currently simplistic: PBC is avg(log(at_j+1))
	pt1 = log(op + 1)
	pt2 = log(sq + 1)
	pt3 = log(op +1)
	pbc = (pt1 + pt2 + pt3) / 3 
	return(pbc)
	#pbc  = rlogis(1, -0.0146, 0.387)
	}
	
#-----------------------------#
# Calculated Socially Informed Attitude 
#-----------------------------#	
getSIA = function(hh){
	rlogis(1, 0.04, 0.277) # Random distribution from empirical ADOPTER data	
	}