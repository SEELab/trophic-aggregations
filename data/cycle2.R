           ### NETWRK's Full Cycle Analysis
           ### Singh P. | July 2014
           ### -----------------------------------------------
       
Cycle2 <- function (x) 
{
	#####initials####
if(class(x)!='network') {stop("x is not a network class object")}
       web <- x %n% "flow"
       y <- x %v% "output"
       N <- length(y)
       TPTS <- apply(web,2,sum) + y
       F <- web/TPTS
       TST <- sum(TPTS)
       
####################################
     #ZERO ALL GLOBAL VARIABLES
     NFST <- NEXNUM <- NCYC <- 0
     CYCS <- rep(0,N)
     
     #ZERO ALL LOCAL VARIABLES
     repeat{#LOCAL VARIABLES REPEAT
     	NNEX  <- 0
     	TCYCS <- rep(0,N)
     	TMP   <- web*0
     	
     	#Determine the no. of Starting Points COUNT THE NUMBER OF
#C  INCOMING CYCLE ARCS FOR EACH VERTEX.  ARRANGE VERTCIES BY DECREASING
#C  NUMBER OF CYCLE ARCS.  ELIMINATE VERTICIES WITH NO CYCLE ARCS FROM
#C  FURTHER ANALYSIS.
     	NFWD <- rep(NULL, N)
     	NTEMP <- rep(NULL, N)
     	for (ii in 1:N) {
     		for (i in 1:N) {
     			NFWD[i] <- 0
     		}
     		NFWD[ii] <- 1
     		for (k in 1:(N-1)) {
     			for (i in 1:N) {
     				if(NFWD[i]>0) {next}
     				for (j in 1:N) {
     					if (NFWD[j] <1) {next}
     					if (web[j,i]<=0) {next}
     					NFWD[i] <- 1
     					break
     				}
     			}
     		}
     		NTEMP[ii] <- 0
     		for (i in 1:N) {
     			if ((NFWD[i]>0) && (x[i,ii]>0)) {NTEMP[ii] <- NTEMP[ii]+1}
     		}
     	}
     	# NTEMP will give the no. of Cycles ending in each node
     	NSTP <- NMAX <- JMAX <- 0
     	MAP <- rep(0,N)
     	for (i in 1:N) {
     		NMAX <- -1
     		for (j in 1:N) {
     			if (NTEMP[j]<=NMAX) {next}
     			NMAX <- NTEMP[j]
     			JMAX=j
     		}
     		if (NMAX >= 0) {NSTP <- NSTP + 1}
     		NTEMP[JMAX] <- -2
     		MAP[i] <- JMAX
     	}
     	# MAP gives the order of nodes. NSTP is the no. of steps ??//to limit the loops//??
     	if (NSTP>0) {# NSTP==0 implies no cycling in the network.
     		NSTP2<-0
     		while(NSTP2<=0){ ###This loop ascertains Critical Arc.
     			ARCMIN <- 10^25 #arbitrary min arc
     			for (ir in 1:NSTP) {
     				for (ic in 1:NSTP) {
     					IRTP <- MAP[ir]  # IRTP is the node to be searched at the ir'th position
     					ICTP <- MAP[ic]  # ICTP ----------------------------------ic'th --------
     					if (web[IRTP,ICTP] <= 0) {next}
     					if (web[IRTP,ICTP] >= ARCMIN) {next}
     					ARCMIN <- web[IRTP,ICTP]
     					IMIN <- IRTP
     					IM <- ir
     					JMIN <- ICTP
     					JM <- ic
     				}
     			}
     			ieqj<-FALSE
     			#Treat Self Loops as Min Arc Separately (LATER)
     			if (IMIN == JMIN) {
     				ieqj <- TRUE
     				break} #break the while loop
     			if (IMIN != JMIN) {
     				NHALF <- (N/2)+1
     				NFWD <- rep(0,N)
     				NFWD[JMIN] <- 1
     				# find nodes that can be reached from JMIN in the forward direction
     				#search pathways upto NHALF links
     				for (k in 1:NHALF) {
     					for (i in 1:N) {
     						if (NFWD[i] >0) {next}
     						for (j in 1:N) {
     							if (NFWD[j] < 1) {next}
     							if (web[j,i] <= 0) {next}
     							NFWD[i] <- 1
     							break
     						}
     					}
     				}
     				#Find nodes that can be reached from IMIN in the backward direction
     				NODE <- rep(0,N)
     				NODE[IMIN] <- 1
     				for (k in 1:NHALF) {
     					for (i in 1:N) {
     						if (NODE[i] > 0) {next}
     						for (j in 1:N) {
     							if (NODE[j] < 1) {next}
     							if (web[i,j] <= 0) {next}
     							NODE[i] <- 1
     							break
     						}
     					}
     				}
     				# Map all nodes common to both searches they will all be members of the nexus           
     				# defined by web[IMIN,JMIN] (ARCMIN)
     				NSTP2 <- 0  ###### ?????
     				MAP2 <- rep(0,NSTP)
     				for (i in 1:NSTP) {
     					if ((NFWD[MAP[i]] <= 0) | (NODE[MAP[i]] <= 0)) {next}
     					NSTP2 <- NSTP2 + 1
     					MAP2[NSTP2] <- MAP[i]
     				}
     				# reorder mapping for IMIN and JMIN to come 1st and 2nd
     				NFWD <- MAP2
     				MAP2[1] <- IMIN
     				MAP2[2] <- JMIN ##### SHORTER WAY POSSIBLE ####
     				if (NSTP2 > 2) {
     					INDX <- 2
     					for (i in 1:NSTP2) {
     						if ((NFWD[i] == IMIN)||(NFWD[i]==JMIN)) {next}
     						INDX <- INDX+1
     						MAP2[INDX] <- NFWD[i]
     					}
     				}
     				if (NSTP2 <= 0) {web[IMIN,JMIN] = -web[IMIN,JMIN]}     				
     		   }#end of if(IMIN!=JMIN) ie if not a self loof
     		} #END OF While NSTP2<=0
     		
     		if(ieqj == FALSE) {
     			# Whole is used to check the probability normalization process
     			WHOLE <- 0
     			
     			##############NETWORK BACKTRACKING ROUTINE################
     			  # Define NODE. NODE and web come from the previous part of the program
     			  # INITIALIZE STARTING NODE AND LEVEL
     			  LEVEL   <- 2
     			  NODE[1] <- 1
     			  NODE[2] <- 2
     			  prev.chk <- lev.chk <- FALSE
     			  repeat { #call it Repeat
     			  	#Advance to next levels 
     			  	#400#          
  	               LM1 <- LEVEL
  	               LEVEL <- LEVEL+1
  	               NODE[LEVEL] <- 1
  	               repeat { #Call it repeat1
  	               	repeat { #Call it Repeatsmall
  	               		# CHECK FOR CONNECTION BETWEEN NODES AT PRESENT TWO LEVELS.
  	               		#410#########################
  	               		if(prev.chk==FALSE) {
  	               			NZ1  <- NODE[LM1]
  	               			KROW <- MAP2[NZ1]
  	               			NZ2  <- NODE[LEVEL]
  	               			KCOL <- MAP2[NZ2]
  	               			conn.chk=FALSE
  	               			if(web[KROW,KCOL]>0) { #conn. exists. 
  	               				conn.chk=TRUE
  	               				break
  	               			}
  	               		}
  	               		prev.chk <-FALSE
  	               		# 420 ########################
  	               		# Try next node in next level 
  	               		NODE[LEVEL] <- NODE[LEVEL]+1
  	               		if(NODE[LEVEL]>NSTP2) {break}
  	               	} #End of repeatsmall
  	               	if(conn.chk==FALSE){
  	               		# BACKTRACK TO PREVIOUS LEVEL. 
  	               		# 430 ########################
  	               		LEVEL <- LEVEL-1
  	               		LM1   <- LEVEL-1
  	               		lev.chk <- FALSE
  	               		if(LEVEL<=2) {lev.chk <- TRUE ############# 2 BREAKS NEEDED here
  	               			break
  	               		}
  	               	}
  	               	#IF THIS CONNECTION COMPLETES CYCLE, REPORT THE RESULTS. 
  	               	# 440 ########################
  	               	comp.chk<-FALSE
  	               	if(NODE[LEVEL]==1) {comp.chk<-TRUE	
  	               		break} #Exit from repeat 1
  	               		# CHECK IF THIS NODE APPEARS PREVIOUSLY IN PATHWAY
  	               		prev.chk <- FALSE
  	               		for(k in 1:LM1) {
  	               			if(NODE[LEVEL]==NODE[k]) {
  	               				prev.chk <- TRUE} }
  	               		if(prev.chk==TRUE) {} #Continue with the loop repeat1
  	               		else {break}
  	               	} #End of Repeat1
  	               	if(lev.chk==TRUE) {break}
  	               	if(comp.chk == TRUE) {break} #NORMAL EXIT FROM Repeat #CYCLE COMPLETE
  	               } #End of Repeat
  	               #END OF BACKTRACKING ROUTINE

     			##########################################################
     			# Calculate the Circuit Probability of this Cycle
     			WEIGHT <- 1
     			for (kk in 1:LM1) {
     				KKP1 <- kk+1
     				KROW <- NODE[kk]
     				KCOL <- NODE[KKP1]
     				KROW <- MAP2[KROW]
     				KCOL <- MAP2[KCOL]
     				WEIGHT <- WEIGHT*F[KROW,KCOL]
     			}
     			# Add this weight to the Temporary Storage Matrix
     			for (kk in 1:LM1) {
     				KKP1 <- kk+1
     				KROW <- NODE[kk]
     				KCOL <- NODE[KKP1]
     				KROW <- MAP2[KROW]
     				KCOL <- MAP2[KCOL]
     				# Also, add this amount to the cycle distributions
     				TCYCS[LM1] <- TCYCS[LM1]+WEIGHT
     				TMP[KROW,KCOL] <- TMP[KROW,KCOL]+WEIGHT
     			}
     			#Report this cycle
     			NNEX <- NNEX+1
     			#KTRY <- MOD(NNEX,5000)
     			print(c(NNEX,'Nexus Cycles and Counting'))
     			NCYC <- NCYC+1
     			L0 <- LM1+1
     			for (kk in 1:L0) {
     				NTMP <- NODE[kk]
     				NTEMP[kk] <- MAP2[NTMP]
     			}
     			print(c(NCYC,'.',NTEMP))
     			WKARC=F[IMIN,JMIN]*TPTS[IMIN]
     			NEXNUM=NEXNUM+1
     			print(c(NEXNUM,'Consists of',NNEX,'cycles','Weak arc:','(',IMIN,JMIN,')=',WKARC))
     			#After NEXUS has been completed, Normalize the Probability Matrix
     			#Subtract proper amounts from WEB
     			PIVOT <- TMP[IMIN,JMIN]
     			if(PIVOT<=0){print('Error in Normalizing Nexus Weights')}
     			for(i in 1:N) {
     				for(j in 1:N) {
     					if(web[i,j]<=0) {next}
     				    web[i,j] <- web[i,j]-((TMP[i,j]/PIVOT)*ARCMIN)
     				}
     			}
     			#Add proper amounts to the cycle distributions
     			for(i in 1:N){CYCS[i]<-CYCS[i]+((TCYCS[i]/PIVOT)*ARCMIN)}
     			#Zero this weak arc before proceeding 
     			web[IMIN,JMIN] <- 0
     			NFST <- 1
     			if(WHOLE>1.00001) {print(c('Bad Sum Check = ',WHOLE))}
     			
     			
     		}### end of if (ieqj==false)##
     		else {
     			NCYC <- NCYC+1
     			NNEX <- NNEX+1
     			CYCS[1]<- CYCS[1]+web[IMIN,JMIN]
     			WKARC<-F[IMIN,JMIN]*TPTS[IMIN]
     			print(c(NCYC,'.','(',IMIN,JMIN,')'))
     			web[IMIN,JMIN] <- 0
     			NEXNUM <- NEXNUM+1
     			print(c(NEXNUM,'Consists of',NNEX,'cycles','Weak arc:','(',IMIN,JMIN,')=',WKARC))
     			NFST <- 1    			
     		}
     		break()    	
       } #End of IF (NSTP>0)
       else {break}	
     }#Endof LOCAL VARIABLES REPEAT ## Use for goto130
     # Report the Overall Results
     # FIRST, UNCOVER ANY LINKS "HIDDEN" DURING SEARCH.
     web=abs(web)
     if(NFST!=0) {
     	print(c('A total of ',NCYC,'Cycles removed'))
     	print('Cycle Distributions')
     	print(CYCS)
     	cycs<-CYCS
     	CYC  <- sum(CYCS)
     	CYCS <- CYCS/TST
     	print('Normalized Distribution')
     	print(CYCS)
     	TEMP <- CYC/TST
     	print(c('cycling index is',TEMP))
     	out <- list(CycleDist = cycs, NormDist=CYCS, CycleIndex = TEMP, WEB=web)
     	return(out)
     }
     else {print('No Cycles Detected')}
     
     
     
     
     
}#END OF FUNCTION Cycle2