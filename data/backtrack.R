           ### The backtracking Routine for the NETWRK's Full Cycle Analysis
           ### Singh P. | July 2014
           ### Source for the Algorithm: Ulanowicz 1983: Identifying the Structure of Cycling in Ecosystems : Mathematical Biosciences 65:219-237
           ### Johnson D.B.:Finding the Elimentary Circuits of a Directed Graph: SIAM J Comput. Vol.4, Iss.1. 1975
           
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
# 410 #########################
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
  		  	if(prev.chk==TRUE) {}  		  			
  		  	else {break}  		  	
  	} #End of Repeat1
  	if(lev.chk==TRUE) {break}
  	if(comp.chk == TRUE) {break} #NORMAL EXIT FROM Repeat #CYCLE COMPLETE
  } #End of Repeat
#########################################################################
#########################################################################
#END OF BACKTRACKING ROUTINE
  