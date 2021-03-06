# This function removes the cycles from the Living nodes for precise trophic aggregation results
### Cycle Removal of Living Nodes
### Singh P. | July 2014
### -----------------------------------------------
cycliv <- function(x){
	
		 #Initials
    if(class(x)!='network') {stop("x is not a network class object")}
    web.all <- x %n% "flow"
    y.all   <- x %v% "output"
    liv <- x %v% 'living'
    TPTS.all <- apply(web,2,sum)+y
    #-------------------------------
    N <- sum(liv)
    web <- web.all[1:N,1:N]
    y <- y.all[1:N]
    TPTS <- apply(web,2,sum)+y
    F <- web/TPTS
    TST <- sum(TPTS)
    # -----------------------------
    ###-----------------------------------------------------------------

                                        #Zero Global Variables
    NFST <- NEXNUM <- NCYC <- 0
    CYCS <- rep(0,N)

###-----------------------------------------------------------------

                                        #Start primary repeat loop
    repeat {
                                        # Zero all local variables
        NNEX  <- 0
        TCYCS <- rep(0,N)
        TMP   <- web*0
                                        #-----------------------------------

                                        #Count cycle arcs and determine exit from return
        NFWD <- rep(NULL, N)
        NTEMP <- rep(NULL, N)
        for (ii in 1:N) {#do 200 ii=1,N
            for (i in 1:N) {
                NFWD[i] <- 0
            } #150
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
                if ((NFWD[i]>0) && (web[i,ii]>0)) {NTEMP[ii] <- NTEMP[ii]+1}
            }
        }#200
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
            if (NMAX > 0) {NSTP <- NSTP + 1}
            NTEMP[JMAX] <- -2
            MAP[i] <- JMAX
        }
        #print(c('NSTP',NSTP))


        ### Condition for breaking/Exiting the primary repeat loop
        ###----------------------------------------
        if (NSTP<=0) {break} #breaks the primary repeat loop
        ###----------------------------------------

                                        # Start the NSTP2 While loop for Critical Arc determination
        ###----------------------------------------------------------------------------------------------
        NSTP2 <- 0
        while(NSTP2<=0) {
            slf.loop<-FALSE
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
            ### Exit from while(NSTP2<=0) if slf.loop
            if (IMIN == JMIN) {
                slf.loop <- TRUE
                break    #-----------------------BREAK THE WHILE LOOP
            }
                                        #Make sure at least one cycle contains the current smallest arc web[IMIN,JMIN]
            NHALF <- (N/2)+1
            NFWD <- rep(0,N)
            NFWD[JMIN] <- 1
            ### find nodes from JMIN in fwd dirctn
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
            ### find nodes from IMIN in bckwd dirctn
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
            ### find Common nodes aka members of the NEXUS
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
        } ###End of While NSTP2<=0
        ###----------------------------------------------------------------------------------------

                                        #IF the Critical arc is self loop
                                       #---------------------------------
        if(slf.loop == TRUE) {
            NCYC <- NCYC+1
            NNEX <- NNEX+1
            CYCS[1]<- CYCS[1]+web[IMIN,JMIN]
            WKARC<-F[IMIN,JMIN]*TPTS[IMIN]
            print(c(NCYC,'.','(',IMIN,JMIN,')'))
            web[IMIN,JMIN] <- 0
            NEXNUM <- NEXNUM+1
            print(c(NEXNUM,'Consists of',NNEX,'cycles','Weak arc:','(',IMIN,JMIN,')=',WKARC))
            NFST <- 1
        }#End of if(slf.loop==TRUE)#

                                        #Begin Search for NEXUS defined by web[IMIN,JMIN] if not a self loop
        ###----------------------------------------------------------------------------------------

        else {
            WHOLE <- 0
                                        # Backtrack Routine Starts --------------
             # Backtrack Routine Starts --------------
            ### Initialize Node and Level
            LEVEL  <- 2
            NODE[1]<- 1
            NODE[2]<- 2
            skip.con.adv <- FALSE
            nex.com <- FALSE

            ### 2 Repeats start. rep1,2.
            repeat { #rep1
                repeat { #rep2
                    skip.con.chk <- FALSE

             ## Adv to nxt levels
                    if(skip.con.adv==FALSE) {
                        LM1         <- LEVEL
                        LEVEL       <- LEVEL+1
                        NODE[LEVEL] <- 1
                    }
### 2 Repeats start. rep3,4
                    repeat { #rep3
                        repeat { #rep4
                            ## Check for conn. b/w nodes at prsnt levels
                            if(skip.con.adv==FALSE){
                                NZ1  <- NODE[LM1]
                                KROW <- MAP2[NZ1]
                                NZ2  <- NODE[LEVEL]
                                KCOL <- MAP2[NZ2]
                                conn.chk <- FALSE
                                ## break rep4 and rep3 if conn exists
                                if(web[KROW,KCOL]>0 && skip.con.chk==FALSE) {
                                    conn.chk <- TRUE
                                    break #brk rep4
                                }
                            }
                            ## try next node in nxt level
                            NODE[LEVEL] <- NODE[LEVEL]+1
                            skip.con.adv <- FALSE
                            skip.con.chk <- FALSE
                            ## break rep4 after all levels are checked(NODE[level]>NSTP2)
                            if(NODE[LEVEL]>NSTP2) {break} #brk rep4
                        }#end of rep4
                        if(conn.chk==TRUE) {
                            conn.chk <- FALSE
                            break #rep3
                        }
### Backtrack to prev. level
                        LEVEL <- LEVEL-1
                        LM1   <- LEVEL-1
                        ## if further backtracking is impossible,
                                        #end search under weak arc. nexus complete
                        nex.com<-FALSE
                        if(LEVEL <= 2){
                            nex.com <- TRUE
                            break #break rep3
                        }
                        else {skip.con.chk<-TRUE} #goes to #420 to inc NODE[LEVEL]
                    }#end of rep3
                    if(nex.com==TRUE) {break} #break rep2
                    ##break rep2 if this conn completes cycle
                    if(NODE[LEVEL]==1) {break} #brk rep2
                    skip.con.adv <- FALSE
                    for (k in 1:LM1) {if (NODE[LEVEL] == NODE[k]) {skip.con.adv <- TRUE}}

                }#end of rep2
                if(nex.com==TRUE) {break} #brk rep1
                                        # -----------------BR Ends --------------
                                        # Calculate circuit prob
                WEIGHT <- 1
                for (kk in 1:LM1) {
                    KKP1 <- kk+1
                    KROW <- NODE[kk]
                    KCOL <- NODE[KKP1]
                    KROW <- MAP2[KROW]
                    KCOL <- MAP2[KCOL]
                    WEIGHT <- WEIGHT*F[KROW,KCOL]
                }
                                        # Add this weight
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
                                        # Report this cycle
                NNEX <- NNEX+1
### #KTRY <- MOD(NNEX,5000)
                print('Nexus Cycles Counting')
                NCYC <- NCYC+1
                L0 <- LM1+1
                for (kk in 1:L0) {
                    NTMP <- NODE[kk]
                    NTEMP[kk] <- MAP2[NTMP]
                }
                print(c(NCYC,'.',NTEMP))
                skip.con.adv<-TRUE
            }#end of rep1
                                        #-----------------NEXUS COMPLETED---NEXUS REPEAT(rep1) ENDS HERE



                                        # Report this NEXUS
            WKARC=F[IMIN,JMIN]*TPTS[IMIN]
            NEXNUM=NEXNUM+1
            print(c('NEXUS',NEXNUM,'Consists of',NNEX,'cycles.','Weak arc:','(',IMIN,JMIN,')=',WKARC))
            # -------------------------------------
            # Normalize Probability Matrix & Subtract proper amounts from web
            PIVOT <- TMP[IMIN,JMIN]
            if(PIVOT<=0){print('Error in Normalizing Nexus Weights')}
            for(i in 1:N) {
                for(j in 1:N) {
                    if(web[i,j]<=0) {next}
                    web[i,j] <- web[i,j]-((TMP[i,j]/PIVOT)*ARCMIN)
                }
            }

            # Add proper amts to cycle distributions
            for(i in 1:N){CYCS[i]<-CYCS[i]+((TCYCS[i]/PIVOT)*ARCMIN)}

            # Zero weak arc
            web[IMIN,JMIN] <- 0
            NFST <- 1
            if(WHOLE>1.00001) {print(c('Bad Sum Check = ',WHOLE))}


        }#End of else i.e. if not a slf.loop
    }#End of primary repeat loop

                                              # Report the Overall Results

    ### FIRST, UNCOVER ANY LINKS "HIDDEN" DURING SEARCH.
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
    }#end of if (NFST!=0)
    else {print('No Cycles Detected')
    	out <- list(WEB=web,TPTS=TPTS, TST=TST, F=F)
    	return(out)}
    
    
    
    
} # End of Function