 # Backtrack Routine Starts --------------
            ### Initialize Node and Level
            LEVEL  <- 2
            NODE[1]<- 1
            NODE[2]<- 2
            skip.con.adv <- FALSE

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
                        if(LEVEL<=2){
                            nex.com<-TRUE
                            break #break rep3
                        }
                        else {skip.con.chk<-TRUE} #goes to #420 to inc NODE[LEVEL]
                    }#end of rep3
                    if(nex.com==TRUE) {break} #break rep2
                    #break rep2 if this conn completes cycle
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
               #KTRY <- MOD(NNEX,5000)
                print(c(NNEX,'Nexus Cycles and Counting'))
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
