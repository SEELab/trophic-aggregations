pavCyc <- function (x) 
{
	#####initials####
if(class(x)!='network') {stop("x is not a network class object")}
        Ti <- x %v% "input"
        exp <- x %v% "export"
        exp[is.na(exp)] <- 0
        res <- x %v% "respiration"
        res[is.na(res)] <- 0
        Tj <- exp + res
        flow <- x %n% "flow"
#########################NORMAL NTEMP3 FUNCTION #########################
                                        # Calculating the no. of Incoming Cycle Arcs for each node
web <- x%n%'flow'
N = dim(web)[1]
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
ntemp1=NTEMP # Storing the cycle values in ntemp1
                                        # Claculating the order of Nodes(decreasing no. of Cycle Arcs) in MAP

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
out <- list(NTEMP_no_of_incomingcyclearcs=ntemp1,MAP=MAP)
#for (s in 1:N) {print(ntemp1[MAP[s]])}
#return(out)
#####################################################################################
                                        # if NSTP = 0 then goto the report of the ovrall result
                                        # from cycling elements, search for the smallest non-zero arc
#
if (NSTP>0) { # NSTP==0 implies no cycling in the network.
                                        #Enter this whole process of eliminating cycles iff there is cycling
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
   # Obtained ARCMIN is a candidate for Critical Arc. If it forms a NEXUS and NSTP2 becomes>0
   #ARCMIN = 8.18356e-11 = F[37,61] for troModels[[48]]
   #print(IMIN)
   #print(JMIN)

                                        # we will be treating self loops separately (if IMIN = JMIN) ??//why not refine them beforehand??//??
                                        # because self loops will be documented separately. #self loops = cycles
   if (IMIN != JMIN) {
                                        # find nodes that can be reached from JMIN in the forward direction
      NHALF <- (N/2)+1
      NFWD <- rep(0,N)
      NFWD[JMIN] <- 1
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
      nfwd=NFWD
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
      node=NODE
                                        # Map all nodes common to both searches they will all be members of the nexus defined by
                                        # web[IMIN,JMIN] (ARCMIN)
      NSTP2 <- 0
      MAP2 <- rep(0,NSTP)
      for (i in 1:NSTP) {
          if ((NFWD[MAP[i]] <= 0) | (NODE[MAP[i]] <= 0)) {next}
          NSTP2 <- NSTP2 + 1
          MAP2[NSTP2] <- MAP[i]
      }
      map2=MAP2
      # print(NSTP2)-checked ~60 for tro48 = 66 nodes
                                        # reorder mapping for IMIN and JMIN to come 1st and 2nd
      NFWD <- MAP2 # NFWD stores the values of MAP2 for reordering
      MAP2[1] <- IMIN
      MAP2[2] <- JMIN
                                        ##### SHORTER WAY POSSIBLE ####
      if (NSTP2 > 2) {
          INDX <- 2
          for (i in 1:NSTP2) {
              if ((NFWD[i] == IMIN)||(NFWD[i]==JMIN)) {next}
              INDX <- INDX+1
              MAP2[INDX] <- NFWD[i]
          }
      }

                                        ##### ################### ####
      if (NSTP2 <= 0) {web[IMIN,JMIN] = -web[IMIN,JMIN]}
  }#end of if(IMIN!=JMIN) ie if not a self loof
   else{#corresponding to if IMIN!=JMIN ### FOR NOW THIS IS A NON-ENDING LOOP#
            ###BEWARE##### # calCC might be incorporarted for the output corresponding to the self loop
                           # The output might be added separately as well. Listing might create problems though.
   }




  }#end of the while(NSTP2<=0) ARCMIN returns # We have found the Critical Arc when loop exits.




} #end of if (NSTP>0) #Cycling in the Network

out <- list(NFWD=nfwd, NODE=node, MAP2 = map2, MAP3 = MAP2)
return(out)
}#end of the function
