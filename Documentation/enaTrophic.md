Trophic Aggregations
====================

The function **enaTrophic** in enaR gives the results of the trophic aggregations corresponding to the Ulanowicz school which interprets the given Model according to Lindeman's Trophic Concepts based on the interactions among the living nodes of the ecosystem and the effect of the Detrital Pool (i.e. the non-living components of the ecosystem. 

It is the third section of results in the well-known network analysis software *NETWRK 4.2b* by Dr. Ulanowicz. The enaTrophic function replicates the same results as presented in NETWRK.

The trophic aggregation algorithm requires the Network object to be ordered such that the non-living nodes appear at the end. (like all the models stored in enaR)

If the inputs of some species do not represent primary productions, but immigrations instead. This function distinguishes between these two types of inputs and treat them differently during the Analysis.

The following are the different results that the function returns in the form of a list.

`>T<-enaTrophic(m)`
`>attributes(T)`
`$names
 [1] "A"    "ETL"  "CE"   "CR"   "GC"   "RDP"  "DTRY" "INPD" "CIRD" "LS"   "TE"  `

1. Lindeman Transformation Matrix (A)
   ----------------------------------
   It is a matrix with dimension = NL X NL where NL=no. of living nodes in m. The columns of A represent the distribution of the corresponding nodes into distinct integral trophic levels. The sum of the columns hence, must be 1. Every row of A gives the composition of that particular trophic level. By convention, the abiotic activity in the network is pushed to the Nth compartment of A and is assigned a trophic level of 1 for further analysis.

2. Effective Trophic Levels (ETL)
   ------------------------------
   The ETL vector gives the effective trophic levels of each living component of the network. The i^th component of the ETL vector is obtained by the summation of the i^th column of the transformation matrix weighted by the value of the trophic level: which represents the effective trophic level of the i^th species in the network.

3. Canonical Exports (CE)
   ----------------------
   The canonical exports vector represents the exports leaving the system from the integer trophic levels.

4. Canonical Respirations (CR)
   ---------------------------
   Canonical respirations vector represents the respiration values for integral trophic levels.

5. Grazing Chain (GC)
   ------------------
   The GC vector represents the Grazing Chain for the Network. This depicts the input flow to the trophic levels from the previous trophic level. (The first value is the aggregate of the exogeneous inputs, the next value represents herbivorous grazing and so on.)

6. Returns to Detrital Pool (RDP)
   ------------------------------
   This vector represents the amount of flow from the respective trophic levels to the detrital pool (or the non-living components) in the network.

7. Detrivory (DTRY)
   ----------------
   Detrivory is the flow from the detritus to the second trophic level. The ratio of Detrivory to the Herbivorous grazing (i.e. GC[2]) depicts the relative importance of recycle to the community.

8. Input to Detrital pool (INPD)
   -----------------------------
   The INPD represents the exogeneous input to the detrital pool.

9. Circulation within the Detrital Pool (CIRD)
   -------------------------------------------
   This value represents the Internal circulation within the detrital pool. 

10. Lindeman Spine (LS)
    -------------------
    The Lindeman spine combines the Detrital pool with the autotrophs and forms a monotonically decreasing sequence of flows from one trophic level to the next, starting with the said combination. The Grazing Chain is not a monotone decreasing sequence of flows due to the effects of recycle.

11. Trophic Efficiencies (TE)
    -------------------------
    It is the ratio of input to a trophic level to the amount of flow that is passed on the next level from it. Clearly, it depicts the efficiency that a particular trophic level is exhibiting in the ecosystem in terms of input and output of flows.




