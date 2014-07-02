troAgg <- function (x, zero.na = TRUE, balance.override = FALSE){
  if (class(x) != "network") {
    stop("x is not a network class object")
  }
  Ti <- x %v% "input"         #### Inputs
  exp <- x %v% "export"
  exp[is.na(exp)] <- 0
  res <- x %v% "respiration"
  res[is.na(res)] <- 0
  n <- length(Ti)
  liv <- x %v% "living"     ##Living vector
  nl = sum(liv)             ##Sum of the living nodes
  flow <- x %n% "flow"      ## flow is the flow matrix /EXCHNGE matrix
  T <- Ti + apply(flow, 2, sum) ## Input throughflow vector
  Tinp = matrix(T, ncol=n, nrow=n, byrow='TRUE')
  GP <- flow[1:n,1:n]/Tinp  ##Input direct flow intensity matrix
  if (zero.na) {
    GP[is.na(GP)] <- 0
  }
  else {
  }
  I <- GP * 0
  diag(I) <- 1
  NP <- ginv((I - GP))

                                        # Lindeman Trophic Aggregations # Singh P. July 2014. Ulanowicz and Kay 1991. Environmental Software 6:131-142.
  a = Ti/T          # First Row Vector of Lindeman Transformation Matrix##
  km1 = NULL
  a[is.na(a)] <- 0
                                        #Lindeman transformation matrix
  A = matrix(0,nrow=n,ncol=n,byrow='true')
  A[1,1:nl] <- a[1:nl]      # 950     A(1,I)=BINPUT(I)
  for (k in 2:nl){
    km1 <- k-1
    for (l in 1:nl)
      if((k>2) | (nl>=n)) {
        for( j in 1:nl){
          A[k,l] <- A[k,l]+A[km1,j]*GP[j,l]
        }
      }
      else {
        for(k2 in (nl+1):n) {
          A[k,l] <- A[k,l]+GP[k2,l]
        }
      }

  }
  if (nl<n) {
    A[n,(nl+1):n] <- 1
  }
                                        # A is the required Lindeman transformation matrix

                                        # 2. Effective Trophic Levels
  MF = matrix(1:n, nrow=n, ncol=n, byrow = 'FALSE')
  etl = rep(1,n)
  etl[1:nl] = apply((MF*A)[1:nl,1:nl],2,sum)

                                        # 3. Canonical Exports
  cel = exp
  ce = A %*% exp
  ce1 = as.vector(ce)

                                        # 4. Canonical Respirations
  crl = res
  cr = A%*%res
  cr1=as.vector(cr)

                                        # 5. Grazing Chain
  gc <- rep(0,nl)
  gc[1] <- sum(A[1,]*T)
  AT = A %*% flow %*% t(A)
  gc[2:nl]=apply(AT[1:(nl-1),1:nl],1,sum)

                                        # 6. Returns to Detrital Pool
  rtd <- AT[1:nl,n]

                                        # 7. Detrivory
  dtry <- sum(AT[n,1:nl])

                                        # 8. Input to Detrital Pool
  U <- A %*% Ti
  dinp <- sum(U[(nl+1):n])

                                        # 9. Circulation within Detrital Pool
  dcir <- AT[n,n]

                                        # 10. Lindeman Spine
  ls = gc
  ls[1] = sum(rtd[2:nl]) + gc[1] + dinp
  ls[2] = gc[2]+dtry

                                        # 11. Trophic Efficiencies
  te=ls
  for(i in 1:nl){
    if(te[i]<=0){break}
    te[i]=te[i+1]/te[i]
  }
  te[is.na(te)] <- 0


  out <- list(A = A, ETL = etl, Cexp = ce1, Cresp = cr1, GrazingChain = gc, ReturnsDetritalPool = rtd, Detrivory = dtry, D_INP = dinp, D_CIR = dcir, Lindeman_Spine = ls,T_eff = te)
  return(out)
}
