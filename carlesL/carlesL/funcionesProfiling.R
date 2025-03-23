#Calcula els valor test de la variable Xnum per totes les modalitats del factor P
ValorTestXnum <- function(Xnum, P){
  # freq dis of fac
  nk <- as.vector(table(P)); 
  n <- sum(nk); 
  
  # mitjanes x grups
  xk <- tapply(Xnum, P, mean);
  
  # valors test
  txk <- (xk - mean(Xnum))/(sd(Xnum)*sqrt((n - nk)/(n*nk))); 
  
  # p-values
  pxk <- pt(txk, n - 1, lower.tail = F);
  for(c in 1:length(levels(as.factor(P)))){
    if (pxk[c] > 0.5){
      pxk[c] <- 1 - pxk[c]
    }
  }
  return (pxk)
}

# ------------------------------------------------------------------------------
ValorTestXquali <- function(P, Xquali){
  taula <- table(P, Xquali);
  n <- sum(taula); 
  pk <- apply(taula, 1, sum)/n;
  pj <- apply(taula, 2, sum)/n;
  pf <- taula/(n*pk);
  pjm <- matrix(data = pj, nrow = dim(pf)[1], ncol = dim(pf)[2], byrow = TRUE);      
  dpf <- pf - pjm; 
  dvt <- sqrt(((1 - pk)/(n*pk)) %*% t(pj*(1 - pj))); 
  
  #i hi ha divisions iguals a 0 dona NA i no funciona
  zkj <- dpf
  zkj[dpf != 0] <- dpf[dpf != 0]/dvt[dpf != 0]; 
  pzkj <- pnorm(zkj, lower.tail = F);
  
  for(c in 1:length(levels(as.factor(P)))){
    for (s in 1:length(levels(Xquali))){
      if (pzkj[c, s] > 0.5){
        pzkj[c,s] <- 1 - pzkj[c, s]
      }
    }
  }
  return (list(rowpf = pf, vtest = zkj, pval = pzkj))
}
