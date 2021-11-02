genResultsDEF1bef100_new_frailty <- function(k, nm, bef, d.ev, d.cens, b0.ev, b0.cens, a.ev, a.cens, m, ft, old)
{ 
  set.seed(k)
  mostra <- rec.ev.sim(n=nm, foltime=ft, 
                       dist.ev=d.ev, anc.ev=a.ev, beta0.ev=b0.ev, 
                       dist.cens=d.cens, anc.cens=a.cens, beta0.cens=b0.cens, 
                       beta=list(c(-0.25,-0.25,-0.25),c(-0.5,-0.5,-0.5),c(-0.75,-0.75,-0.75)), 
                       x=list(c("bern", 0.5),c("bern", 0.5),c("bern", 0.5)),
                       priskb=bef, max.old=old)
  mostra_accum <- accum(mostra)
  mod <- try(glm.cmp(formula.lambda=obs.ep.accum~x+x.1+x.2+offset(log(time.accum)), data=mostra_accum))
  r <- 1
  while(class(mod)=="try-error" | class(try(chol(vcov(mod)), silent=TRUE))=="try-error")
  {
    set.seed(100+k*r)
    mostra <- rec.ev.sim(n=nm, foltime=ft, 
                         dist.ev=d.ev, anc.ev=a.ev, beta0.ev=b0.ev, 
                         dist.cens=d.cens, anc.cens=a.cens, beta0.cens=b0.cens, 
                         beta=list(c(-0.25,-0.25,-0.25),c(-0.5,-0.5,-0.5),c(-0.75,-0.75,-0.75)), 
                         x=list(c("bern", 0.5),c("bern", 0.5),c("bern", 0.5)),
                         priskb=bef, max.old=old)
    mostra_accum <- accum(mostra)
    mod <- try(glm.cmp(formula.lambda=obs.ep.accum~x+x.1+x.2+offset(log(time.accum)), data=mostra_accum))
    r <- r + 1
  }
  
  complete.eprev <- function(data, id, var){
    Daux <- subset(data, nid==id)
    n <- dim(Daux)[1]
    eval(parse(text=paste0("aux <- seq_len(n)+(", "Daux[1,", var, "]-1)")))
    return(aux)
  }
  mostra$real.episode <- mostra$real.episode - 1 ### Para estratificar por los episodios previos (no el número de episoodio real)
  # Guardo la media de episodios previos real
  real_ep_previos <- mean(mostra$real.episode, na.rm=T)
  #mostra$real.episode[mostra$real.episode>2] <- 2
  mostra$old[is.na(mostra$old)] <- 0
  mostra$old2 <- -mostra$old   
 
  modAG<-coxph(Surv(start2,stop2,status)~as.factor(x)+as.factor(x.1)+as.factor(x.2)+old2+
                 strata(as.factor(risk.bef))+frailty(nid) ,data=mostra)
  
  gcoefAG.x <- coef(modAG)[1]; gcoefAG.x1 <- coef(modAG)[2]; gcoefAG.x2 <- coef(modAG)[3]  
  gsdAG.x <- sqrt(diag(vcov(modAG)))[1]; gsdAG.x1 <- sqrt(diag(vcov(modAG)))[2]; gsdAG.x2 <- sqrt(diag(vcov(modAG)))[3]
  
  ### IMPUTACIÓN COMPOISSON
  beta <- unlist(coef(mod))
  rv <- t(chol(vcov(mod)))
  b.star <- beta + rv %*% rnorm(ncol(rv))
  if (b.star[5] < 0.05) b.star[5] <- 0.05
  if (b.star[5] > 500) b.star[5] <- 500
  lambda <- ifelse(mostra$risk.bef==TRUE, exp(b.star[1]+b.star[2]*mostra$x+b.star[3]*mostra$x.1+
                                                b.star[4]*mostra$x.2)*(-1)*mostra$old, 0)
  for (i in 1:m)
  {
    for (j in 1:nrow(mostra))
    {
      if(mostra$risk.bef[j]==TRUE)
      {
        eval(parse(text=paste0("mostra$EprevCOMPoiss",i,"[",j,"] <- rcom(1, lambda[",j,"], nu=exp(b.star[5]))")))
      }else{
        eval(parse(text=paste0("mostra$EprevCOMPoiss",i,"[",j,"] <- 0")))
      }
    }
  }
  
  for (i in 1:m)
  {
    col  <- which(colnames(mostra)==paste0("EprevCOMPoiss", i))
    eval(parse(text=paste0("k <- lapply(unique(mostra$nid), 
                           function(k) complete.eprev(mostra, k, ", col, "))")))
    eval(parse(text=paste0("mostra$EprevCOMPoissDef", i, "<- unlist(k)"))) 
  }
  comP_ep_previos <- mean(c(mostra$EprevCOMPoissDef1, mostra$EprevCOMPoissDef2, mostra$EprevCOMPoissDef3, mostra$EprevCOMPoissDef4, mostra$EprevCOMPoissDef5), na.rm=T)
  
  # Modelos estratificando por los episodios previos imputados, usando todos los individuos
  coefs.x   <- vector()
  var.x     <- vector()
  coefs.x1  <- vector()
  var.x1    <- vector()
  coefs.x2  <- vector()
  var.x2    <- vector()
  coefsB.x  <- vector()
  varB.x    <- vector()
  coefsB.x1 <- vector()
  varB.x1   <- vector()
  coefsB.x2 <- vector()
  varB.x2   <- vector()
  for (i in 1:m)
  {
    #eval(parse(text=paste0("mostra$EprevCOMPoissDef", i, "[mostra$EprevCOMPoissDef", i, ">2] <- 2"))) 
    eval(parse(text=paste0("modPWP <- coxph(Surv(start2,stop2,status)~as.factor(x)+as.factor(x.1)+as.factor(x.2)+old2+
                    strata(as.factor(100000*risk.bef+EprevCOMPoissDef", i, "))+frailty(nid), data=mostra)")))
    coefs.x[i]  <- coef(modPWP)[1]
    var.x[i]    <- diag(vcov(modPWP))[1]
    coefs.x1[i] <- coef(modPWP)[2]
    var.x1[i]   <- diag(vcov(modPWP))[2]
    coefs.x2[i] <- coef(modPWP)[3]
    var.x2[i]   <- diag(vcov(modPWP))[3]
    #eval(parse(text=paste0("mostra$EprevCOMPoissDef", i, "[mostra$EprevCOMPoissDef", i, ">2] <- 2"))) 
    eval(parse(text=paste0("modPWP <- coxph(Surv(stop2-start2,status)~as.factor(x)+as.factor(x.1)+as.factor(x.2)+old2+
                    strata(as.factor(100000*risk.bef+EprevCOMPoissDef", i, "))+frailty(nid), data=mostra)")))
    coefsB.x[i]  <- coef(modPWP)[1]
    varB.x[i]    <- diag(vcov(modPWP))[1]
    coefsB.x1[i] <- coef(modPWP)[2]
    varB.x1[i]   <- diag(vcov(modPWP))[2]
    coefsB.x2[i] <- coef(modPWP)[3]
    varB.x2[i]   <- diag(vcov(modPWP))[3]
  }
  ### Resultados COMPOISSON
  gcoefCOMPois.x <- mean(coefs.x); gcoefCOMPois.x1 <- mean(coefs.x1); gcoefCOMPois.x2 <- mean(coefs.x2)
  wvar.x  <- (1/m)*sum(var.x); wvar.x1  <- (1/m)*sum(var.x1); wvar.x2  <- (1/m)*sum(var.x2)
  bvar.x  <- (1/(m-1))*sum((coefs.x-gcoefCOMPois.x)^2); bvar.x1  <- (1/(m-1))*sum((coefs.x1-gcoefCOMPois.x1)^2); bvar.x2  <- (1/(m-1))*sum((coefs.x2-gcoefCOMPois.x2)^2) 
  gsdCOMPois.x   <- sqrt(wvar.x+(1+1/m)*bvar.x); gsdCOMPois.x1 <- sqrt(wvar.x1+(1+1/m)*bvar.x1); gsdCOMPois.x2 <- sqrt(wvar.x2+(1+1/m)*bvar.x2)

  gcoefCOMPoisB.x <- mean(coefsB.x); gcoefCOMPoisB.x1 <- mean(coefsB.x1); gcoefCOMPoisB.x2 <- mean(coefsB.x2)
  wvarB.x  <- (1/m)*sum(varB.x); wvarB.x1  <- (1/m)*sum(varB.x1); wvarB.x2  <- (1/m)*sum(varB.x2)
  bvarB.x  <- (1/(m-1))*sum((coefsB.x-gcoefCOMPoisB.x)^2); bvarB.x1  <- (1/(m-1))*sum((coefsB.x1-gcoefCOMPoisB.x1)^2); bvarB.x2  <- (1/(m-1))*sum((coefsB.x2-gcoefCOMPoisB.x2)^2) 
  gsdCOMPoisB.x   <- sqrt(wvarB.x+(1+1/m)*bvarB.x); gsdCOMPoisB.x1 <- sqrt(wvarB.x1+(1+1/m)*bvarB.x1); gsdCOMPoisB.x2 <- sqrt(wvarB.x2+(1+1/m)*bvarB.x2)
  
  res <- data.frame(gcoefAG.x, gcoefAG.x1, gcoefAG.x2, gsdAG.x, gsdAG.x1, gsdAG.x2,
                    gcoefCOMPois.x, gcoefCOMPois.x1, gcoefCOMPois.x2, gsdCOMPois.x, gsdCOMPois.x1, gsdCOMPois.x2,
                    gcoefCOMPoisB.x, gcoefCOMPoisB.x1, gcoefCOMPoisB.x2, gsdCOMPoisB.x, gsdCOMPoisB.x1, gsdCOMPoisB.x2,
                    real_ep_previos, comP_ep_previos)
  return(res)
}