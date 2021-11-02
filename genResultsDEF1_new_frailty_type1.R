genResultsDEF1_new_frailty <- function(k, nm, bef, d.ev, d.cens, b0.ev, b0.cens, a.ev, a.cens, m, ft, old)
{ 
  set.seed(k)
  mostra <- rec.ev.sim(n=nm, foltime=ft, 
                       dist.ev=d.ev, anc.ev=a.ev, beta0.ev=b0.ev, 
                       dist.cens=d.cens, anc.cens=a.cens, beta0.cens=b0.cens, 
                       beta=list(c(0,0,0)), 
                       x=list(c("bern", 0.5)),
                       priskb=bef, max.old=old)
  mostra_accum <- accum(mostra)
  mod <- try(glm.cmp(formula.lambda=obs.ep.accum~x+offset(log(time.accum)), data=mostra_accum), silent=TRUE)
  r <- 1
  while(class(mod)=="try-error" | class(try(chol(vcov(mod)), silent=TRUE))=="try-error")
  {
    set.seed(100+k*r)
    mostra <- rec.ev.sim(n=nm, foltime=ft, 
                         dist.ev=d.ev, anc.ev=a.ev, beta0.ev=b0.ev, 
                         dist.cens=d.cens, anc.cens=a.cens, beta0.cens=b0.cens, 
                         beta=list(c(0,0,0)), 
                         x=list(c("bern", 0.5)),
                         priskb=bef, max.old=old)
    mostra_accum <- accum(mostra)
    mod <- try(glm.cmp(formula.lambda=obs.ep.accum~x+offset(log(time.accum)), data=mostra_accum), silent=TRUE)
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
  # Modelo estratificando por los episodios reales
  mostra$old[is.na(mostra$old)] <- 0
  mostra$old2 <- -mostra$old  
  modAG<-coxph(Surv(start2,stop2,status)~as.factor(x)+old2+
                 strata(as.factor(risk.bef))+frailty(nid), data=mostra)
  
  gcoefAG.x <- coef(modAG)[1]; 
  gsdAG.x <- sqrt(diag(vcov(modAG)))[1]; 
  
  ### IMPUTACIÓN COMPOISSON
  beta <- unlist(coef(mod))
  rv <- t(chol(vcov(mod)))
  b.star <- beta + rv %*% rnorm(ncol(rv))
  if (b.star[3] < 0.05) b.star[3] <- 0.05
  if (b.star[3] > 500) b.star[3] <- 500
  lambda <- ifelse(mostra$risk.bef==TRUE, exp(b.star[1]+b.star[2]*mostra$x)*(-1)*mostra$old, 0)
  
  # mod2 <- glm.cmp(formula.lambda=obs.episode~x+x.1+x.2+offset(log(time)), data=mostra[mostra$risk.bef==FALSE, ])
  # beta <- unlist(coef(mod2))
  # rv <- t(chol(vcov(mod2)))
  # b.star2 <- beta + rv %*% rnorm(ncol(rv))
  # lambda1 <- ifelse(mostra$risk.bef==TRUE, exp(b.star2[1]+b.star2[2]*mostra$x+b.star2[3]*mostra$x.1+
  #                                                b.star2[4]*mostra$x.2)*(-1)*mostra$old, 0)
  for (i in 1:m)
  {
    for (j in 1:nrow(mostra))
    {
      if(mostra$risk.bef[j]==TRUE)
      {
        eval(parse(text=paste0("mostra$EprevCOMPoiss",i,"[",j,"] <- rcom(1, lambda[",j,"], nu=exp(b.star[3]))")))
  #      eval(parse(text=paste0("mostra$EprevCOMPoissB",i,"[",j,"] <- rcom(1, lambda1[",j,"], nu=exp(b.star2[5]))")))
      }else{
        eval(parse(text=paste0("mostra$EprevCOMPoiss",i,"[",j,"] <- 0")))
   #     eval(parse(text=paste0("mostra$EprevCOMPoissB",i,"[",j,"] <- 0")))
      }
    }
  }
  
  for (i in 1:m)
  {
    col  <- which(colnames(mostra)==paste0("EprevCOMPoiss", i))
    #col2 <- which(colnames(mostra)==paste0("EprevCOMPoissB", i))
    eval(parse(text=paste0("k <- lapply(unique(mostra$nid), 
                           function(k) complete.eprev(mostra, k, ", col, "))")))
    eval(parse(text=paste0("mostra$EprevCOMPoissDef", i, "<- unlist(k)"))) 
    #eval(parse(text=paste0("k <- lapply(unique(mostra$nid), 
    #                       function(k) complete.eprev(mostra, k, ", col2, "))")))
    #eval(parse(text=paste0("mostra$EprevCOMPoissBDef", i, "<- unlist(k)")))  
  }
  comP_ep_previos <- mean(c(mostra$EprevCOMPoissDef1, mostra$EprevCOMPoissDef2, mostra$EprevCOMPoissDef3, mostra$EprevCOMPoissDef4, mostra$EprevCOMPoissDef5), na.rm=T)
  #comPB_ep_previos <- mean(c(mostra$EprevCOMPoissBDef1, mostra$EprevCOMPoissBDef2, mostra$EprevCOMPoissBDef3, mostra$EprevCOMPoissBDef4, mostra$EprevCOMPoissBDef5), na.rm=T)
  
  # Modelos estratificando por los episodios previos imputados, usando todos los individuos
  coefs.x   <- vector()
  var.x     <- vector()
  coefsB.x  <- vector()
  varB.x    <- vector()
  for (i in 1:m)
  {
    #eval(parse(text=paste0("mostra$EprevCOMPoissDef", i, "[mostra$EprevCOMPoissDef", i, ">2] <- 2"))) 
    eval(parse(text=paste0("modPWP <- coxph(Surv(start2,stop2,status)~as.factor(x)+old2+
                    strata(as.factor(100000*risk.bef+EprevCOMPoissDef", i, "))+frailty(nid), data=mostra)")))
    coefs.x[i]  <- coef(modPWP)[1]
    var.x[i]    <- diag(vcov(modPWP))[1]
    #eval(parse(text=paste0("mostra$EprevCOMPoissBDef", i, "[mostra$EprevCOMPoissBDef", i, ">2] <- 2"))) 
    eval(parse(text=paste0("modPWP <- coxph(Surv(stop2-start2,status)~as.factor(x)+old2+
                    strata(as.factor(100000*risk.bef+EprevCOMPoissDef", i, "))+frailty(nid), data=mostra)")))
    coefsB.x[i]  <- coef(modPWP)[1]
    varB.x[i]    <- diag(vcov(modPWP))[1]
  }
  ### Resultados COMPOISSON
  gcoefCOMPois.x <- mean(coefs.x); 
  wvar.x  <- (1/m)*sum(var.x); 
  bvar.x  <- (1/(m-1))*sum((coefs.x-gcoefCOMPois.x)^2); 
  gsdCOMPois.x   <- sqrt(wvar.x+(1+1/m)*bvar.x); 
  
  gcoefCOMPoisB.x <- mean(coefsB.x);
  wvarB.x  <- (1/m)*sum(varB.x); 
  bvarB.x  <- (1/(m-1))*sum((coefsB.x-gcoefCOMPoisB.x)^2); 
  gsdCOMPoisB.x   <- sqrt(wvarB.x+(1+1/m)*bvarB.x);
  
  res <- data.frame(gcoefAG.x, gsdAG.x,
                    gcoefCOMPois.x, gsdCOMPois.x,
                    gcoefCOMPoisB.x, gsdCOMPoisB.x,
                    real_ep_previos, comP_ep_previos)
  return(res)
}