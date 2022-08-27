library(ggplot2)
library(scales)
library(lubridate)

function_simple_multiple = function(CF, period, period_to, i, m, plot = TRUE){
  
  freq = ifelse(m == 12,yes = "mensuellement",
                no = ifelse(m == 4,yes = "trimestriellement",
                            no = ifelse(m == 2,yes = "semi-annuellement",no = "annuellement")))
  
  res = CF*(1+i/m)^((period_to - period)*m)
  res = round(res,2)
  res_total = sum(res)
  if(plot){
    plot_simple_multiple_timeline(CF, period, period_to, color = 1:length(period),freq = freq)
  }
  
  if(sign((period-period_to)[which.max(abs(period-period_to))]) <0){
   x_res = 1
  } else  {
    x_res = max(c(period,period_to))-3
  } 
  
  for(l in 1:length(period)){

    text(
      x_res, y=1.4-0.2*l,col = l,
      bquote(.(CF[l])  %*% (1+frac(.(i),.(m)))^(.(period_to - period[l])%*%.(m)) == .(res[l])), cex=1)
  }


  text(
    x_res, y=0.2,col = "black",
    paste0("Réponse = ",bquote(.(res_total))), cex=1)

  return(res_total)
}


plot_simple_multiple_timeline = function(CF, period, period_to, color = "red",freq){
  rangeYM <- c(0,max(period,period_to))
  plot(NA,ylim=c(-1,1),xlim=rangeYM,ann=FALSE,axes=FALSE)
  labels  <- c("P", "S")
  labels = c(CF, "target")
 
  positions <- c(period, period_to)
  
  directions <- c(1, -1)
  
  abline(h=0,lwd=2,col="#5B7FA3")
  ypts <- rep_len(c(rep(-0.2,length(period)),0.2), length.out=length(period)+1)
  txtpts <- rep_len(c(rep(1,length(period)),3), length.out=length(period)+1)
  segments(positions,0,positions,ypts,col="gray80")
  
  axis(
    1,
    at=c(0:max(positions)),
    cex.axis=0.6,
    pos=0,
    lwd=0,
    lwd.tick=2,
    col="#5B7FA3",
    font=2
  )
  par(xpd=NA)
  
  text(
    positions, y=ypts,
    labels=labels, cex=1, pos=txtpts
  )
  
  text(
    max(positions)/2, y=1,
    labels=paste0("Période = ",freq), cex=1, pos=txtpts
  )
  
  
  iArrows <- igraph:::igraph.Arrows
  
  for(i in 1:length(period)){
    iArrows(positions[i], -abs(ypts[i]-0.4), positions[length(positions)], -abs(ypts[length(positions)])-0.3,
            h.lwd=2, sh.lwd=2, sh.col=color[i],
            curve=sign(period[i]-period_to)*0.1 , width=1, size=0.7)
  }

  
  
}



Generate_multiple_question = function(n.obs = NULL, max.obs = 5){
  if(is.null(n.obs)){
    n.obs = sample(1:max.obs,size = 1)
  }
  CF = round(rnorm(n.obs,100,100),2)
  period = sample(0:30,size = n.obs,replace = FALSE)
  CF = CF[order(period,decreasing = FALSE)]
  period = period[order(period,decreasing = FALSE)]
  period_to =  sample(0:30,size = 1,replace = FALSE)
  i = round(runif(1,0.01,0.15),5)
  m = sample(c(1,4,2,12),1)
  action = ifelse(CF > 0,yes = "déposer",no = "retirer")
  freq = ifelse(m == 12,yes = "mensuellement",
                no = ifelse(m == 4,yes = "trimestriellement",
                            no = ifelse(m == 2,yes = "semi-annuellement", no = "annuellement")))

  if(length(CF) == 1){
    Q = paste0("Vous avez l'intention de ",
               paste0(action[1:(length(CF)-1)],
                      " ", CF[1:(length(CF)-1)],
                      " dollars ", " dans ",12/m*period[1:(length(CF)-1)]," mois ","",collapse = ", "),
               " dans (de) votre compte de banque. Quel est la valeur de ce flux monétaires au mois ", 12/m*period_to,"  si votre compte de banque vous offre un taux d'intérêt de ", i*100,"%" , " capitalisé ",freq,".")
    
    
  } else {
    Q = paste0("Vous avez l'intention de ",
               paste0(action[1:(length(CF)-1)],
                      " ", CF[1:(length(CF)-1)],
                      " dollars ", " dans ",12/m*period[1:(length(CF)-1)]," mois ","",collapse = ", ")," et ",
               action[(length(CF))],
               " ", CF[length(CF)],
               " dollars "," dans ",12/m*period[(length(CF))],
               " mois dans (de) votre compte de banque. Quel est la valeur de ces flux monétaires au mois ", 12/m*period_to,"  si votre compte de banque vous offre un taux d'intérêt de ", i*100,"%" , " capitalisé ",freq,".")
    
    
  }
  
  R = function_simple_multiple(CF = CF,
                           period = period,
                           period_to =  period_to,
                           i = i,
                           m = m,
                           plot = TRUE)
  
  return(list(question = Q, response = R))
}



