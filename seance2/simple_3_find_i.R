
function_solve_i = function(CF, CF2, period, period_to, m, plot = TRUE){
  
 
  res = ((CF2/CF)^(1/((period_to - period)))-1)*m
  
  if(plot){
    plot_solve_i(CF,CF2, period, period_to)
  }
  positions = c(period_to, period)
text(
  round(max(positions)/2), y=1,col = "black",
  bquote(S == P%*%(1+frac(i,m))^(n%*%m)), cex=1)


text(
  round(max(positions)/2), y=0.75, col = "black",
  bquote(((frac(S,P))^(frac(1,n%*%m))-1)%*%m == i), cex=1)

text(
  round(max(positions)/2), y=0.50, col = "black",
  bquote(((frac(.(CF2),.(CF)))^(frac(1,.(period_to - period)))-1)%*%.(m) == .(res)), cex=1)


  return(res)
}


plot_solve_i = function(CF, CF2, period, period_to){
  rangeYM <- c(0,max(period,period_to))
  plot(NA,ylim=c(-1,1),xlim=rangeYM,ann=FALSE,axes=FALSE)
  labels  <- c("P", "S")
  labels = c(CF, CF2)
  dir = -0.2
  
  positions <- c(period, period_to)
  
  directions <- c(1, -1)
  
  abline(h=0,lwd=2,col="#5B7FA3")
  ypts <- rep_len(c(-0.5,0.5), length.out=2)
  txtpts <- rep_len(c(1,3), length.out=2)
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
  
  iArrows <- igraph:::igraph.Arrows
  
  iArrows(positions[1], -abs(ypts[1]), positions[2], -abs(ypts[2]),
          h.lwd=2, sh.lwd=2, sh.col="red",
          curve=dir , width=1, size=0.7)
  
  
}

Generate_i_question = function(){
  n.obs = sample(1,size = 1)
  CF = round(rnorm(n.obs,10000,100),2)
  CF2 = round(rnorm(n.obs,10000,1000),2)
  period = sample(0,size = 1,replace = FALSE)
  period_to =  sample(1:30,size = n.obs,replace = FALSE)
  m = sample(c(1,4,2,12),1)
  
  freq = ifelse(m == 12,yes = "mensuellement",
                no = ifelse(m == 4,yes = "trimestriellement",
                            no = ifelse(m == 2,yes = "semi-annuellement",no = "annuellement")))
  freq2 = ifelse(m == 12,yes = "mois",
                no = ifelse(m == 4,yes = "trimestre",
                            no = ifelse(m == 2,yes = "semi-année",no = "ans")))
  
 
  print(paste0("Si vous avez investie ",CF ,"$ dans votre compte de banque il y a ",period_to," ", freq2, " et que vous avez ", CF2, "$ dans votre compte de banque aujourd'hui, quel le taux de rendement nominal capitalisé ", freq, " de votre investissement."))
 
  function_solve_i(CF = CF,
                   CF2 = CF2,
                           period = period,
                           period_to =  period_to,
                           m = m,
                           plot = TRUE)
}

Generate_i_question()
