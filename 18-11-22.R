#install.packages("sfcr")
# install.packages("tidyverse")
# library( sfcr )
# library( tidyverse )

#################################
###    CALIBRAGEM            ####
#################################
calib.input <- sfcr_set( 
  b ~ 0.5,
  P ~ 1,
  K ~ 100,
  bcb ~ 0.05,
  rcb ~ 0.1,
  bs ~ 0.3,
  l ~ 0.2,
  fs ~ 0.1,
  pi ~ 0.05,
  u ~ 0.8,
  v ~ 1.5,
  g ~ 0.2,
  i ~ 0.03,
  delta ~ 0.1,
  is ~ 0.02,
  x ~ 0.2,
  m ~ 0.2,
  unemp ~ 0.2,
  N ~ 100,
  tau ~ 0.4,
  psi ~ 0.3,
  mu1 ~ 0.2,
  varphi1 ~ 0.35,
  mu2 ~ 0.18,
  varphi2 ~ 0.1,
  zeta_m0 ~ -0.15,
  zeta_m1 ~ 0.1,
  zeta_x0 ~ -0.15,
  zeta_x1 ~ 0.1,
  share_world_GDP ~ 0.06,
  phi_s ~ 0.03,
  phi_row ~ 0.9,
  phi_d ~ 1,
  varepsilon1 ~ 0.5, 
  varepsilon2 ~ 0.1,
  wg ~ 0,
  sigma1 ~ 0.7,
  sigma3 ~ 1,
  sf ~ 0.25,
  # e ~ 2,
  # Ps ~ 0.9
)


jgbop.calib <- sfcr_set(
  # First calibrate the balance sheet matrix
  B ~ b * P * K,
  Bcb ~ bcb * B,
  Bs ~ bs * B,
  Bb ~ B - Bs - Bcb,
  Rcb ~ rcb * P * K / e,
  Lb ~ l * P * K,
  Fs ~ fs * P * K / e , 
  Rb ~ Bcb , #Perguntar ao Italo
  D ~ Bb + e * Fs + Lb + Rb , # assumes Vb = 0
  rho ~ Bb / D,
  Vh ~ D,
  Vf ~ P * K - Lb, 
  Vb ~ Bb + e * Fs + Lb + Rb - D ,
  Vg ~ -B,
  Vcb ~ Bcb + e * Rcb - Rb  ,
  Vs ~ Bs - e * Fs - e * Rcb ,
  CheckV ~ Vh + Vb + Vg + Vcb + Vs + Vf - P * K,
  
  # Second, conditional on the BS matrix, calibrate the net borrowing/lending
  # We assume that external inflation is nil
  pi.fac ~ pi / ( 1 + pi ),
  Savh ~ D * pi.fac,
  Savf ~ -Lb * pi.fac,
  Savg ~ -B * pi.fac,
  CA ~ -Bs * pi.fac ,
  CA2 ~ -pi.fac * Vs - e * pi.fac * (Fs + Rcb),
  Savcb ~ Bcb * pi.fac - Rb * pi.fac ,
  Savcb2 ~ pi.fac * Vcb - e * pi.fac * Rcb,
  Savb ~ -D * pi.fac + Lb * pi.fac + Bb * pi.fac + Rb * pi.fac , # Savb is not zero and should not be. Counterfactually, it is negative in the model because of inflation-exchange rate nexus
  Savb2 ~ pi.fac * Vb - e * pi.fac * Fs ,
  CheckSAV ~ Savh + Savf + Savg - CA + Savb + Savcb,
  # Por conta da inflacao internacional, as variaveis nao vao crescer a mesma taxa no steady state.
  
  # Now, get values for non-financial variables
  Y ~ u * K / v,
  G ~ g * Y, # let us assume no WBjg at the baseline scenario (to introduce the policy thereafter). Furthermore, with a nill interest on Rcb, Picb is 0. Then
  WBjg ~ wg * W * (N - Nf),
  
  Picb ~ i * ( Bcb - Rb ) / (1 + pi) , #according to table 2 it should be Rb insted of Rcb.
  
  Ta ~ Savg + P * G + i * (B / (1+pi)) + WBjg - Picb, # this equation closes the government's constraint
  I ~ delta * K ,
  Piuf ~ Savf + P * I, # this equation closes the firm's sector capital account
  X ~ x * Y,
  M ~ m * Y,
  
  Ps ~ ( P * X - i * Bs / ( 1 + pi ) + is * e * Fs - CA  ) / ( e * M ), # This equation closes the RW sector
  
  
  W ~ ( 1 / alpha ) * ( P / ( 1 + tau ) - beta * e * Ps ),
  omega_w0 ~ pi / mu1 - mu2 * er + omega,
  omega_f0 ~ omega + varphi2 * er - pi / varphi1,
  
  
  alpha ~ Nf / Y,
  Nf ~ (1 - unemp) * N,
  omega ~ W / P,
  wage_share ~ omega * alpha, #WS = W*N / P*Y =(W/P) * (N/Y) = omega * alpha
  Pi_share ~ (1 - wage_share) * Y, 
  beta ~ m,
  C ~ Y - X - G - I ,
  Pi ~ P * ( C + I + G + X ) - Ps * e * M - i * Lb / ( 1 + pi ) - W * Nf, # This equation closes Firm's current sector
  Pidf ~ Pi - Piuf,
  SavhCheck ~ W * Nf + Pidf + Pidb - Ta + i * D / ( 1 + pi ) - P * C - Savh,
  Pib ~ i * ( Lb + Bb + Rb - D ) / ( 1 + pi ) + is * e * Fs , #this equation closes the banks' sector
  Lf ~ Lb,
  Piub ~ Savb,
  Pidb ~ Pib - Piub, 
  YD ~ (1 - psi) * Y,
  Yk ~ K/(1+pi) / v,
  
  #Distributive conflict
  sigma2 ~ (-sigma1 * ((1 - psi ) * (W * Nf + i * (D / (1+pi))) +(Pidf + Pidb)) + P * C - sigma3 * WBjg) / (Vh / (1+pi))  , 
  #   #crescimento
  er ~ e * Ps / P,
  
  
  #Foreign sector
  b_imp ~ M / (((Y / (1+pi))^zeta_m1) * (e * Ps / P)^zeta_m0),
  a_exp ~ X / (Ys^zeta_x1 * (P / e * Ps)^zeta_x0),
  Ys ~ Y / share_world_GDP,
  Phi2 ~ (Bs * pi.fac ) / (e * Ps * Ys * ((1 + i)/ ((1+is) * (1+phi_s) * (eet / e) ))^phi_row),
   # O ideal seria ee_t+1 = ee_t + varepsilon ( e_t-1 - ee_t-1)
  ee ~ eet / (1+pi),
  eet ~ ee +  varepsilon1 * ( e / (1+pi) - ee / (1+pi)),
  Phi1 ~ (pi.fac * Fs) / ((D / e) * (((1+is) *phi_d * (eet / e)) / (1+i) )),
  
  eq ~ (i * Bs / (1+pi) + P * X) / (is * Fs + Ps * M ),
  
  # e ~ Fs / Bs + varepsilon2 * eq
  e ~ e / (1+pi)
)


calib.output <- sfcr_baseline(
  equations = jgbop.calib,
  external = calib.input,
  periods = 2,
  method = 'Gauss'
)

calib.output <- calib.output[-1,]
options(scipen = 999)


#Secao de testes
#teste horizontal
a <- calib.output %>% mutate( teste = Vh + Vf - B + Vcb + Vs - P * K )
a$teste
#teste dos bancos 
a <- a %>% mutate( teste2 = Bb + Rb - D + Lb + e * Fs ) 
a$teste2
a <- a %>% mutate( testeB2 = Bb * pi/(1+pi) + Rb* pi/(1+pi) - D* pi/(1+pi) + Lb* pi/(1+pi) + e * Fs * pi/(1+pi) )
a$testeB2
#teste do setor do governo
a <- a %>% mutate( teste3 = Savg + pi*  B / (1 + pi))
a$teste3
a <- a %>% mutate( testeG2 = Savg + P * G - Ta - Picb + i * (B / (1 + pi)))
a$testeG2
#teste do setor das familias
a <- a %>% mutate( teste4 = W * Nf + Pidf + Pidb + i * (D / (1 + pi ) ) - P * C - Ta - Savh)
a$teste4
#teste do setor corrente das firmas
a <- a %>% mutate( teste5 = P * C + P * I + P * G + P * X - Ps * e * M - W * Nf - Pi - i * (Lb / (1+pi)) ) 
a$teste5
a <- a %>% mutate(testeF2 = Savf + Lb * (pi / (1 + pi)))
a$testeF2
#Teste do Setor externo
a <- a %>% mutate( teste6 = CA  + Bs * (pi / (1+pi))  )
a$teste6
a <- a %>% mutate( testeRW2 = CA - P * X + e * Ps * M - is * e * Fs + i * (Bs / (1+pi)))
a$testeRW2
#Teste do BC
a <- a %>% mutate(testeBC = Picb - i * (Bcb / (1+pi)) + i * (Rb / (1+pi)))
a$testeBC
a <- a %>% mutate(testeBC2 = Rb * (pi / (1+ pi)) - Bcb * ( pi / (1+pi)) )
a$testeBC2





####################################################
###       Modelo Principal cambio Flutuante       ###
#####################################################


# Crica modelo 
jgbop.model <- sfcr_set(
  #SFC Household
  Vh ~ D,
  D ~ D[-1] + Savh,
  Savh ~ W * Nf + WBjg + Pidf + Pidb + i * D[-1] - P * C - Ta,
  #SFC Firms
  Pif ~ P * (C + I + G +  X) - e * Ps * M - W * Nf - i * Lb[-1] , #Corrente
  Pidf ~ (1 - sf) * Pif ,
  Piuf ~ sf * Pif ,
  Savf ~ Piuf - P * I,
  Lf ~ Lf[-1] - Savf,
  Vf ~ P * K - Lf,
   #SFC Banks
  Rb ~ D - Bb - Lb - e * Fs,
  Vb ~ Bb + e * Fs + Lb + Rb - D,
  Lb ~ Lf,
  Bb ~ rho * D,
  Pib ~ i *( Lb[-1] + Bb[-1] + Rb[-1] - D[-1] ) + is * e * Fs[-1],
  Savb ~  -(D - D[-1])  + (Lb - Lb[-1]) + (Bb - Bb[-1]) + (Rb - Rb[-1]) + e * (Fs - Fs[-1]),
  Piub ~ Savb,
  Pidb ~ Pib - Piub,
  #SFC Gov
  Savg ~ Ta - P * G - WBjg - i * B[-1] + Picb,
  B ~ B[-1] - Savg,
  #SFC CB
  Picb ~ i * Bcb[-1] - i * Rb[-1],
  Bcb ~ B - Bb - Bs,
  Vcb ~ Bcb - Rb + e * Rcb,
  Savcb ~ Picb - i * Bcb[-1] + i * Rb[-1],

  #SFC RW
  CA ~ P * X - e * Ps * M + is * e * Fs[-1] - i * Bs[-1] ,
  
  #tentativa 1 - Roda corretamente
  Rcb ~ Rcb[-1] + (CA + (Bs - Bs[-1])) / e - (Fs - Fs[-1]),
  
  #Tentativa 2
  # e ~  ((Rcb - Rcb[-1]) - Ps * Ys * Phi2 * ( ( 1 + i) / ((1 + is) * ( 1 + phi_s) * (eet[-1] / e[-1])) )^phi_row + Ps * M - is * Fs[-1]) / (P * X - i * Bs[-1] + Phi1 * D * (((1 + is) * phi_d * (eet[-1]/ e[-1])) / (1 + i) )),
  
  #Tentativa 3
  # fec ~  ( - Ps * Ys * Phi2 * ( ( 1 + i) / ((1 + is) * ( 1 + phi_s) * (eet[-1] / e[-1])) )^phi_row + 
  #         Ps * M - is * Fs[-1]) / (P * X - i * Bs[-1] + Phi1 * D * (((1 + is) * phi_d * (eet[-1]/ e[-1])) / (1 + i) )),
  # Rcb ~ Rcb[-1] + 0.2 * fec,
  # e ~ 0.8 * fec,
  Vs ~ Bs - e * Fs - e * Rcb ,
   
  #Equacoes comportamentais
  Ta ~ psi * (W * Nf + Pidf + Pidb + i * D[-1]),
  #inflacao
  W ~ W[-1] + (mu1 * (omega_w0 + mu2 * er - omega) )* W[-1] ,
  P ~ P[-1] + (varphi1 * (omega - omega_f0 + varphi2 * er) )* P[-1] ,
  omega ~ W / P,

  #mercado de trabalho
  Nf ~ min(N , Y * alpha) ,
  WBjg ~ wg * W * (N - Nf) ,
  YD ~ ((1 - psi ) * (W * Nf + Pidf + Pidb + i * D[-1]) + WBjg ) / P,
  C ~ sigma1 * (((1 - psi ) * (W * Nf + i * D[-1]) +Pidf + Pidb) / P) + sigma2 * (Vh[-1] / P) + sigma3 * (WBjg / P) ,
  #crescimento
  u ~ Y / Yk,
  Yk ~ K[-1] / v ,
  I ~ delta * K[-1] ,
  K ~ K[-1] + I - delta * K[-1],
  #Produto
  Y ~ min( C + I + G + X - M, Yk ), #min (Nf / alpha , min (C + I + G + X - M, Yk)),
  G ~ g * Y,

  #Setor Externo
  M ~ b_imp * ( e * Ps / P )^zeta_m0 * (Y[-1]^zeta_m1),
  X ~ a_exp * (P / (e * Ps))^zeta_x0 * (Ys^zeta_x1),

  # Oferta de moeda internacional
  Bs ~ Bs[-1] + e * Ps * Ys * Phi2 * ( ( 1 + i) / ((1 + is) * ( 1 + phi_s) * (eet / e)) )^phi_row,
  
  #Demanda de moeda internacional
  Fs ~  Fs[-1] + D / e * Phi1 * (((1 + is) * phi_d * (eet / e)) / (1 + i) ),
  
  #Cambio
 # O ideal seria ee_t+1 = ee_t + varepsilon ( e_t-1 - ee_t-1)
  ee ~ eet[-1],
  eet ~ ee +  varepsilon1 * ( e[-1] - ee[-1]), #eet = ee tomorow

  eq ~ (i * Bs[-1] - P * X) / (is * Fs[-1] - Ps * M),
  # e ~ e[-1] + Fs / Bs + varepsilon2 * eq ,
  er ~ (e * Ps) / P,
  e ~ e[-1] * (1 + pi),

  #Variaveis analiticas
  pi ~ (P - P[-1]) / P[-1],
  unemp ~ (N - Nf)/N,
  CAr ~ CA /P,
  CAY ~ CAr / Y
)
 
jgbop.pars <- sfcr_set(
  i ~ calib.output$i,
  sf ~ calib.output$sf,
  rho ~ calib.output$rho,
  is ~ calib.output$is,
  Ps ~ calib.output$Ps,
  psi ~ calib.output$psi,
  mu1 ~ calib.output$mu1,
  omega_w0 ~ calib.output$omega_w0,
  mu2 ~ calib.output$mu2,
  varphi1 ~ calib.output$varphi1,
  omega_f0 ~ calib.output$omega_f0,
  varphi2 ~ calib.output$varphi2,
  N ~ calib.output$N,
  alpha ~ calib.output$alpha,
  wg ~ 0,
  sigma1 ~ calib.output$sigma1,
  sigma2 ~ calib.output$sigma2,
  sigma3 ~ calib.output$sigma3,
  v ~ calib.output$v,
  delta ~ calib.output$delta,
  g ~ calib.output$g,
  b_imp ~ calib.output$b_imp,
  zeta_m0 ~ calib.output$zeta_m0,
  zeta_m1 ~ calib.output$zeta_m1,
  a_exp ~ calib.output$a_exp,
  zeta_x0 ~ calib.output$zeta_x0,
  zeta_x1 ~ calib.output$zeta_x1,
  Ys ~ calib.output$Ys,
  phi_s ~ calib.output$phi_s,
  phi_row ~ calib.output$phi_row,
  Phi1 ~ calib.output$Phi1,
  phi_d ~ calib.output$phi_d,
  Phi2 ~ calib.output$Phi2,
  varepsilon1 ~ calib.output$varepsilon1,
  varepsilon2 ~ calib.output$varepsilon2,
  # Rcb ~ calib.output$Rcb, #Fech2
  
)
 
jgbop.init <- sfcr_set(
  D ~ calib.output$D,
  Lb ~ calib.output$Lb,
  Lf ~ calib.output$Lb,
  e ~ calib.output$e,
  Fs ~ calib.output$Fs,
  Bb ~ calib.output$Bb,
  Rb ~ calib.output$Rb,
  B ~ calib.output$B,
  Bcb ~ calib.output$Bcb,
  Bs ~ calib.output$Bs,
  Rcb ~ calib.output$Rcb, #Fech2
  W ~ calib.output$W,
  P ~ calib.output$P,
  Vh ~ calib.output$Vh,
  K ~ calib.output$K,
  eet ~ calib.output$eet,
  ee ~ calib.output$ee,
  pi ~ calib.output$pi
)
 
jgbop.base <- sfcr_baseline(
  equations = jgbop.model,
  external = jgbop.pars,
  initial = jgbop.init,
  method = "Gauss",
  periods = 500
)

jgbop.base <- jgbop.base[-1,]
         
plot(jgbop.base$Y, type ='l')
#Secao de testes
#teste horizontal
b <- jgbop.base %>% mutate( teste = Vh + Vf - B + Vcb + Vs - P * K )
b$teste
#teste dos bancos 
b <- b %>% mutate( teste2 = Bb + Rb - D + Lb + e * Fs ) 
b$teste2
b <- b %>% mutate( testeB2 = -Savb + (Bb - lag(Bb)) + (Rb - lag(Rb)) - (D - lag(D)) + (Lb - lag(Lb)) + e * (Fs - lag(Fs)) )
b$testeB2
#teste do setor do governo
b <- b %>% mutate( teste3 = Savg + B - lag(B))
b$teste3
b <- b %>% mutate( testeG2 = Savg + P * G - Ta - Picb + i * lag(B))
b$testeG2
#teste do setor das familias
b <- b %>% mutate( teste4 = W * Nf + Pidf + Pidb + i * lag(D) - P * C - Ta - Savh)
b$teste4
b <- b %>% mutate( testeh2 = (D - lag(D)) - Savh)
b$testeh2
#teste do setor corrente das firmas
b <- b %>% mutate( teste5 = P * C + P * I + P * G + P * X - Ps * e * M - W * Nf - Pif - i * lag(Lb) ) 
b$teste5
b <- b %>% mutate(testeF2 = Savf + Lb - lag(Lb))
b$testeF2
#Teste do Setor externo
b <- b %>% mutate( teste6 = -CA  - (Bs - lag(Bs)) + e * (Fs - lag(Fs)) + e * (Rcb - lag(Rcb))  )
b$teste6
b <- b %>% mutate( testeRW2 = CA - P * X + e * Ps * M - is * e * lag(Fs) + i * lag(Bs))
b$testeRW2
#Teste do BC
b <- b %>% mutate(testeBC = Picb - i * lag(Bcb) + i * lag(Rb))
b$testeBC
b <- b %>% mutate(testeBC2 = (Rb - lag(Rb)) - (Bcb - lag(Bcb)) - e * (Rcb - lag(Rcb)) )
b$testeBC2

tfm_jgbop <- sfcr_matrix(
  columns = c( "Households" , "Firms_curent" , "Firms_capital", "Banks", "Government", "CB", "RW" ),
  codes = c( "h" , "fc", "fk",  "b", "g", "cb", "s" ),
  c( "Consumption", h = "-P * C", fc = "+P * C" ),
  c( "Investiment", fc = "+ P * I", fk = "- P * I"),
  c( "Govt. Exp.", fc = "+P * G", g = "-P * G" ),
  c( "Exports", fc = "+P * X", s = "-P * X"),
  c( "Imports", fc = "-Ps * e * M", s = "+Ps * e * M"),
  c( "wages", h = "+W * Nf + WBjg", g = "-WBjg", fc = "-W * Nf" ),
  c( "Profits", h = "+Pidf + Pidb", fc = "-Pidf", b = "-Pidb"),
  c( "Retained Profits", fc = "-Piuf", fk = "+Piuf"),
  c( "Taxes", h = "-T", g = "+T" ),
  c( "CB Profits", g = "+Picb", cb = "-Picb"),
  c( "Int. deposits", h = "+i * D[-1]", b = "-i * D[-1]"),
  c( "Int. loans", fc = "- i * Lb[-1]", b = "+i * Lb[-1]"),
  c( "Int. Bonds", b = "+ i * Bb[-1]", g = "-i * B[-1]", cb = "+ i * Bcb[-1]", s = "+i * Bs[-1]"),
  c( "Int. Foreign bonds", b = "+is * e * Fs[-1]", s = "-is * e * Fs[-1]"),
  c( "Bank reserves", b= "+ i * Rb[-1]", cb = "-i * Rb[-1]"),
  c( "Ch. deposits", h = "-d(D)", b = "+d(D)" ),
  c( "Ch. loans", fk = "+d(Lf)", b= "-d(Lb)"),
  c( "Ch. bonds", b = "-d(Bb)", g = "+d(B)", cb = "-d(Bcb)", s = "-d(Bs)"),
  c( "Ch. Fs", b = "- e * d(Fs)", s = "+ e * d(Fs)"),
  c( "Ch. Rb", b = "-d(Rb)", cb = "+d(Rb)"),
  c( "Ch. Rcb", cb = "-e * d(Rcb)", s = "+e * d(Rcb)" )
)
tfm_jgbop

sfcr_validate( tfm_jgbop,jgbop.base, which = "tfm" )
