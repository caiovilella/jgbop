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
  omega_w ~ 0.4,
  omega_f ~ 0.3,
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
  Phi1 ~ 0.002,
  Phi4 ~ 0.003,
  varepsilon1 ~ 0.5, 
  varepsilon2 ~ 0.1,
  wg ~ 0.6,
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
  Fs ~ fs * P * K / e , #Alterei esta equação
  Rb ~ Bcb , 
  D ~ Bb + e * Fs + Lb + Rb , # assumes Vb = 0
  rho ~ Bb / D,
  Vh ~ D,
  Vf ~ P * K - Lb, 
  Vb ~ Bb + e * Fs + Lb + Rb - D  ,
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
  Savcb ~ Bcb * pi.fac - Rb * pi.fac ,
  Savb ~ -D * pi.fac + Lb * pi.fac + Bb * pi.fac + Rb * pi.fac , # Savb is not zero and should not be. Counterfactually, it is negative in the model because of inflation-exchange rate nexus
  CheckSAV ~ Savh + Savf + Savg - CA + Savb + Savcb,
  # Por conta da inflacao internacional, as variaveis nao vao crescer a mesma taxa no steady state.
  
  # Now, get values for non-financial variables
  Y ~ u * K / v,
  G ~ g * Y, # let us assume no WBjg at the baseline scenario (to introduce the policy thereafter). Furthermore, with a nill interest on Rcb, Picb is 0. Then
  
  
  Picb ~ i * ( Bcb - Rb ) / (1 + pi) , #according to table 2 it should be Rb insted of Rcb.
  Ta ~ Savg + P * G + i * (B / (1+pi)) - Picb, # this equation closes the government's constraint
  I ~ delta * K,
  Piuf ~ Savf + P * I, # this equation closes the firm's sector capital account
  X ~ x * Y,
  M ~ m * Y,
  
  Ps ~ ( P * X + i * Bs / ( 1 + pi ) - is * e * Fs - CA  ) / ( e * M ), # This equation closes the RW sector
  
 
  W ~ ( 1 / alpha ) * ( P / ( 1 + tau ) - beta * e * Ps ),
  alpha ~ Nf / Y,
  Nf ~ (1 - unemp) * N,
  omega ~ W / P,
  wage_share ~ omega * alpha, #WS = W*N / P*Y =(W/P) * (N/Y) = omega * alpha
  beta ~ m,
  C ~ Y - X - G - I,
  Pi ~ P * ( C + I + G + X ) - Ps * e * M - i * Lb / ( 1 + pi ) - W * Nf, # This equation closes Firm's current sector
  Pidf ~ Pi - Piuf,
  SavhCheck ~ W * Nf + Pidf + Pidb - Ta + i * D / ( 1 + pi ) - P * C - Savh,
  Pib ~ i * ( Lb + Bb + Rb - D ) / ( 1 + pi ) + is * e * Fs , #this equation closes the banks' sector
  Piub ~ Savb,
  Pidb ~ Pib - Piub, 
  YD ~ (1 - psi) * Y,
  Yk ~ K/(1+pi) / v,
  
  #Distributive conflict
  sigma2 ~ (-sigma1 * ((1 - psi ) * (W * Nf + i * (D / (1+pi))) +(Pidf + Pidb)) + P * C) / (Vh / (1+pi))  , 
  #   #crescimento
  er ~ e * Ps / P,
  mu1 ~( ( W * pi.fac)/ (W / (1+pi)) )* ( 1 / (omega_w + mu2 * er - omega)),
  varphi1 ~ ( ( P * pi.fac) / ( P / (1+pi) ) ) * ( 1 / (omega -( omega_f - varphi2 * er))),
  
  #Foreign sector
  b_imp ~ M / (Y^zeta_m1 * (e * Ps / P)^zeta_m0),
  a_exp ~ X / (Ys^zeta_x1 * (P / e * Ps)^zeta_x0),
  Ys ~ Y / share_world_GDP,
  Phi3 ~ (Bs * pi.fac - Phi4 * (e * pi.fac) * (B / (1+pi))) / (Ps * Ys * ((1 + i)/ ((1+is) * (1+phi_s) * (eet / e) ))^phi_row),
   # O ideal seria ee_t+1 = ee_t + varepsilon ( e_t-1 - ee_t-1)
  ee ~ eet / (1+pi),
  eet ~ ee +  varepsilon1 * ( e / (1+pi) - ee / (1+pi)),
  Phi2 ~ (Phi1 * D * ( ( 1 + i) / ((1 + is) * phi_d * (eet / e) )) ) / ((e * pi.fac) * Fs),
  
  eq ~ (i * Bs / (1+pi) + P * X) / (is * Fs + Ps * M ),
  
  e ~ Fs / Bs + varepsilon2 * eq
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
a <- calib.output %>% mutate( teste = Vh + Vb + Vf - B + Vcb + Vs - P * K )
a$teste
#teste dos bancos 
a <- a %>% mutate( teste2 = Bb + Rb - D + Lb + e * Fs ) 
a$teste2
a <- a %>% mutate( testeB2 = Bb * pi/(1+pi) + Rb* pi/(1+pi) - D* pi/(1+pi) + Lb* pi/(1+pi) + e * pi.fac * Fs - Savb)
a$testeB2
#teste do setor do governo
a <- a %>% mutate( teste3 = Savg + pi*  B / (1 + pi))
a$teste3
a <- a %>% mutate( testeG2 = Savg + P * G - Ta - Picb + i * (B / (1 + pi)))
a$testeG2
#teste do setor das familias
a <- a %>% mutate( teste4 =  W * Nf + Pidf + Pidb + i * (D / (1 + pi ) ) - P * C - Ta - Savh)
a$teste4
#teste do setor corrente das firmas
a <- a %>% mutate( teste5 = P * (C + I + G + X) - Ps * e * M - W * Nf - Pi - i * (Lb / (1+pi)) ) 
a$teste5
a <- a %>% mutate(testeF2 = Savf + Lb * (pi / (1 + pi)))
a$testeF2
#Teste do Setor externo
a <- a %>% mutate( teste6 = CA  + Bs * (pi / (1+pi)) - e * pi.fac * (Fs +  Rcb) )
a$teste6
a <- a %>% mutate( testeRW2 = CA - P * X + e * Ps * M - is * e * Fs + i * (Bs / (1+pi)) - e * pi.fac * (Rcb + Fs) )
a$testeRW2
#Teste do BC
a <- a %>% mutate(testeBC = Picb - i * (Bcb / (1+pi)) + i * (Rb / (1+pi)) - e * pi.fac * Rcb)
a$testeBC
a <- a %>% mutate(testeBC2 = Savcb - pi.fac * e * Rcb + Rb * pi.fac - Bcb * pi.fac  )
a$testeBC2





####################################################
###       Modelo Principal cambio Flutuante       ###
#####################################################


# Crica modelo 
jgbop.model <- sfcr_set(
  #SFC Household
  Vh ~ D,
  D ~ D[-1] + Savh,
  Savh ~ W * Nf + WBjg + Pid + i * D[-1] - P * C - Ta,
  #SFC Firms
  Pif ~ P * (C + I + G +  X) - e * Ps * M - W * Nf - i * Lb[-1] , #Corrente
  Pid ~ (1 - sf) * Pif + Pidb  ,
  Piu ~ sf * Pif ,
  Savf ~ Piu - P * I,
  Lf ~ Lf[-1] - Savf,
  Vf ~ P * K - Lf,
  #SFC Banks
  Rb ~ D - Bb - Lb - e * Fs,
  Vb ~ (e - e[-1]) * Fs[-1], #Ganhos de Capital pelos ativos externos
  Lb ~ Lf,
  Bb ~ rho * D,
  Pib ~ i *( Lb[-1] + Bb[-1] + Rb[-1] - D[-1] ) + is * e * Fs[-1],
  Savb ~  -(D - D[-1])  + (Lb - Lb[-1]) + (Bb - Bb[-1]) + (Rb - Rb[-1]),
  Pidb ~ Pib - Savb,
  #SFC Gov
  Savg ~ Ta - P * G - WBjg - i * B[-1] + Picb,
  B ~ B[-1] - Savg,
  #SFC CB
  Picb ~ i * Bcb[-1] - i * Rb[-1],
  Bcb ~ B - Bb - Bs,
  Vcb ~ Bcb - Rb + e * Rcb + (e - e[-1]) * Rcb[-1], #Ganho de capital com as reservas estrangeiras
  
  #SFC RW
  CA~ P * X - e * Ps * M + is * e * Fs[-1] - i * Bs[-1] ,
  Rcb ~ Rcb[-1] + (CA + (Bs - Bs[-1])) / e - (Fs - Fs[-1]),
  Vs ~ Bs - e * Fs - e * Rcb - (e - e[-1]) * (Fs[-1] + Rcb[-1]), #Ganho (perda) de capital com reservas e ativos estrangeiros
  
  #Equacoes comportamentais
  Ta ~ psi * (W * Nf + Pid + i * D[-1]),
  #inflacao
  W ~ W[-1] + (mu1 * (omega_w0 + mu2 * er - omega) )* W[-1] ,
  P ~ P[-1] + (varphi1 * (omega - omega_f0 - varphi2 * er) )* P[-1] ,
  Ps ~ (1 + pis) * Ps[-1] ,
  omega ~ W / P,
  
  #mercado de trabalho
  Nf ~ min(N , Y * alpha) ,
  WBjg ~ wg * W * (N - Nf) ,
  YD ~ (1 - psi ) * (W * Nf + Pid + i * D[-1]) + WBjg ,
  C ~ (sigma1 * ((1 - psi ) * (W * Nf + i * D[-1]) +Pid) + sigma2 * Vh[-1] + sigma3 * WBjg) / P , 
  #crescimento
  u ~ Y / Yk,
  Yk ~ K[-1] / v ,
  I ~ delta * K[-1] ,
  K ~ K[-1] + I - delta * K[-1],
  #Produto
  Y ~ min( C + I + G + X - M, Yk ), #min (Nf / alpha , min (C + I + G + X - M, Yk)),
  G ~ min(g * Y, Yk - C - I - X + M),
  
  #Setor Externo
  M ~ b_imp * ( e * Ps / P )^zeta_m0 * (Y^zeta_m1),
  X ~ a_exp * (P / (e * Ps))^zeta_x0 * (Ys^zeta_x1),
  
  # Oferta de moeda internacional
  Bs ~ Bs[-1] + Ps * Ys * Phi3 * ( ( 1 + i) / ((1 + is) * ( 1 + phi_s) * (eet / e)) )^phi_row + Phi4 * (e - e[-1])*Bs[-1],
  
  #Demanda de moeda internacional
  Fs ~  Fs[-1] + D * Phi1 * (((1 + is) * phi_d * (eet/ e)) / (1 + i) ) - Phi2 * (e - e[-1]) * Fs[-1],
  
  #Cambio
  e2 ~ e[-1], # O ideal seria ee_t+1 = ee_t + varepsilon ( e_t-1 - ee_t-1)
  ee ~ eet[-1],
  eet ~ ee +  varepsilon1 * ( e2[-1] - ee[-1]), #eet = ee tomorow
  #O problema de eet, é que o segundo termo não tende para zero no longo prazo.
  eq ~ (i * Bs[-1] - P * X) / (is * Fs[-1] - Ps * M),
  # e ~ Fs / Bs + varepsilon2 * eq,
  er ~ (e * Ps) / P,
  
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
  psi ~ calib.output$psi,
  omega_w0 ~ calib.output$omega_w,
  omega_f0 ~ calib.output$omega_f,
  mu1 ~ calib.output$mu1,
  mu2 ~ calib.output$mu2,
  varphi1 ~ calib.output$varphi1,
  varphi2 ~ calib.output$varphi2,
  alpha ~ calib.output$alpha,
  wg ~ calib.output$wg,
  sigma1 ~ calib.output$sigma1,
  sigma2 ~ calib.output$sigma2,
  sigma3 ~ calib.output$sigma3,
  v ~ calib.output$v,
  delta ~ calib.output$delta,
  g ~ calib.output$g,
  b_imp ~ calib.output$b_imp,
  a_exp ~ calib.output$a_exp,
  zeta_m0 ~ calib.output$zeta_m0,
  zeta_x0 ~ calib.output$zeta_x0,
  zeta_m1 ~ calib.output$zeta_m1,
  zeta_x1 ~ calib.output$zeta_x1,
  Ys ~ calib.output$Ys,
  pis ~ calib.output$pis,
  Phi1 ~ calib.output$Phi1,
  Phi2 ~ calib.output$Phi2,
  Phi3 ~ calib.output$Phi3,
  Phi4 ~ calib.output$Phi4,
  phi_s ~ calib.output$phi_s,
  phi_d ~ calib.output$phi_d,
  phi_row ~ calib.output$phi_row,
  varepsilon1 ~ calib.output$varepsilon1,
  varepsilon2 ~ calib.output$varepsilon2,
  N ~ calib.output$N,
  e ~ calib.output$e
)

jgbop.init <- sfcr_set(
  D ~ calib.output$D,
  Lb ~ calib.output$Lb,
  Lf ~ calib.output$Lb,
  # e ~ calib.output$e,
  Fs ~ calib.output$Fs,
  Bb ~ calib.output$Bb,
  Rb ~ calib.output$Rb,
  B ~ calib.output$B,
  Bcb ~ calib.output$Bcb,
  Rcb ~ calib.output$Rcb,
  Bs ~ calib.output$Bs,
  W ~ calib.output$W,
  P ~ calib.output$P,
  Vh ~ calib.output$Vh,
  K ~ calib.output$K,
  e2 ~ calib.output$e2,
  ee ~ calib.output$ee,
  eet ~ calib.output$eet
)

jgbop.base <- sfcr_baseline(
  equations = jgbop.model,
  external = jgbop.pars,
  initial = jgbop.init,
  method = "Gauss",
  periods = 500 
)

JGBop.float.baseline <- JGBop.float.baseline[-1,]
