install.packages("sfcr")
install.packages("tidyverse")
install.packages("ggplot")
library( sfcr )
library( tidyverse )
library( ggplot2 )
library(gridExtra)

#################################
###    CALIBRATION           ####
#################################
calib.input <- sfcr_set( 
  b ~ 0.5,
  P ~ 1,
  K ~ 100,
  bcb ~ 0.05,
  rcb ~ 0.1,
  bs ~ 0.3,
  l ~ 0.2,
  pi ~ 0.05,
  u ~ 0.8,
  v ~ 1.5,
  g ~ 0.25,
  is ~ 0.02,
  x ~ 0.2,
  beta ~ 0.2,
  unemp ~ 0.15,
  N ~ 100,
  tau_t ~ 0.9,
  mu_u ~ 2,
  mu_b ~ 0.2,
  mu1 ~ 0.4,
  mu2 ~ 0.2,
  varphi2 ~ 0.2,
  zeta_m0 ~ -0.15,
  zeta_m1 ~ 1.1,
  zeta_x0 ~ -0.15,
  zeta_x1 ~ 1.1,
  share_world_GDP ~ 0.1,
  phi_s ~ 0.03,
  phi_d ~ 1,
  wg ~ 0,
  sigma1 ~ 0.8,
  sigma3 ~ 1,
  sigma4 ~ 0.1,
  sf ~ 0.3,
  gamma ~ 0.1,
  varepsilon1 ~ 0.5121951,
  e ~ 2
)


jgbop.calib <- sfcr_set(
  # First calibrate the balance sheet matrix
  B ~ b * P * K,
  Bcb ~ bcb * B,
  Bs ~ bs * B,
  Bb ~ B - Bs - Bcb,
  Rcb ~ rcb * P * K / e,
  Lb ~ l * P * K,
  Rb ~ Bcb , 
  D ~ Bb + Lb + Rb , # assumes Vb = 0
  rho ~ Bb / D,
  Vh ~ D,
  Vf ~ P * K - Lb, 
  Vb ~ Bb + Lb + Rb - D ,
  Vg ~ -B,
  Vcb ~ Bcb + e * Rcb - Rb  ,
  Vs ~ Bs - e * Rcb ,
  CheckV ~ Vh + Vb + Vg + Vcb + Vs + Vf - P * K,
  
  # Second, conditional on the BS matrix, calibrate the net borrowing/lending
  # We assume that external inflation is nil
  pi.fac ~ pi / ( 1 + pi ),
  i ~ (1+is) * (1+pi) * (1+phi_s) - 1,
  r ~ (1 + i) / (1 + pi) - 1,
  Savh ~ D * pi.fac,
  Savf ~ -Lb * pi.fac,
  Savg ~ -B * pi.fac,
  CA ~ -Bs * pi.fac ,
  CA2 ~ -pi.fac * Vs - e * pi.fac * Rcb,
  Savcb ~ Bcb * pi.fac - Rb * pi.fac ,
  Savcb2 ~ pi.fac * Vcb - e * pi.fac * Rcb,
  Savb ~ -D * pi.fac + Lb * pi.fac + Bb * pi.fac + Rb * pi.fac , # Savb is not zero and should not be. Counterfactually, it is negative in the model because of inflation-exchange rate nexus
  Savb2 ~ pi.fac * Vb - e * pi.fac * Fs ,
  CheckSAV ~ Savh + Savf + Savg - CA + Savb + Savcb,
  
  # Now, get values for non-financial variables
  Picb ~ i * ( Bcb - Rb ) / (1 + pi) , #according to table 2 it should be Rb insted of Rcb.
  
  Ta ~ -B * pi.fac + P * G + i * (B / (1+pi)) + WBjg - Picb, # this equation closes the government's constraint
  psi ~ Ta / (W * Nf + Pidf + Pidb + i * D/(1+pi)),
  
  W ~ (Savh - Pidf - Pidb - i * (D / (1+pi)) + P * C + Ta ) / (Nf + wg *(N - Nf)),
  omega ~ W / P,
  
  omega_w0 ~ pi / mu1 - mu2 * er + omega,
  mu_a ~ mu1 + mu_u * unemp - mu_b * wg,
  varphi1 ~ pi / (omega - omega_f0 + varphi2 * er),
  tau ~ (1 - (omega * alpha + er * beta)) / (omega * alpha + er * beta),
  omega_f0 ~ (1 - (1+tau_t) *(er * beta)) / ((1+tau_t)*alpha) + varphi2 * er,
  
  C ~ Y - X - G - I + M ,
  Y ~ u * Yk,
  Yk ~ K / v,
  gi ~ gamma * (u - ut),
  ut ~ u,
  delta ~ I/K - gi,
  I ~ (Piuf - Savf)/P,# this equation closes the firm's sector capital account
  alpha ~ Nf / Y,
  Nf ~ (1 - unemp) * N,
  
  
  Pif ~ P * ( C + I + G + X ) - Ps * e * M - i * Lb / ( 1 + pi ) - W * Nf,# This equation closes Firm's current sector
  Piuf ~ Pif * sf,
  Pidf ~ Pif - Piuf,
  SavhCheck ~ W * Nf + Pidf + Pidb - Ta + i * D / ( 1 + pi ) - P * C - Savh,
  Pib ~ i * ( Lb + Bb + Rb - D ) / ( 1 + pi ) , #this equation closes the banks' sector
  Lf ~ Lb,
  Pidb ~ Pib - Piub, 
  Piub ~ Savb,
  YD ~ (1 - psi) * Y,
  
  G ~ g * Y,# let us assume no WBjg at the baseline scenario (to introduce the policy thereafter). Furthermore, with a nill interest on Rcb, Picb is 0. Then
  WBjg ~ wg * W * (N - Nf),
  sigma2 ~ (P * C - ( 1 - psi) * sigma1 * (W * Nf + i * D / (1+pi)) - sigma3 * WBjg - sigma4 * Vh / (1+pi) ) / ( (1 - psi) * (Pidb + Pidf) )  ,
  
  #Foreign sector
  X ~ x * Y,
  M ~ beta * Y,
  Ps ~ ( P * X - i * Bs / ( 1 + pi ) - CA  ) / ( e * M ), # This equation closes the RW sector
  a_exp ~ X / (Ys^zeta_x1 * (P / (e * Ps))^zeta_x0),
  b_imp ~ M / ((Y^zeta_m1) * (e * Ps / P)^zeta_m0),
  
  Ys ~ Y / share_world_GDP,
  Phi ~ Bs / (e * Ps * Ys * ((1 + i)/ ((1+is) * (1+phi_s) * (eet / e) ))),
  # phi_s ~ phi_s1 - phi_s2 * 0 ,
  
  # O ideal seria ee_t+1 = ee_t + varepsilon ( e_t-1 - ee_t-1)
  ee ~ eet / (1+pi),
  eet ~ e * (1+pi),
  # varepsilon1 ~ (eet - ee) / ( e / (1+pi) - ee / (1+pi)),
  
  er ~ e * Ps / P,
  
  epsilon ~ e / Bs,
  wage_share ~ omega * alpha, #WS = W*N / P*Y =(W/P) * (N/Y) = omega * alpha
  Pi_share ~ (Pif) / (P * Y), 
)


calib.output <- sfcr_baseline(
  equations = jgbop.calib,
  external = calib.input,
  periods = 2,
  method = 'Gauss'
)

calib.output <- calib.output[-1,]
options(scipen = 999)






############################################################
###       Main Model - Floating exchange rate regime     ###
############################################################

# Model's  equations
jgbop.model<- sfcr_set(
  #Households
  Vh ~ D,
  #Firms
  Vf ~ P * K - Lf,
  #Banks
  Rb ~ D - Bb - e * Fs - Lb,
  Vb ~ Bb + e * Fs + Lb + Rb - D,
  #Gov
  Vg ~ -B,
  #CB
  Bcb ~ B - Bs - Bb,
  Vcb ~ Bcb + e * Rcb - Rb,
  #RW
  Vs ~ Bs - e * (Fs + Rcb),
  
  #Flow of funds
  ##Households
  D ~ D[-1] + Savh,
  Savh ~ W * Nf + WBjg + Pidb + Pidf + i * D[-1] - Ta - P * C,
  ##Firms current
  Pif ~ P * ( C + I + G + X) - e * Ps * M - W * Nf - i * Lb[-1],
  Piuf ~ sf * Pif,
  Pidf ~ (1 - sf) * Pif,
  ##Firms Capital
  Savf ~ Piuf - P * I,
  Lf ~ Lf[-1] - Savf,
  ##Banks
  Savb ~ (Rb - Rb[-1]) + (Bb - Bb[-1]) + (Lb - Lb[-1]) + e * (Fs - Fs[-1]) - (D - D[-1]),
  Piub ~ Savb,
  Pib ~ i * (Bb[-1] + Rb[-1] + Lb[-1] - D[-1]) + is * e * Fs[-1],
  Pidb ~ Pib - Piub,
  Lb ~ Lf,
  ##Gov
  Savg ~ -(B - B[-1]),
  B ~ B[-1] + P * G + WBjg + i * B[-1] - Ta - Picb,
  ##CB
  Picb ~ i * Bcb[-1] - i * Rb[-1],
  Savcb ~ (Bcb - Bcb[-1]) + e * (Rcb - Rcb[-1]) - (Rb - Rb[-1]),
  ##RW - Fechamento
  CA ~ P * X - e * Ps * M - i * Bs[-1] + is * e * Fs[-1],
  Rcb ~ Rcb[-1] + ( (CA + (Bs - Bs[-1])) / e ) - (Fs - Fs[-1]),
  
  #Behavioral equation
  Bb ~ rho * D,
  
  Ta ~ psi * ( W * Nf + Pidb + Pidf + i * D[-1] ),
  W ~ W[-1] + W[-1] * mu1 * (omega_w0 + mu2 * er - omega),
  P ~ P[-1] + P[-1] * varphi1 * (omega + varphi2 * er - omega_f0),
  omega ~ W / P,
  mu1 ~ mu_a + mu_b * wg - mu_u * unemp,
  tau ~ (1 - (omega * alpha + er * beta)) / (omega * alpha + er * beta),
  omega_f0 ~ (1 - (1+tau_t) * (er * beta) ) / ((1+tau_t)*alpha) + varphi2 * er,
  
  Nf ~ min(Y * alpha, N),
  Ype ~ N / alpha,
  WBjg ~ wg * W * (N - Nf),
  
  Y ~ C + I + G + X -  M,
  gi ~ gamma * (u - ut),
  I ~ max(gi + delta,0) * K[-1],
  G ~ ( 1 - g0 ) * g * Y + g0 * ( Ype - C - I - X + M ), #Funcao analoga GL (2007) JPKE
  C ~ ((sigma1 * (1 - psi) * (W * Nf + i * D[-1])) + sigma2 * (1-psi) * (Pidb + Pidf) + (sigma4 * Vh[-1]) + (sigma3 * WBjg)) / P,
  K ~ K[-1] + I - delta * K[-1],
  u ~ Y / Yk,
  Yk ~ K[-1] / v,
  
  X ~ a_exp * (P / (e * Ps))^zeta_x0 * Ys^zeta_x1,
  M ~ b_imp * (e * Ps / P)^zeta_m0 * (Y[-1])^zeta_m1,
  
  Bs ~  e * Ps * Ys * Phi2 * ( (1 + r) / ((1 + is) * (1 + phi_s) * (eet/e))),
  Fs ~ ((D / e) * Phi1) * (((1+is) * phi_d * (eet / e)) / ( 1 + r)),
  r ~ (1 + i) / ( 1 + pi) - 1,
  ee ~ eet[-1],
  eet ~ eet[-1] * (1 + pi) + varepsilon1 * (e[-1] - ee[-1]),
  
  er ~ e * Ps / P,
  
  e ~ Bs / Fs,
  
  
  #Analytical variables
  pi ~ (P - P[-1]) / P[-1],
  unemp ~ (N - Nf) / N,
  CAr ~ CA / P,
  CAY ~ CAr / Y,
  wage_share ~ (W * Nf + WBjg ) / (P * Y + WBjg),
  profit_share ~ (P * Y - W * Nf - e * Ps * M) / (P * Y + WBjg),
  Gastos_gov ~ (P * G + WBjg) / P,
  RW_share ~ er * beta,
  d_rcb ~ d(Rcb),
  d_rcbY ~ d(Rcb) / Y,
  g_rcb ~ d(Rcb) / Rcb[-1],
  k_inflow ~ Bs / P,
  k_outflow ~ er * Fs,
  net_k_flow ~ k_inflow - k_outflow,
  rendas ~ - i * Bs / P + is * er * Fs,
  comercial ~ X - M,
  RcbY ~ (Rcb * e) / (P * Y),
  RcbM ~ Rcb / M,
  d_rcbM ~d(Rcb) / M,
  RcbK ~ (Rcb * e) / (P* K),
  R_CA ~ CA / e,
  R_Bs ~ d(Bs) / e,
  R_fs ~ d(Fs),
  BY ~ B / (P * Y),
  wbar ~ (W * Nf + WBjg) / (P*N),
  eetr ~ eet / P
)

#model's parameters
jgbop.pars <- sfcr_set(
  i ~ calib.output$i,
  sf ~ calib.output$sf,
  rho ~ calib.output$rho,
  is ~ calib.output$is,
  Ps ~ calib.output$Ps,
  psi ~ calib.output$psi,
  mu_a ~ calib.output$mu_a,
  mu_b ~ calib.output$mu_b,
  mu_u ~ calib.output$mu_u,
  omega_w0 ~ calib.output$omega_w0,
  mu2 ~ calib.output$mu2,
  varphi1 ~ calib.output$varphi1,
  varphi2 ~ calib.output$varphi2,
  N ~ calib.output$N,
  alpha ~ calib.output$alpha,
  beta ~ calib.output$beta,
  wg ~ calib.output$wg,
  sigma1 ~ calib.output$sigma1,
  sigma2 ~ calib.output$sigma2,
  sigma3 ~ calib.output$sigma3,
  sigma4 ~ calib.output$sigma4,
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
  # phi_s2 ~ calib.output$phi_s2,
  Phi1 ~ calib.output$Phi1,
  phi_d ~ calib.output$phi_d,
  Phi2 ~ calib.output$Phi2,
  varepsilon1 ~ calib.output$varepsilon1,
  tau_t ~ calib.output$tau_t,
  ut ~ calib.output$u,
  gamma ~ calib.output$gamma,
  g0 ~ 0
)

#Initial values according to calibration
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
  Rcb ~ calib.output$Rcb, 
  W ~ calib.output$W,
  P ~ calib.output$P,
  Vh ~ calib.output$Vh,
  K ~ calib.output$K,
  eet ~ calib.output$eet,
  ee ~ calib.output$ee,
  pi ~ calib.output$pi,
  Y ~ calib.output$Y
)

#Baseline
jgbop.base <- sfcr_baseline(
  equations = jgbop.model,
  external = jgbop.pars,
  initial = jgbop.init,
  method = "Broyden",
  periods = 100
)



###################################################################
##.                  Baseline's outcome                          ##
###################################################################

# sfcr_dag_blocks_plot(jgbop.model)

jgbop.base.longo <- pivot_longer(jgbop.base, cols=-period)

#Check analyses on the graphs routine in the "Baseline's outcomes" section


###################################################################
##               Main model with Full employment                 ##
###################################################################
#Introducing the JG escheme
wg_shock1 <- sfcr_shock(
  variables  = sfcr_set(
    wg ~ 0.8,
    mu_b ~ 0.2,
    g0 ~ 0
  ),
  start = 1,
  end = 100
)
# Runing a shocked scenario from the last row of the baseline
wg_shock <- sfcr_scenario(
  baseline = jgbop.base,
  scenario = wg_shock1,
  periods = 100
)

# Intorducing traditional demand spurs to push unemployment rate about 2%
g_shock <- sfcr_shock(
  variables  = sfcr_set(
    wg ~ 0,
    g0 ~ 1
  ),
  start = 1,
  end = 100
)

g_scenario <- sfcr_scenario(
  baseline = jgbop.base,
  scenario = g_shock,
  periods = 100
)

# tail(g_scenario$unemp)

# Generating a baseline scenario to compare
wg_shock0 <- sfcr_shock(
  variables  = sfcr_set(
    wg ~ 0,
    g0 ~ 0
  ),
  start = 1,
  end = 100
)
wg_no_shock <- sfcr_scenario(
  baseline = jgbop.base,
  scenario = wg_shock0,
  periods = 100
)

###################################################################
##           Changing the wg to 1.                  ##
###################################################################
#Introducing the JG escheme with wg=1
wg_shock1b <- sfcr_shock(
  variables  = sfcr_set(
    wg ~ 1,
    mu_b ~ 0.2,
    g0 ~ 0
  ),
  start = 1,
  end = 100
)
# Runing a shocked scenario from the last row of the baseline
wg_shock2b <- sfcr_scenario(
  baseline = jgbop.base,
  scenario = wg_shock1b,
  periods = 100
)


###################################################################
##           Changing the Z_B from 0.2 to 0.1.                  ##
###################################################################

# Now, we assume that the JG scheme will not affect the workers bargaining so hard.
#Introducing the JG escheme
wg_mu_b_shock1 <- sfcr_shock(
  variables  = sfcr_set(
    wg ~ 0.8,
    g0 ~ 0,
    mu_b ~ 0.1
  ),
  start = 1,
  end = 100
)
# Runing a shocked scenario from the last row of the baseline
wg_mu_b_shock <- sfcr_scenario(
  baseline = jgbop.base,
  scenario = wg_mu_b_shock1,
  periods = 100
)




##############################################
#  Changing the mu_b from 0.2 to 0.4         #
##############################################

# Now, we assume that the JG scheme will affect stronger the workers bargaining.
#Introducing the JG escheme
wg_mu_b_scenario <- sfcr_shock(
  variables  = sfcr_set(
    wg ~ 0.8,
    g0 ~ 0,
    mu_b ~ 0.4
  ),
  start = 1,
  end = 100
)
# Runing a shocked scenario from the last row of the baseline
wg_mu_b_shock2 <- sfcr_scenario(
  baseline = jgbop.base,
  scenario = wg_mu_b_scenario,
  periods = 100
)

##############################################
#  Changing the mu_b from 0.2 to 0.8         #
##############################################

# Now, we assume that the JG scheme will affect stronger the workers bargaining.
#Introducing the JG escheme
wg_mu_b_scenario2 <- sfcr_shock(
  variables  = sfcr_set(
    wg ~ 0.8,
    g0 ~ 0,
    mu_b ~ 0.8
  ),
  start = 1,
  end = 100
)
# Runing a shocked scenario from the last row of the baseline
wg_mu_b_shock3 <- sfcr_scenario(
  baseline = jgbop.base,
  scenario = wg_mu_b_scenario2,
  periods = 100
)

##############################################
#  Changing the alpha from 1.6 to 1.8        #
##############################################
#Introducing the JG escheme
wg_shock1_alpha <- sfcr_shock(
  variables  = sfcr_set(
    wg ~ 0.8,
    mu_b ~ 0.2,
    g0 ~ 0,
    alpha ~ 1.8
  ),
  start = 1,
  end = 100
)
# Runing a shocked scenario from the last row of the baseline
wg_shock_alpha <- sfcr_scenario(
  baseline = jgbop.base,
  scenario = wg_shock1_alpha,
  periods = 100
)

#Estimulo de demanda
g_shock_alpha <- sfcr_shock(
  variables  = sfcr_set(
    wg ~ 0,
    g0 ~ 1,
    alpha ~ 1.8
  ),
  start = 1,
  end = 100
)

g_scenario_alpha <- sfcr_scenario(
  baseline = jgbop.base,
  scenario = g_shock_alpha,
  periods = 100
)

# Generating a baseline scenario to compare
wg_shock0_alpha <- sfcr_shock(
  variables  = sfcr_set(
    wg ~ 0,
    g0 ~ 0,
    alpha ~ 1.8
  ),
  start = 1,
  end = 100
)
wg_no_shock_alpha <- sfcr_scenario(
  baseline = jgbop.base,
  scenario = wg_shock0,
  periods = 100
)


##############################################
#               Multiplos choques.           #
##############################################
jgbop.wg.pars <- sfcr_set(
  i ~ calib.output$i,
  sf ~ calib.output$sf,
  rho ~ calib.output$rho,
  is ~ calib.output$is,
  Ps ~ calib.output$Ps,
  psi ~ calib.output$psi,
  mu_a ~ calib.output$mu_a,
  mu_b ~ calib.output$mu_b,
  mu_u ~ calib.output$mu_u,
  omega_w0 ~ calib.output$omega_w0,
  mu2 ~ calib.output$mu2,
  varphi1 ~ calib.output$varphi1,
  varphi2 ~ calib.output$varphi2,
  N ~ calib.output$N,
  alpha ~ calib.output$alpha,
  beta ~ calib.output$beta,
  wg ~ 0.8,
  sigma1 ~ calib.output$sigma1,
  sigma2 ~ calib.output$sigma2,
  sigma3 ~ calib.output$sigma3,
  sigma4 ~ calib.output$sigma4,
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
  # phi_s2 ~ calib.output$phi_s2,
  Phi1 ~ calib.output$Phi1,
  phi_d ~ calib.output$phi_d,
  Phi2 ~ calib.output$Phi2,
  varepsilon1 ~ calib.output$varepsilon1,
  tau_t ~ calib.output$tau_t,
  ut ~ calib.output$u,
  gamma ~ calib.output$gamma,
  g0 ~ 0
)

baseline.multi <- sfcr_baseline(
  equations = jgbop.model,
  initial = jgbop.init,
  external = jgbop.wg.pars,
  method = "Broyden",
  periods = 100,
)

shock.mult <- sfcr_shock(
  variables = sfcr_set(
    mu_b ~ 0.2),
  start = 1,
  end= 100
)


mu_b_scenarios <- sfcr_expand(shock.mult, mu_b, seq(0.2,0.9, 0.05))
conflict_claims <- sfcr_multis(expanded = mu_b_scenarios, fixed = baseline.multi, periods = 100)

g1_scenario <- mutate(g_scenario, simulation = 16)
unemp_scenario <- mutate(wg_no_shock, simulation =17)


########################################################
##            Cenário com Controle de Capital.        ##
########################################################
#Introducing the JG escheme
wg_shock2 <- sfcr_shock(
  variables  = sfcr_set(
    wg ~ 0.8,
    mu_b ~ 0.2,
    g0 ~ 0,
    phi_d ~0.7
  ),
  start = 1,
  end = 100
)
# Runing a shocked scenario from the last row of the baseline
wg_phi_d_shock <- sfcr_scenario(
  baseline = jgbop.base,
  scenario = wg_shock2,
  periods = 100
)

# Intorducing traditional demand spurs to push unemployment rate down
g_shock2 <- sfcr_shock(
  variables  = sfcr_set(
    wg ~ 0,
    g0 ~ 1,
    phi_d ~ 0.7
  ),
  start = 1,
  end = 100
)

g_phi_d_scenario <- sfcr_scenario(
  baseline = jgbop.base,
  scenario = g_shock2,
  periods = 100
)
