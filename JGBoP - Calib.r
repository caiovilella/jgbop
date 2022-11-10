# install.packages("sfcr")
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
  pi ~ 0.1,
  u ~ 0.8,
  v ~ 1.5,
  g ~ 0.2,
  i ~ 0.03,
  delta ~ 0.1,
  is ~ 0.02,
  x ~ 0.2,
  m ~ 0.2,
  Nf ~ 80,
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
  varepsilon2 ~ 0.1
)


jgbop.calib <- sfcr_set(
  # First calibrate the balance sheet matrix
  B ~ b * P * K,
  Bcb ~ bcb * B,
  Bs ~ bs * B,
  Bb ~ B - Bs - Bcb,
  Rcb ~ rcb * P * K / e,
  Lb ~ l * P * K,
  Fs ~ fs * P * Y, #Alterei esta equação
  Rb ~ Bcb, #Perguntar ao Italo
  D ~ Bb + e * Fs + Lb + Rb, # assumes Vb = 0
  rho ~ Bb / D,
  Vh ~ D,
  Vf ~ P * K - Lb, 
  Vb ~ Bb + e * Fs + Lb + Rb - D,
  Vg ~ -B,
  Vcb ~ Bcb + e * Rcb - Rb,
  Vs ~ Bs - e * Fs - e * Rcb,
  CheckVs ~ Vh + Vb + Vg + Vcb + Vs + Vf - P * K,
  
  # Second, conditional on the BS matrix, calibrate the net borrowing/lending
  # We assume that external inflation is nil
  pi.fac ~ pi / ( 1 + pi ),
  Savh ~ D * pi.fac,
  Savf ~ -Lb * pi.fac,
  Savg ~ -B * pi.fac,
  Savs ~ Bs * pi.fac, # What about Fs and Rcb?
  Savcb ~ ( Vcb - e * Rcb ) * pi.fac,
  Savb ~ -D * pi.fac + Lb * pi.fac + Bb * pi.fac + Rb * pi.fac, # Savb is not zero and should not be. Counterfactually, it is negative in the model because of inflation-exchange rate nexus
  CheckSAV ~ Savh + Savf + Savg + Savs + Savb + Savcb,
  
  # Now, get values for non-financial variables
  Y ~ u * K / v,
  G ~ g * Y, # let us assume no WBjg at the baseline scenario (to introduce the policy thereafter). Furthermore, with a nill interest on Rcb, Picb is 0. Then
  Picb ~ i * ( Bcb - Rb ), #according to table 2 it should be Rb insted of Rcb.
  Tax ~ Savg + P * G + i * B - Picb, # this equation closes the government's constraint
  I ~ delta * K,
  Piuf ~ Savf + P * I, # this equation closes the firm's sector capital account
  X ~ x * Y,
  M ~ m * Y,
  Ps ~ ( P * X - i * Bs / ( 1 + pi ) + is * e * Fs - Savs ) / ( e * M ), # This equation closes the RW sector
  W ~ ( 1 / alpha ) * ( P / ( 1 + tau ) - beta * e * Ps ),
  alpha ~ Nf / Y,
  omega ~ W / P,
  wage_share ~ (omega) * alpha, #WS = W*N / P*Y =(W/P) * (N/Y) = omega * alpha
  beta ~ m,
  C ~ Y - X - G - I,
  Pi ~ P * ( C + I + G + X ) - Ps * e * M - i * Lb / ( 1 + pi ) - W * Nf, # This equation closes Firm's current sector
  Pidf ~ Pi - Piuf,
  SavhCheck ~ W * Nf + Pidf + Pidb - Tax + i * D / ( 1 + pi ) - P * C,
  Pib ~ i * ( Lb + Bb + Rb - D ) / ( 1 + pi ) + is * e * Fs, #this equation closes the banks' sector
  Piub ~ Savb,
  Pidb ~ Pib - Piub,
  YD ~ (1 - psi) * Y,
  Yk ~ K/(1+pi) / v,
  
  #Distributive conflict
  er ~ e * Ps / P,
  mu1 ~( ( W * pi.fac)/ (W / (1+pi)) )* ( 1 / (omega_w + mu2 * er - omega)),
  varphi1 ~ ( ( P * pi.fac) / ( P / (1+pi) ) ) * ( 1 / (omega -( omega_f - varphi2 * er))),
  
  #Foreign sector
  b_imp ~ (M / Y^zeta_m1) / (e * Ps / P)^zeta_m0,
  a_exp ~ (X / Ys^zeta_x1) / (P / e * Ps)^zeta_x0,
  Ys ~ Y / share_world_GDP,
  Phi3 ~ (Bs * pi.fac - Phi4 * e * pi.fac * (B / (1+pi))) / (Ps * Ys * ((1 + i)/ ((1+is) * (1+phi_s) * (eet / e) ))^phi_row),
  e2 ~ e / (1+pi), # O ideal seria ee_t+1 = ee_t + varepsilon ( e_t-1 - ee_t-1)
  ee ~ eet / (1+pi),
  eet ~ ee +  varepsilon1 * ( e2 / (1+pi) - ee / (1+pi)),
  Phi2 ~ (Fs * pi.fac + Phi1 * D * ( ( 1 + i) / ((1 + is) * phi_d * (eet / e) )) ) / (e * pi.fac * (Fs / (1+pi))),
  eq ~ (i * Bs / (1+pi) - P * X) / (is * Fs / (1+pi) - Ps * M),
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
