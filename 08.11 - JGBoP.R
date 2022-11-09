library( sfcr )

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
  fs ~ 0.2,
  e ~ 2,  
  pi ~ 0.03,
  u ~ 0.8,
  v ~ 1.5,
  g ~ 0.2,
  i ~ 0.03,
  delta ~ 0.1,
  is ~ 0.02,
  x ~ 0.2,
  m ~ 0.2,
  Nf ~ 80,
  tau ~ 0.4
  )


jgbop.calib <- sfcr_set(
  # First calibrate the balance sheet matrix
  B ~ b * P * K,
  Bcb ~ bcb * B,
  Bs ~ bs * B,
  Bb ~ B - Bs - Bcb,
  Rcb ~ rcb * P * K / e,
  Lb ~ l * P * K,
  Fs ~ fs * P * K / e,
  Rb ~ Bcb,
  D ~ Bb + e * Fs + Lb + Rb, # assumes Vb = 0
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
  Savs ~ Bs * pi.fac,
  Savcb ~ ( Vcb - e * Rcb ) * pi.fac,
  Savb ~ -D * pi.fac + Lb * pi.fac + Bb * pi.fac + Rb * pi.fac, # Savb is not zero and should not be. Counterfactually, it is negative in the model because of inflation-exchange rate nexus
  CheckSAV ~ Savh + Savf + Savg + Savs + Savb + Savcb,
  
  # Now, get values for non-financial variables
  Y ~ u * K / v,
  G ~ g * Y, # let us assume no WBjg at the baseline scenario (to introduce the policy thereafter). Furthermore, with a nill interest on Rcb, Picb is 0. Then
  Picb ~ i * ( Bcb - Rcb ),
  Tax ~ Savg + P * G + i * B - Picb, # this equation closes the government's constraint
  I ~ delta * K,
  Piuf ~ Savf + P * I, # this equation closes the firm's sector capital account
  X ~ x * Y,
  M ~ m * Y,
  Ps ~ ( P * X - i * Bs / ( 1 + pi ) + is * e * Fs - Savs ) / ( e * M ),
  W ~ ( 1 / alpha ) * ( P / ( 1 + tau ) - beta * e * Ps ),
  alpha ~ Nf / Y,
  beta ~ m,
  C ~ Y - X - G - I,
  Pi ~ P * ( C + I + G + X ) - Ps * e * M - i * Lb / ( 1 + pi ) - W * Nf,
  Pidf ~ Pi - Piuf,
  SavhCheck ~ W * Nf + Pidf + Pidb - Tax + i * D / ( 1 + pi ) - P * C,
  Pib ~ i * ( Lb + Bb + Rb - D ) / ( 1 + pi ) + is * e * Fs,
  Piub ~ Savb,
  Pidb ~ Pib - Piub
)


calib.output <- sfcr_baseline(
  equations = jgbop.calib,
  external = calib.input,
  periods = 2,
  method = 'Gauss'
)

calib.output <- calib.output[-1,]

