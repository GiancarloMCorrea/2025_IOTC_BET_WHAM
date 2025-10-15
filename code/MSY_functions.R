# Create function to find Fmsy:
Do_Equil_Calc = function(Hrate, n_ages, Nfleet, selmat, M_par, waassb, waafish,
                         fracyearSSB, F_rep, F_ages) {
  
  # ONLY WORKS FOR ONE AREA MODEL!
  recdist = 1
  seasdur = 1
  fleet_area_def = matrix(c(1:Nfleet, rep(1, times = Nfleet)), ncol = 2, nrow = Nfleet)
  # Calculate SPR and YPR:  
  YPR = 0
  SPR = 0
  ntemp = 1 # rec equilibrium
  # Initialize numbers matrix:
  equ_numbers = vector(length = (3*n_ages), mode = "numeric")
  equ_numbers[1] = ntemp*recdist
  # Set F-at-age matrix:
  Fmat = matrix(0, ncol = n_ages, nrow = Nfleet)
  for(f in 1:Nfleet) Fmat[f,] = Hrate[f] * selmat[f,]
  equ_Z = vector(length = n_ages, mode = "numeric")
  Survivors = numeric(1)
  equ_catch_fleet = numeric(Nfleet)
  SSB_equil_pop = vector(length = n_ages, mode = "numeric")
  for(a in 1:(3*n_ages)) {
    
    if(a > n_ages) {
      a1 = n_ages
    } else { a1 = a }
    
      if (equ_numbers[a] > 0.0) {
        N_beg = equ_numbers[a]
        if(a1 <= n_ages) {
          these_fleets = fleet_area_def[,1][fleet_area_def[,2] == 1]
          if(length(these_fleets) > 1) equ_Z[a1] = M_par[a1] + colSums(Fmat[these_fleets, ])[a1]
          else equ_Z[a1] = M_par[a1] + Fmat[these_fleets, a1]
        }
        Nsurvive = N_beg * exp(-seasdur * equ_Z[a1])
        Survivors = Nsurvive
      } else {
        equ_Z[a1] = M_par[a1]
      }

      if (a == (3 * n_ages - 1)) {
        equ_numbers[a+1] = Survivors / (1. - exp(-equ_Z[n_ages]))
      } 
      if (a < (3 * n_ages - 1)) {
        equ_numbers[a+1] = Survivors
      }
    
  } # age loop
  
    Zrate2 = (1. - exp(-seasdur * equ_Z))/equ_Z
    equ_numbers[n_ages] = equ_numbers[n_ages] + sum(equ_numbers[(n_ages+1):(3*n_ages)])
    these_fleets = fleet_area_def[,1][fleet_area_def[,2] == 1]
    for(f in seq_along(these_fleets)) {
      equ_catch_fleet[these_fleets[f]] = equ_catch_fleet[these_fleets[f]] + sum((equ_numbers[1:n_ages]*Fmat[these_fleets[f],]*waafish[these_fleets[f],])*Zrate2)
    }
    # SSB equil:
    tempvec_a = equ_numbers[1:n_ages]*exp(-fracyearSSB*equ_Z[ 1:n_ages])
    SSB_equil_pop = waassb * tempvec_a
  
  
  YPR_dead = sum(equ_catch_fleet)
  
  # F reporting:
  if(F_rep == 3) {
    equ_F_std = sum(Hrate*seasdur) # assuming only one season
  }
  
  if(F_ages[2] == n_ages) stop("F_ages[2] must be less than n_ages for F_rep 4 or 5")
  
  if(F_rep == 4) {
    tempbase = 0.0
    tempM = 0.0
    tempZ = 0.0
    for (a in F_ages[1]:F_ages[2]) {
        tempbase = tempbase + equ_numbers[a]
        tempZ = tempZ + equ_numbers[a+1] 
        temp3 = equ_numbers[a]
        temp3 = temp3*exp(-seasdur * M_par[a])
        tempM = tempM + temp3
      
    }
    equ_F_std = log(tempM) - log(tempZ)
  }
  if(F_rep == 5) {
    countN = 0.0
    for (a in F_ages[1]:F_ages[2]) {
      tempbase = 0.0
      tempM = 0.0
      tempZ = 0.0
        tempbase = tempbase + equ_numbers[a] 
        tempZ = tempZ + equ_numbers[a+1] 
        temp3 = equ_numbers[a]
        temp3 = temp3*exp(-seasdur * M_par[a])
        tempM = tempM + temp3
      
      countN = countN + 1.
      equ_F_std = equ_F_std + log(tempM) - log(tempZ)
    }
    equ_F_std = equ_F_std/countN
  }  
  
  out_vec = as.vector(c(sum(SSB_equil_pop), YPR_dead, equ_F_std))
  return(out_vec)
}


# -------------------------------------------------------------------------

Equil_Spawn_Recr_Fxn <- function(SRparm2, SSB_virgin, Recr_virgin, SPR_temp) {
  
  # Initialize variables
  steepness <- SRparm2
  B_equil <- R_equil <- temp <- 0
  alpha <- beta <- 0
  
  # Function to handle the positive function (posfun) used in the original C++
  posfun <- function(value, min_value, temp) {
    if (value < min_value) {
      return(min_value)
    }
    return(value)
  }
  
  # Same as case 6 (Beverton-Holt)
  alpha <- 4.0 * steepness * Recr_virgin / (5.0 * steepness - 1.0)
  beta <- (SSB_virgin * (1.0 - steepness)) / (5.0 * steepness - 1.0)
  B_equil <- alpha * SPR_temp - beta
  B_equil <- posfun(B_equil, 0.0001, temp)
  R_equil <- (4.0 * steepness * Recr_virgin * B_equil) / (SSB_virgin * (1.0 - steepness) + (5.0 * steepness - 1.0) * B_equil)
  
  # Return results as a named vector
  return(c(B_equil, R_equil))
}


# -------------------------------------------------------------------------
# Frel is the relative F per fleet for (last) year
# selmat is a matrix n_fleet x n_ages
# M_par is a vector n_ages
# F_rep is reporting type (as in SS3)
# F_ages are ages for F_rep == 4 or 5 (see SS3)

calc_ref_msy = function(Frel, selmat, M_par, 
                        waassb, waafish,
                        SR_R0, SR_h,
                        F_rep, 
                        F_ages, fracyearSSB = 0) {
  
  n_ages = ncol(selmat)
  Nfleet = nrow(selmat)
  Fmult = 1 # always 1
  Btgt_Fmult = Fmult # not sure
  Fmax = 5.00 * Btgt_Fmult
  Hrate = numeric(Nfleet)
  Bmark_RelF_Use = Frel 
  
  # Calculate SSB virgin:
  SSB0 = 0
  seasdur = 1
  ntemp0 = SR_R0
  for(a in 1:(3*n_ages)) {
    if(a > n_ages) {
      a1 = n_ages
    } else { a1 = a }
    SSB0 = SSB0 + (ntemp0 * waassb[a1] * exp(-fracyearSSB * seasdur * M_par[a1]))
    ntemp0 = ntemp0 * exp(-seasdur * M_par[a1])
  }
  
  # Continue code..
  SSB_unf = SSB0 # or SSB_virgin
  Recr_unf = SR_R0
  
  SPR_unfished = SSB_unf / Recr_unf
  yld1 = numeric(length = 3)
  F2 = numeric(length = 3)
  F2[1] = -log(Fmax / Btgt_Fmult - 1.)
  df = 0.050
  jj = 3
  Fishon = 1
  Closer = 1.0
  Nloops2 = 80 # or 40?
  bestF1 = 0
  bestF2 = 0
  
  for (j in 0:Nloops2) {
    df = df*0.95
    Closer = Closer*0.8
    F2[2] = F2[1] + df * 0.5
    F2[3] = F2[2] - df
    for (ii in jj:1) {
      Fmult = Fmax / (1.00 + exp(-F2[ii]))
      for (f in 1:Nfleet) {
        Hrate[f] = Fmult * Bmark_RelF_Use[f]
      } # loop
      equil_out = Do_Equil_Calc(Hrate=Hrate, n_ages = n_ages, Nfleet = Nfleet, 
                                selmat = selmat, M_par = M_par, waassb = waassb,
                                waafish = waafish, F_rep=F_rep, F_ages=F_ages, 
                                fracyearSSB=fracyearSSB)
      SSB_equil = equil_out[1]
      YPR_dead = equil_out[2]
      MSY_SPR = SSB_equil / SPR_unfished
      SPR_temp = SSB_equil
      Equ_SpawnRecr_Result = Equil_Spawn_Recr_Fxn(SRparm2 = SR_h, SSB_virgin = SSB_unf, 
                                                  Recr_virgin = Recr_unf, 
                                                  SPR_temp = SPR_temp)
      Bmsy = Equ_SpawnRecr_Result[1]
      Recr_msy = Equ_SpawnRecr_Result[2]
      
      yld1[ii] = YPR_dead * Recr_msy
      bestF1 = bestF1 + F2[ii] * ((exp(yld1[ii] / 1.0e08)^5) - 1)
      bestF2 = bestF2 + (exp(yld1[ii] / 1.0e08)^5) - 1
    }
    
    dyld = (yld1[2] - yld1[3]) / df 
    temp = (yld1[2] + yld1[3] - 2 * yld1[1]) / (0.25 * df * df) 
    dyldp = -sqrt(temp * temp + 1) 
    last_F1 = F2[1]
    temp = F2[1] - dyld * (1 - Closer) / (dyldp)
    
    if (j <= 9) {
      F2[1] = (1 - Closer) * temp + Closer * (bestF1 / bestF2)
    } else {
      F2[1] = temp
    }
  }
  
  out_vec = c(Bmsy, yld1[1], equil_out[3])
  names(out_vec) = c("SSB_MSY", "MSY", "F_MSY")
  return(out_vec)
  
}


# -------------------------------------------------------------------------
# Calculate Ref Points SS3 way:

this_model = fit1
y_i = this_model$env$data$n_years_model
sel_at_age = matrix(0, ncol = this_model$env$data$n_ages, nrow = this_model$env$data$n_fleets)
waa_fish = matrix(0, ncol = this_model$env$data$n_ages, nrow = this_model$env$data$n_fleets)
for(f in 1:this_model$env$data$n_fleets) {
  sel_at_age[f,] = t(this_model$rep$selAL[[this_model$env$data$selblock_pointer_fleets[y_i,f]]][y_i,]) %*%  this_model$rep$catch_phi_mat[,,y_i]
  waa_fish[f,] = this_model$rep$pred_waa[2,y_i,]
}

calc_ref_msy(Frel = this_model$rep$F[y_i, ]/sum(this_model$rep$F[y_i, ]),
             selmat = sel_at_age,
             M_par = this_model$rep$MAA[y_i, ],
             waassb = this_model$rep$pred_waa[1,y_i,]*this_model$rep$mat_at_age[y_i,],
             waafish = waa_fish,
             SR_R0 = exp(this_model$parList$mean_rec_pars[2]),
             SR_h = h_init,
             F_rep = 3,
             F_ages = c(1, 9),
             fracyearSSB = 0
             )


