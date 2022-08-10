stop_dist_apatite_238 <- 18.81
stop_dist_apatite_235 <- 21.80
stop_dist_apatite_232 <- 22.25
stop_dist_apatite_147 <- 5.93

thu_zircon <- 0.87 
thu_apatite <- 1.94
  
results_apatite <- data.frame(v_max = rep(NA, 267), rs_max = NA, ft238_max = NA, ft235_max = NA, ft232_max = NA, ft147_max = NA, sa_max = NA, ftbar_max = NA, rft_max = NA, ftbar_3d = NA, rft_3d = NA)

  for (i in 1:nrow(apatite_raw)) {
    
    if (apatite_raw$geo[i] == "hexagonal") {
      # Volume
      results_apatite[i,1] <- if (apatite_raw$w_max[i] > sqrt(3)*apatite_raw$w_max[i]/2) {
        apatite_raw$l_avg[i] * apatite_raw$w_max[i] * (apatite_raw$w_max[i] - apatite_raw$w_max[i]/(2*sqrt(3))) - apatite_raw$np_num_obs[i] * (sqrt(3) * apatite_raw$w_max[i]^2 * apatite_raw$w_max[i]/8 - (1/(6*sqrt(3)))*(apatite_raw$w_max[i] - sqrt(3)*apatite_raw$w_max[i]/2)^3)
      } else {
        apatite_raw$l_avg[i] * apatite_raw$w_max[i] * (apatite_raw$w_max[i] - apatite_raw$w_max[i]/(2*sqrt(3))) - apatite_raw$np_num_obs[i] * (sqrt(3) * apatite_raw$w_max[i]^2 * apatite_raw$w_max[i]/8)
      }
                            
      # Surface Area
      results_apatite[i,7] <- 2 * apatite_raw$l_avg[i] * (apatite_raw$w_max[i] + apatite_raw$w_max[i]/sqrt(3)) + 2 * apatite_raw$w_max[i] * (apatite_raw$w_max[i] - apatite_raw$w_max[i] / (2*sqrt(3))) - apatite_raw$np_num_obs[i] * ((sqrt(3)*apatite_raw$w_max[i]^2/4) + (2 - sqrt(2)) * apatite_raw$w_max[i] * apatite_raw$w_max[i] + (sqrt(2)-1)*apatite_raw$w_max[i]^2/(2*sqrt(3))) ## THIS IS CORRECT. Cross checked against spreadsheet. 
      # Rs_SA/V
      results_apatite[i,2] <- 3*results_apatite[i,1]/results_apatite[i,7] 
      # Ft
      results_apatite[i,3] <- 1 - (3/4)*(stop_dist_apatite_238/results_apatite[i,2]) + ((0.2093 - 0.0465 * apatite_raw$np_num_obs[i]) * (apatite_raw$w_max[i] + apatite_raw$w_max[i] / sqrt(3)) + (0.1062 + 0.2234 * stop_dist_apatite_238 / (stop_dist_apatite_238 + 6 * (apatite_raw$w_max[i] * sqrt(3) - apatite_raw$w_max[i]))) * (apatite_raw$l_avg[i] - apatite_raw$np_num_obs[i] * ((apatite_raw$w_max[i] * sqrt(3)/2 + apatite_raw$w_max[i])/4))) * (stop_dist_apatite_238^2 / results_apatite[i,1])
      results_apatite[i,4] <- 1 - (3/4)*(stop_dist_apatite_235/results_apatite[i,2]) + ((0.2093 - 0.0465 * apatite_raw$np_num_obs[i]) * (apatite_raw$w_max[i] + apatite_raw$w_max[i] / sqrt(3)) + (0.1062 + 0.2234 * stop_dist_apatite_235 / (stop_dist_apatite_235 + 6 * (apatite_raw$w_max[i] * sqrt(3) - apatite_raw$w_max[i]))) * (apatite_raw$l_avg[i] - apatite_raw$np_num_obs[i] * ((apatite_raw$w_max[i] * sqrt(3)/2 + apatite_raw$w_max[i])/4))) * (stop_dist_apatite_235^2 / results_apatite[i,1])
      results_apatite[i,5] <- 1 - (3/4)*(stop_dist_apatite_232/results_apatite[i,2]) + ((0.2093 - 0.0465 * apatite_raw$np_num_obs[i]) * (apatite_raw$w_max[i] + apatite_raw$w_max[i] / sqrt(3)) + (0.1062 + 0.2234 * stop_dist_apatite_232 / (stop_dist_apatite_232 + 6 * (apatite_raw$w_max[i] * sqrt(3) - apatite_raw$w_max[i]))) * (apatite_raw$l_avg[i] - apatite_raw$np_num_obs[i] * ((apatite_raw$w_max[i] * sqrt(3)/2 + apatite_raw$w_max[i])/4))) * (stop_dist_apatite_232^2 / results_apatite[i,1])
      results_apatite[i,6] <- 1 - (3/4)*(stop_dist_apatite_147/results_apatite[i,2]) + ((0.2093 - 0.0465 * apatite_raw$np_num_obs[i]) * (apatite_raw$w_max[i] + apatite_raw$w_max[i] / sqrt(3)) + (0.1062 + 0.2234 * stop_dist_apatite_147 / (stop_dist_apatite_147 + 6 * (apatite_raw$w_max[i] * sqrt(3) - apatite_raw$w_max[i]))) * (apatite_raw$l_avg[i] - apatite_raw$np_num_obs[i] * ((apatite_raw$w_max[i] * sqrt(3)/2 + apatite_raw$w_max[i])/4))) * (stop_dist_apatite_147^2 / results_apatite[i,1])
      # Ft bar 
      results_apatite[i,8] <- (1.04 + 0.247 * thu_apatite)^-1 * results_apatite[i,3] + (1 + 4.21 * thu_apatite)^-1 * results_apatite[i,5] + (1- ((1.04 + 0.247 * thu_apatite)^-1) - ((1 + 4.21 * thu_apatite)^-1)) * results_apatite[i,4]
      # R_Ft
      results_apatite[i,9] <- ((1.04 + 0.247 * thu_apatite)^-1 * stop_dist_apatite_238 + (1 + 4.21 * thu_apatite)^-1 * stop_dist_apatite_232 + (1- ((1.04 + 0.247 * thu_apatite)^-1) - ((1 + 4.21 * thu_apatite)^-1)) * stop_dist_apatite_235) / (1.681 - 2.428 * results_apatite[i,8] + 1.153 * results_apatite[i,8]^2 - 0.406 * results_apatite[i,8]^3)
    
      # Derived 3D values 
      
      # Ft bar 
      results_apatite[i,10] <- (1.04 + 0.247 * thu_apatite)^-1 * apatite_raw$ft238_3d[i] + (1 + 4.21 * thu_apatite)^-1 * apatite_raw$ft232_3d[i] + (1- ((1.04 + 0.247 * thu_apatite)^-1) - ((1 + 4.21 * thu_apatite)^-1)) * apatite_raw$ft235_3d[i]
      # R_Ft
      results_apatite[i,11] <- ((1.04 + 0.247 * thu_apatite)^-1 * stop_dist_apatite_238 + (1 + 4.21 * thu_apatite)^-1 * stop_dist_apatite_232 + (1- ((1.04 + 0.247 * thu_apatite)^-1) - ((1 + 4.21 * thu_apatite)^-1)) * stop_dist_apatite_235) / (1.681 - 2.428 * results_apatite[i,10] + 1.153 * results_apatite[i,10]^2 - 0.406 * results_apatite[i,10]^3)
        
     } else {
      
      # Volume
      results_apatite[i,1] <- (4/3)*pi*(apatite_raw$w_max[i]/2)*(apatite_raw$w_max[i]/2)*(apatite_raw$l_avg[i]/2)
      # Surface Area
      results_apatite[i,7] <- 4 * pi * (((apatite_raw$w_max[i]/2)^1.6075 * (apatite_raw$w_max[i]/2)^1.6075 + (apatite_raw$w_max[i]/2)^1.6075 * (apatite_raw$l_avg[i]/2)^1.6075 + (apatite_raw$l_avg[i]/2)^1.6075 * (apatite_raw$w_max[i]/2)^1.6075)/3)^(1/1.6075)
      # Rs
      results_apatite[i,2] <- 3*results_apatite[i,1]/results_apatite[i,7] ##### DOUBLE CHECK THAT THIS RETURNS CORRECT VALUE!!!!!!
      # Ft
      results_apatite[i,3] <- 1 - (3/4) * (stop_dist_apatite_238/results_apatite[i,2]) + ((1/16) + 0.1686 * (1-(apatite_raw$w_max[i]/2)/results_apatite[i,2])^2) * (stop_dist_apatite_238/results_apatite[i,2])^3
      results_apatite[i,4] <- 1 - (3/4) * (stop_dist_apatite_235/results_apatite[i,2]) + ((1/16) + 0.1686 * (1-(apatite_raw$w_max[i]/2)/results_apatite[i,2])^2) * (stop_dist_apatite_235/results_apatite[i,2])^3
      results_apatite[i,5] <- 1 - (3/4) * (stop_dist_apatite_232/results_apatite[i,2]) + ((1/16) + 0.1686 * (1-(apatite_raw$w_max[i]/2)/results_apatite[i,2])^2) * (stop_dist_apatite_232/results_apatite[i,2])^3
      results_apatite[i,6] <- 1 - (3/4) * (stop_dist_apatite_147/results_apatite[i,2]) + ((1/16) + 0.1686 * (1-(apatite_raw$w_max[i]/2)/results_apatite[i,2])^2) * (stop_dist_apatite_147/results_apatite[i,2])^3
      # Ft bar 
      results_apatite[i,8] <- (1.04 + 0.247 * thu_apatite)^-1 * results_apatite[i,3] + (1 + 4.21 * thu_apatite)^-1 * results_apatite[i,5] + (1- ((1.04 + 0.247 * thu_apatite)^-1) - ((1 + 4.21 * thu_apatite)^-1)) * results_apatite[i,4]
      # R_Ft
      results_apatite[i,9] <- ((1.04 + 0.247 * thu_apatite)^-1 * stop_dist_apatite_238 + (1 + 4.21 * thu_apatite)^-1 * stop_dist_apatite_232 + (1- ((1.04 + 0.247 * thu_apatite)^-1) - ((1 + 4.21 * thu_apatite)^-1)) * stop_dist_apatite_235) / (1.681 - 2.428 * results_apatite[i,8] + 1.153 * results_apatite[i,8]^2 - 0.406 * results_apatite[i,8]^3)
      
      # Derived 3D values 
      
      # Ft bar 
      results_apatite[i,10] <- (1.04 + 0.247 * thu_apatite)^-1 * apatite_raw$ft238_3d[i] + (1 + 4.21 * thu_apatite)^-1 * apatite_raw$ft232_3d[i] + (1- ((1.04 + 0.247 * thu_apatite)^-1) - ((1 + 4.21 * thu_apatite)^-1)) * apatite_raw$ft235_3d[i]
      # R_Ft
      results_apatite[i,11] <- ((1.04 + 0.247 * thu_apatite)^-1 * stop_dist_apatite_238 + (1 + 4.21 * thu_apatite)^-1 * stop_dist_apatite_232 + (1- ((1.04 + 0.247 * thu_apatite)^-1) - ((1 + 4.21 * thu_apatite)^-1)) * stop_dist_apatite_235) / (1.681 - 2.428 * results_apatite[i,10] + 1.153 * results_apatite[i,10]^2 - 0.406 * results_apatite[i,10]^3)
        }
    }

results_apatite <- rowid_to_column(results_apatite, "id") 
results_apatite <-  mutate(results_apatite, grain = "apatite")

#Zircon

stop_dist_zircon_238 <- 15.55
stop_dist_zircon_235 <- 18.05
stop_dist_zircon_232 <- 18.43
stop_dist_zircon_147 <- 4.76

results_zircon <- data.frame(v_both = rep(NA, 226), rs_both = NA, ft238_both = NA, ft235_both = NA, ft232_both = NA, ft147_both = NA, sa_both = NA, ftbar_both = NA, rft_both = NA, ftbar_3d = NA, rft_3d = NA)

for (i in 1:nrow(zircon_raw)) {
  
  if (zircon_raw$geo[i] == "tetragonal") {
    
    # Volume
    results_zircon[i,1] <- zircon_raw$w_max[i] * zircon_raw$w_min[i] * zircon_raw$l_avg[i] - zircon_raw$np_num_obs[i] * (zircon_raw$w_min[i]/4) * (zircon_raw$w_max[i]^2 + zircon_raw$w_min[i]^2 / 3)
    # Surface Area
    results_zircon[i,7] <- 2 * (zircon_raw$w_max[i] * zircon_raw$w_min[i] + zircon_raw$w_min[i] * zircon_raw$l_avg[i] + zircon_raw$w_max[i] * zircon_raw$l_avg[i]) - zircon_raw$np_num_obs[i] * ((zircon_raw$w_max[i]^2 + zircon_raw$w_min[i]^2) / 2 + (2-sqrt(2)) * zircon_raw$w_max[i] * zircon_raw$w_min[i])
    # Rs
    results_zircon[i,2] <- 3 * results_zircon[i,1]/results_zircon[i,7]
    # Ft
    results_zircon[i,3] <- 1 - (3/4) * (stop_dist_zircon_238/results_zircon[i,2]) + (0.2095 * (zircon_raw$w_max[i] + zircon_raw$w_min[i] + zircon_raw$l_avg[i]) - (0.096 - 0.013 * (zircon_raw$w_max[i]^2 + zircon_raw$w_min[i]^2)/zircon_raw$l_avg[i]^2) * (zircon_raw$w_max[i] + zircon_raw$w_min[i]) * zircon_raw$np_num_obs[i]) * (stop_dist_zircon_238^2/results_zircon[i,1])
    results_zircon[i,4] <- 1 - (3/4) * (stop_dist_zircon_235/results_zircon[i,2]) + (0.2095 * (zircon_raw$w_max[i] + zircon_raw$w_min[i] + zircon_raw$l_avg[i]) - (0.096 - 0.013 * (zircon_raw$w_max[i]^2 + zircon_raw$w_min[i]^2)/zircon_raw$l_avg[i]^2) * (zircon_raw$w_max[i] + zircon_raw$w_min[i]) * zircon_raw$np_num_obs[i]) * (stop_dist_zircon_235^2/results_zircon[i,1])
    results_zircon[i,5] <- 1 - (3/4) * (stop_dist_zircon_232/results_zircon[i,2]) + (0.2095 * (zircon_raw$w_max[i] + zircon_raw$w_min[i] + zircon_raw$l_avg[i]) - (0.096 - 0.013 * (zircon_raw$w_max[i]^2 + zircon_raw$w_min[i]^2)/zircon_raw$l_avg[i]^2) * (zircon_raw$w_max[i] + zircon_raw$w_min[i]) * zircon_raw$np_num_obs[i]) * (stop_dist_zircon_232^2/results_zircon[i,1])
    results_zircon[i,6] <- 1 - (3/4) * (stop_dist_zircon_147/results_zircon[i,2]) + (0.2095 * (zircon_raw$w_max[i] + zircon_raw$w_min[i] + zircon_raw$l_avg[i]) - (0.096 - 0.013 * (zircon_raw$w_max[i]^2 + zircon_raw$w_min[i]^2)/zircon_raw$l_avg[i]^2) * (zircon_raw$w_max[i] + zircon_raw$w_min[i]) * zircon_raw$np_num_obs[i]) * (stop_dist_zircon_147^2/results_zircon[i,1])
    # Ft bar 
    results_zircon[i,8] <- (1.04 + 0.247 * thu_zircon)^-1 * results_zircon[i,3] + (1 + 4.21 * thu_zircon)^-1 * results_zircon[i,5] + (1- ((1.04 + 0.247 * thu_zircon)^-1) - ((1 + 4.21 * thu_zircon)^-1)) * results_zircon[i,4]
    # R_Ft
    results_zircon[i,9] <- ((1.04 + 0.247 * thu_zircon)^-1 * stop_dist_zircon_238 + (1 + 4.21 * thu_zircon)^-1 * stop_dist_zircon_232 + (1- ((1.04 + 0.247 * thu_zircon)^-1) - ((1 + 4.21 * thu_zircon)^-1)) * stop_dist_zircon_235) / (1.681 - 2.428 * results_zircon[i,8] + 1.153 * results_zircon[i,8]^2 - 0.406 * results_zircon[i,8]^3)
    
    # Derived 3D values 
    
    # Ft bar 
    results_zircon[i,10] <- (1.04 + 0.247 * thu_zircon)^-1 * zircon_raw$ft238_3d[i] + (1 + 4.21 * thu_zircon)^-1 * zircon_raw$ft232_3d[i] + (1- ((1.04 + 0.247 * thu_zircon)^-1) - ((1 + 4.21 * thu_zircon)^-1)) * zircon_raw$ft235_3d[i]
    # R_Ft
    results_zircon[i,11] <- ((1.04 + 0.247 * thu_zircon)^-1 * stop_dist_zircon_238 + (1 + 4.21 * thu_zircon)^-1 * stop_dist_zircon_232 + (1- ((1.04 + 0.247 * thu_zircon)^-1) - ((1 + 4.21 * thu_zircon)^-1)) * stop_dist_zircon_235) / (1.681 - 2.428 * results_zircon[i,10] + 1.153 * results_zircon[i,10]^2 - 0.406 * results_zircon[i,10]^3)
  
    } else {
    
    # Volume
    results_zircon[i,1] <- (4/3)*pi*(zircon_raw$w_max[i]/2)*(zircon_raw$w_min[i]/2)*(zircon_raw$l_avg[i]/2)
    # Surface Area
    results_zircon[i,7] <- 4 * pi * (((zircon_raw$w_max[i]/2)^1.6075 * (zircon_raw$w_min[i]/2)^1.6075 + (zircon_raw$w_min[i]/2)^1.6075 * (zircon_raw$l_avg[i]/2)^1.6075 + (zircon_raw$l_avg[i]/2)^1.6075 * (zircon_raw$w_max[i]/2)^1.6075)/3)^(1/1.6075)
    # Rs
    results_zircon[i,2] <- 3*results_zircon[i,1]/results_zircon[i,7] 
    # Ft
    results_zircon[i,3] <- 1 - (3/4) * (stop_dist_zircon_238/results_zircon[i,2]) + ((1/16) + 0.1686 * (1-(zircon_raw$w_max[i]/2)/results_zircon[i,2])^2) * (stop_dist_zircon_238/results_zircon[i,2])^3
    results_zircon[i,4] <- 1 - (3/4) * (stop_dist_zircon_235/results_zircon[i,2]) + ((1/16) + 0.1686 * (1-(zircon_raw$w_max[i]/2)/results_zircon[i,2])^2) * (stop_dist_zircon_235/results_zircon[i,2])^3
    results_zircon[i,5] <- 1 - (3/4) * (stop_dist_zircon_232/results_zircon[i,2]) + ((1/16) + 0.1686 * (1-(zircon_raw$w_max[i]/2)/results_zircon[i,2])^2) * (stop_dist_zircon_232/results_zircon[i,2])^3
    results_zircon[i,6] <- 1 - (3/4) * (stop_dist_zircon_147/results_zircon[i,2]) + ((1/16) + 0.1686 * (1-(zircon_raw$w_max[i]/2)/results_zircon[i,2])^2) * (stop_dist_zircon_147/results_zircon[i,2])^3
    # Ft bar 
    results_zircon[i,8] <- (1.04 + 0.247 * thu_zircon)^-1 * results_zircon[i,3] + (1 + 4.21 * thu_zircon)^-1 * results_zircon[i,5] + (1- ((1.04 + 0.247 * thu_zircon)^-1) - ((1 + 4.21 * thu_zircon)^-1)) * results_zircon[i,4]
    # R_Ft
    results_zircon[i,9] <- ((1.04 + 0.247 * thu_zircon)^-1 * stop_dist_zircon_238 + (1 + 4.21 * thu_zircon)^-1 * stop_dist_zircon_232 + (1- ((1.04 + 0.247 * thu_zircon)^-1) - ((1 + 4.21 * thu_zircon)^-1)) * stop_dist_zircon_235) / (1.681 - 2.428 * results_zircon[i,8] + 1.153 * results_zircon[i,8]^2 - 0.406 * results_zircon[i,8]^3)
    
    # Derived 3D values 
    
    # Ft bar 
    results_zircon[i,10] <- (1.04 + 0.247 * thu_zircon)^-1 * zircon_raw$ft238_3d[i] + (1 + 4.21 * thu_zircon)^-1 * zircon_raw$ft232_3d[i] + (1- ((1.04 + 0.247 * thu_zircon)^-1) - ((1 + 4.21 * thu_zircon)^-1)) * zircon_raw$ft235_3d[i]
    # R_Ft
    results_zircon[i,11] <- ((1.04 + 0.247 * thu_zircon)^-1 * stop_dist_zircon_238 + (1 + 4.21 * thu_zircon)^-1 * stop_dist_zircon_232 + (1- ((1.04 + 0.247 * thu_zircon)^-1) - ((1 + 4.21 * thu_zircon)^-1)) * stop_dist_zircon_235) / (1.681 - 2.428 * results_zircon[i,10] + 1.153 * results_zircon[i,10]^2 - 0.406 * results_zircon[i,10]^3)
  }
    
}

results_zircon <- rowid_to_column(results_zircon, "id") 
results_zircon <-  mutate(results_zircon, grain = "zircon")


# Updating data with redone calculations using the 2d_calculations.R script. 

apatite_raw$ft238_max <- results_apatite$ft238_max
apatite_raw$ft232_max <- results_apatite$ft232_max
apatite_raw$ft235_max <- results_apatite$ft235_max
apatite_raw$ft147_max <- results_apatite$ft147_max
apatite_raw$v_max <- results_apatite$v_max
apatite_raw$rs_max <- results_apatite$rs_max
apatite_raw$sa_max <- results_apatite$sa_max
apatite_raw$ftbar_max <- results_apatite$ftbar_max
apatite_raw$rft_max <- results_apatite$rft_max
apatite_raw$ftbar_3d <- results_apatite$ftbar_3d
apatite_raw$rft_3d <- results_apatite$rft_3d

xlsx::write.xlsx(apatite_raw, "./data/20220704_apatite-data.xlsx")

zircon_raw$ft238_both <- results_zircon$ft238_both
zircon_raw$ft232_both <- results_zircon$ft232_both
zircon_raw$ft235_both <- results_zircon$ft235_both
zircon_raw$ft147_both <- results_zircon$ft147_both
zircon_raw$v_both <- results_zircon$v_both
zircon_raw$rs_both <- results_zircon$rs_both
zircon_raw$sa_both <- results_zircon$sa_both
zircon_raw$ftbar_both <- results_zircon$ftbar_both
zircon_raw$rft_both <- results_zircon$rft_both
zircon_raw$ftbar_3d <- results_zircon$ftbar_3d
zircon_raw$rft_3d <- results_zircon$rft_3d

xlsx::write.xlsx(zircon_raw, "./data/20220704_zircon-data.xlsx")






