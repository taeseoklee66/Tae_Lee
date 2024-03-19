# Tae_Lee
Simulation Project
# Project 1: Simulation

VaR_Wet <- quantile(P_Wetwell, 0.05, na.rm=TRUE)
CVaR_Wet <- round(mean(P_Wetwell[P_Wetwell < VaR_Wet]),2)
