# Simulation & Risk Analysis Project 

Compagnie Pétrolière et Gazière, INC. (hereafter the “Company”) requested to analyze oil and gas drilling project. The drilling outcome results in either a "dry well" or a "wet well." A wet well generates revenue through the production of oil/gas over time, calculated based on assumed product prices and declining production rates from initial values. I simulated the future values of 2024 drilling costs and simulated distributions for the cost of a dry well and the net present value (NPV) for a wet well. Additionally, I incorporated the probability of a wet well and the number of wells to simulate the NPV for the entire project. My simulation resulted in a right-skewed distribution for the NPV with a median of $250 million with an initial investment of $124 million. Over 15 years, this is a 4.8% annual return on investment. 

## Data used
The dataset provided by the Company includes estimated drilling costs for crude oil, natural gas, and dry wells spanning the period from 1960-2007. Due to changes in reporting regulations and the evolving nature of the industry, only data from 1990-2006 is deemed relevant for this analysis. The year 2007 is considered an outlier and has been excluded from further consideration. Additionally, the provided data includes arithmetic annual changes in drilling costs, calculated from 1990-2006, which served as the basis for simulating possible future values of drilling costs for 2024. Instead of focusing individually on costs for oil, gas, and dry wells, a recommendation is made by the Company's analysts to treat them equally and assume that an average cost applies to all of them, assuming independence, resulting in a final dataset of 48 observations. In addition, I used annual projections of oil prices from 2025 to 2050, which include high, low, and actual estimates for oil prices. 

## Methodology
I simulated the price of drilling a single dry well and the NPV, which is the sum of all expenses and revenues of drilling a single wet well. Subsequently, I simulated the number of wet wells to estimate the NPV of the entire project. Then, I multiplied the number of each well to their respective costs to calculate the NPV in the next 15 years. Throughout 100,000 simulations, I ensured to account for the extreme scenarios to account for the potential risk of the projects. 

### Dry Well Cost
If the company encounters a dry well, it incurs expenses including drilling, seismic, lease, professional overhead, and completion costs, with no revenue generated. I simulated the Dry Well Costs from 2007-2023. Based on the U.S. Energy Information Association report, I followed different distribution assumptions to simulate each period of time:
#### 2007-2012 Drilling Cost
The Company’s Price Analysis group assumed that drilling cost changes from one year to the next from 2006-2012 follow a normal distribution. For the first approach, I validated this normality assumption by viewing quantile-quantile plots and through the Shapiro-Wilk normality test and found this assumption reasonable. 
#### 2012-2023 Drilling Cost
I used a beta distribution to simulate the drilling cost fluctuations for 2012-2015 and 2015-2023. The beta distribution was achieved through a three-point estimation method incorporating the mean, minimum, and maximum values provided in the report, ensuring alignment of projections with a normal distribution shape. To capture the decreasing price trend (-9.17% on average) from 2012-2015, I used a B(α=1, β=7) distribution with a maximum of 22% and a minimum of 7%. For the increasing trend from 2015-2023 (5% on average), I used a B(α=7, β=2) distribution with a maximum of 6% and a minimum of 2%.
#### SEISMIC AND LEASE COSTS
Seismic and lease costs include purchasing seismic data to determine the optimal well location and the right to drill on the land. The price of a single seismic section is $43,000, and the number of seismic sections necessary per well is normally distributed with a mean of 3 sections and a standard deviation of 0.35. The price to lease a single acre is $960, and the number of acres required per well is also normally distributed with a mean of 600 acres and a standard deviation of 50.
#### PROFESSIONAL OVERHEAD
The professional overhead per well is represented by a triangular distribution, with a most likely cost of $215,000, a minimum of $172,000, and a maximum of $279,500. This is an annual expense over the lifetime of a well but stops after the first year if the well is dry.
#### FINAL DRY WELL COST
Dry wells incur drilling costs, seismic and lease costs, and one year of professional overhead costs. The sum of the mean and most likely expenses is $6,238,000 with a mean drilling cost of $5,318,000, mean seismic and lease cost of $705,000, and most likely an overhead cost of $215,000. For a complete understanding of potential dry well costs, see Figure 1.

```ruby
#NPV of a single Dry well
Dry_well_cost <- rep(0,100000)
set.seed(123)
for(i in 1:100000){
  #Drilling cost of 2006 is $2279.8
  P0 <- 2279.8
  r <- rnorm(n=1, mean = 0.1314913, sd = 0.1784372)
  #2006 to 2007
  Pt <- P0*(1 + r)
  #2007 to 2012
  for(j in 1:5){
    r <- rnorm(n=1, mean = 0.1314913, sd = 0.1784372)
    Pt <- Pt*(1+r)
  }
  #2012 to 2015
  for(z in 1:3){
    #Use beta distribution (best match to the given mean,min and max)
    r <- mean(-rescale(rbeta(n = 1000,shape1 = 1, shape2 = 7),c(0.07,0.22)))
    Pt <- Pt*(1+r)
  }
  #2015 to 2024
  for(x in 1:9){
    #Use beta distribution (best match to the given mean,min and max)
    r <- mean(rescale(rbeta(n = 1000,shape1 = 7, shape2 = 2),c(0.02,0.06)))
    Pt <- Pt*(1+r)#Drilling cost
  }
  #Lease Cost
  Acre <- rnorm(n=1, mean = 600, sd = 50)
  Lease_cost <- Acre * 960
  #Seismic Cost
  Seismic_section <- rnorm(n=1, mean = 3, sd = 0.35)
  Seismic_cost <- Seismic_section * 43000  
  #Overhead Cost
  Overhead_cost <- rtri(1, 172000, 279500, 215000) 
  #Drilling cost Unit is thousand dollar
  Dry_well_cost[i] <- Pt*1000 + Lease_cost + Seismic_cost + Overhead_cost
}
``` 
The dry well cost simulation resulted in a normal-looking distribution, with a median cost of $5,913,626. This distribution is shown in Figure 1. The worst-case scenario
 2
is represented by the right side of the distribution. In the worst 5% of scenarios, the average cost of a dry well is $11,838,281.

<img width="575" alt="image" src="https://github.com/taeseoklee66/Tae_Lee/assets/120340773/4badce8f-aba8-4ab6-ad18-7c8f4b8391de">

### Wet Well NPV
The NPV for a wet well is calculated, accounting for analogous expenses incurred for a dry well, alongside additional factors such as completion cost, production risk, revenue risk, and operating expenses.

#### COMPLETION COST
If it is determined that there is oil present in the well, engineers must prepare the well to produce oil. This is the completion cost, which is normally distributed with a mean of $390,000 and a standard deviation of $50,000.
#### PRODUCTION RISK
Oil extraction's production risk is addressed by simulating oil production rates. Initial production rates (IP) are modeled with a lognormal distribution, derived from an underlying normal distribution, with a mean of 420 barrels of oil per day, and a standard deviation of 120 barrels of oil per day. Additionally, the decline rate is modeled with a uniform distribution between 15% and 32%. These distributions, accompanied by a correlation coefficient of 0.64, represent production uncertainties. I applied the IP and the decline rate values to the formula for calculating the production rate at the year's end and multiplied by the number of days in the year to derive the total production volume.
#### REVENUE RISK
Future oil prices are projected using long-term forecasts, incorporating historical data and estimates provided by the World Bank of EIA. Throughout the project's duration, oil price distributions are modeled with a triangle distribution, covering worst-case, best-case, and typical scenarios. Net revenue interest (NRI) is normally distributed with a mean of 75% and a standard deviation of 2%.
#### OPERATING EXPENSES
Operating expenses, encompassing manpower and hardware costs, are modeled with a normal distribution, averaging $2.25 per barrel with a standard deviation of $0.30 per barrel. Additionally, I factored in a severance tax of 4.6%.
#### NET PRESENT VALUE CALCULATION
The final step of the calculation involves summing all revenues and expenses for each year (beginning at year 0), discounting them at the weighted average cost of capital, assumed to be 10% per year for this model, and aggregating them across years to compute the forecasted NPV for the project.

```ruby
#Set Initial production rate and yearly decline rate
IP = rlnorm(100000, meanlog = 6, sdlog = 0.28)
decline_rate = runif(100000, min = 0.15, max = 0.32)
#Correlation 0.64 between the IP and the decline rate
ip_decline_rate <- rep(0,100000)
R <- matrix(data=cbind(1, 0.64, 0.64, 1), nrow=2)
U <- t(chol(R))
#Make a function for adding correlation 
standardize <- function(x){
  x.std = (x - mean(x))/sd(x)
  return(x.std)
}
destandardize <- function(x.std, x){
  x.old = (x.std * sd(x)) + mean(x)
  return(x.old)
}
#Correlation 
Both.r <- cbind(standardize(IP), standardize(decline_rate))
SB.r <- U %*% t(Both.r)
SB.r <- t(SB.r)
ip_decline_rate <- cbind(destandardize(SB.r[,1], IP), destandardize(SB.r[,2], decline_rate))
#NPV of a single Wet Well
NPV <- rep(0,100000)
set.seed(123)
for(i in 1:100000){
  #Drilling cost 
  #Average cost of 2006
  P0 <- 2279.8
  r <- rnorm(n=1, mean = 0.1314913, sd = 0.1784372)
  #2006 to 2007
  Pt <- P0*(1 + r)
  #2007 to 2012
  for(j in 1:5){
    r <- rnorm(n=1, mean = 0.1314913, sd = 0.1784372)
    Pt <- Pt*(1+r)
  }
  #2012 to 2015
  for(z in 1:3){
    #Use beta distribution (best match to the given mean,min and max)
    r <- mean(-rescale(rbeta(n = 1000,shape1 = 1, shape2 = 7),c(0.07,0.22)))
    Pt <- Pt*(1+r)
  }
  #2015 to 2024
  for(x in 1:9){
    r <- mean(rescale(rbeta(n = 1000,shape1 = 7, shape2 = 2),c(0.02,0.06)))
    Pt <- Pt*(1+r)
  }
  #Lease Cost
  Acre <- rnorm(n=1, mean = 600, sd = 50)
  Lease_cost <- Acre * 960
  #Seismic Cost
  Seismic_section <- rnorm(n=1, mean = 3, sd = 0.35)
  Seismic_cost <- Seismic_section * 43000
  #Now Additional costs for wet well
  #Completion Cost (One time cost if the well is wet)
  Completion_cost <- rnorm(n=1, mean = 390000, sd = 50000)
  #2024 - 2039 year Overhead cost (Remain same across the life time of well)
  Overhead_cost <- rtri(1, 172000, 279500, 215000)
  #Production Risk
  #New IP and decline_rate after correlation is applied.
  IP <- ip_decline_rate [i,1]
  decline_rate <- ip_decline_rate [i,2]
  #2024 End year production rate
  Production_rate <- (1-decline_rate) * IP
  Oil_volume <- 365*((IP + Production_rate)/2)
  Oil_price <- rtri(1, Price[1,3], Price[1,2], Price[1,4])
  #Operating expense
  Operating_cost <- rnorm(n=1, mean = 2.25, sd = 0.30)
  NRI <- rnorm(n=1, mean = 0.75, sd = 0.02)
  #Year Revenue
  Year_revenue <- (Oil_volume * Oil_price * NRI) - (Oil_volume*Operating_cost) - (Oil_volume * Oil_price * NRI * 0.046) - Overhead_cost
  #NRI
  Final_Year_revenue <- (Year_revenue/(1+0.1))
  #2025 - 2039 year production rate
  for(j in 2:15){
    Production_rate_begin <- Production_rate
    #Production at the end of year
    Production_rate <- (1-decline_rate) * Production_rate_begin
    Production_rate_end <- Production_rate
    Oil_volume <- 365*((Production_rate_begin + Production_rate_end)/2)
    Oil_price <- rtri(1, Price[j-1,3], Price[j-1,2], Price[j-1,4])
    #Operating cost change over year
    Operating_cost <- rnorm(n=1, mean = 2.25, sd = 0.30)
    Year_revenue <- (Oil_volume * Oil_price * NRI) - (Oil_volume*Operating_cost) - Overhead_cost - (Oil_volume * Oil_price * NRI * 0.046)
    Final_Year_revenue <- Final_Year_revenue + (Year_revenue/(1+0.1)^(j))
  }
  NPV[i] = Final_Year_revenue - Lease_cost - Seismic_cost - Completion_cost - (Pt*1000)
}
```
The NPV simulation resulted in a right-skewed distribution, shown in Figure 2 below. The median of the distribution is $19,077,186. In this case, the worst-case scenario is represented by the left side of the distribution. In the worst 5% of scenarios, the mean NPV is $6,586,623.
<img width="632" alt="image" src="https://github.com/taeseoklee66/Tae_Lee/assets/120340773/f8b4a2bf-8990-4742-9f92-863e6b243ca4">

### Wet Well Likelihood
The two variable factors necessary for a profitable well are that it contains hydrocarbons and that a reservoir can be developed in the rock formation to hold hydrocarbons. The probability of hydrocarbon presence in a well is normally distributed with a mean of 99% and a standard deviation of 5%. The probability that a reservoir can be developed is normally distributed with a mean of 80% and a standard deviation of 10%. The two constant factors associated with a wet well are that an impermeable seal must be available to trap the hydrocarbons in the reservoir, and a structure or closure must be present that will cause the hydrocarbons to pool in a field where the drill bit will penetrate. Both of these factors have a 100% probability of occurrence. Additionally, I calculated 5% Value at risk (VaR) and Conditional Value at Riks (CVaR).

### NUMBER OF WET WELLS
To calculate the number of wet wells, I used the probabilities of wet wells outlined previously along with the total number of planned wells. Using a normal distribution for the probability of hydrocarbons and reservoirs, I multiplied to get the overall probability of a wet well. I then used a uniform distribution with a minimum of 10 and a maximum of 30 to simulate the total number of wells, and multiplied by the probability to get the total number of wet wells.

```ruby
#Probability of Wet well & Dry well 
P_Wetwell <- rep(0,100000)
P_Drywell <- rep(0,100000)

set.seed(123)
for (i in 1:100000){
  P_Hydrocarbons <- rtruncnorm(1, a = 0, b = 100, mean = 99, sd = 5) / 100
  P_Reservoir <- rtruncnorm(1, a = 0, b = 100, mean = 80, sd = 10) / 100
  #Probability of Wet well is P_Hydrocarbond * P_Reservoir 
  P_producing_wells <- P_Hydrocarbons * P_Reservoir
  N_Planned_well <- runif(1, min = 10, max = 30)
  wells_producing <- rbinom(N_Planned_well, 1, P_producing_wells)
  
  Proportion_Wetwell <- sum(wells_producing == 1) / length(wells_producing)
  Proportion_Drywell <- sum(wells_producing == 0) / length(wells_producing)
  
  P_Wetwell[i] <- Proportion_Wetwell
  P_Drywell[i] <- Proportion_Drywell
}
#5% VaR & CVaR of Wet well
VaR_Wet <- quantile(P_Wetwell, 0.05, na.rm=TRUE)
CVaR_Wet <- round(mean(P_Wetwell[P_Wetwell < VaR_Wet]),2)
h <- hist(P_Wetwell, plot=FALSE)
cuts <- cut(h$breaks, c(-Inf, VaR_Wet, Inf))
plot(h, col = cuts, xlab='Probability of Wet well')
abline(v = VaR_Wet, col="red", lwd=2)
mtext(paste("Value at Risk",VaR_Wet, sep=" = "), at=VaR_Wet, col="red")
mtext(paste("5% CVaR",CVaR_Wet, sep = " = "), at=CVaR_Wet, col="blue", adj = 1, line = -2)
```
<img width="933" alt="image" src="https://github.com/taeseoklee66/Tae_Lee/assets/120340773/889ca9e0-5d4f-4dd6-bbe2-6ebec16b5b23">

The median proportion of wet wells is 77%, with a sharp decline in frequency of around 60%. The 5% VaR is 52%, indicating that in the worst 5% of situations, the company can expect 52% or less of the wells drilled to be wet. The 5% CVaR is 45%, meaning that the company can expect 45% of wells to be wet in the 5% worst cases on average.
### NPV CALCULATION
I multiplied the number of each well to their respective costs to calculate the NPV in the next 15 years. Throughout 100,000 simulations, I ensured to account for the extreme scenarios to account for the potential risk of the projects. Risk was further assessed using Value at Risk (VaR) and Conditional Value at Risk (CVaR).
```ruby
Final_NPV <- rep(0,100000)
for (k in 1:100000){
  P_Hydrocarbons <- rtruncnorm(1, a = 0, b = 100, mean = 99, sd = 5) / 100
  P_Reservoir <- rtruncnorm(1, a = 0, b = 100, mean = 80, sd = 10) / 100
  P_producing_wells <- P_Hydrocarbons * P_Reservoir
  
  N_Planned_well <- runif(1, min = 10, max = 30)
  wells_producing <- rbinom(N_Planned_well, 1, P_producing_wells)
  
  N_Wetwell <- sum(wells_producing == 1)
  N_Drywell <- sum(wells_producing == 0)
  
  #Dry_Well_Cost
  Dry_cost <- sample(Dry_well_cost,N_Drywell,replace=FALSE)
  Final_Dry_well_cost <- sum(Dry_cost)
  
  #Wet_Well_Cost
  Wet_cost <- sample(NPV,N_Wetwell,replace=FALSE)
  Final_Wet_well_cost <- sum(Wet_cost)
  
  Final_NPV[k] = Final_Wet_well_cost - Final_Dry_well_cost
}  
#Check out VaR & CVaR
VaR_Final <- round(quantile(Final_NPV, 0.05, na.rm=TRUE),2)
CVaR_Final <- mean(Final_NPV[Final_NPV < VaR_Final], na.rm=TRUE)

#Dollar sign for the graph
NPV_VaR <- dollar(round(VaR_Final/1000000,2))
NPV_CVaR <- dollar(round(CVaR_Final/1000000,2))
NPV_median <- dollar(round(summary(Final_NPV)[3]/1000000,2))

#Visualize CVaR and VaR
hist(Final_NPV/1000000, breaks = 100, main = '', xlab = '$ Millions')
abline(v = CVaR_Final/1000000, col = "red", lwd = 2, lty = 2)
abline(v = VaR_Final/1000000, col = "red", lwd = 2)
abline(v = summary(Final_NPV)[3]/1000000, col = 'blue', lwd = 2)
legend_labels <- c(paste("CVaR", NPV_CVaR, sep = " = "), paste("VaR", NPV_VaR, sep = " = "),paste("Median",NPV_median, sep = " = "))
legend("topright", legend = legend_labels, col = c("red", "red","blue"), lwd = 3, lty = c(2, 1,1))

```
<img width="932" alt="image" src="https://github.com/taeseoklee66/Tae_Lee/assets/120340773/b53f9c0a-d5ab-4886-9dfe-a3639eaa81b1">

The median of the distribution is $250 million. The VaR for my NPV simulation stands at $ $100 million. This means that in 5% of cases, I can expect the NPV to fall below $100 million. Additionally, the CVaR of $72 million means that if the NPV dips below the VaR, I can expect the CVaR to be around $72 million.

### INITIAL INVESTMENT
After conducting NPV calculations, I evaluated the initial investment, aiming to assess the project's annual return and understand its financial viability. The initial investment for the dry well comprises drilling, lease, seismic, and overhead costs, while the initial investment for wet well also includes completion costs in addition to those associated with the dry well.

```ruby
#Initial invest amount
median_Dry_cost <- median(Dry_well_cost)
median_Wet_Proportion <- median(P_Wetwell)

## 390,000 is the completion cost
## 20 is the median of # of planned well 
Initial_spend = (median_Dry_cost * (1-median_Wet_Proportion) + (median_Dry_cost + 390000) * median_Wet_Proportion) * 20
```
The total initial investment cost was calculated by multiplying the number of each wells to the respective initial investment cost. I got a median initial investment of $124 million. Based on this initial investment alongside the median NPV, I established a 4.8% annual profit return for the project.

## Results
After comprehensive analysis of the project, I got the following findings:
1. The distribution for the proportion of wet wells yielded a median of 77%.
2. The final distribution for the Net Present Value (NPV) of the project resulted in a median of $250 million. In the worst 5% of cases, the Value at Risk (VaR) was $100 million, with a Conditional Value at Risk (CVaR) of $72 million.
3. The initial investment amounted to $124 million, with an expected annual return of 4.8%.

## Recommendation & Conclusion
I assisted in simulating the price of drilling a single dry well and the NPV alongside the likelihood of encountering a wet well, thereby contributing to the decision-making process for the drilling project. Based on my findings, I do not recommend proceeding with this project if the company expects a higher annual return than 4.8%. This rate significantly lags behind the historical average annual return of the S&P 500 index, which is 10.17% over the last 100 years, making the project less appealing in comparison. I encourage the company to analyze alternative investments that align with the company's long-term objectives and financial goals.
