        #***************   University of Eloued ****************************
        #***************   Dr ;Atmane  medini *********************ذذذ*******
        #****************  2025 /2026  **********************ذذذذذذذذذذذذذ******
        #****************  Open Statistical Software ***********************


data=read.csv(file.choose(data),header = TRUE,sep = ";")
head(data)
names(data)
attach(data)
View(data)

#********************Importation of necessary packages in to R_Enveronnement**
install.packages("tseries","vars","urca","ggplot2")

#** Or you can use the next code************************************************
install.packages("tseries")
install.packages("vars")
install.packages("urca")
install.packages("ggplot2")

#**if the packages are already  installed you can just require them*************
require(vars)
require(tseries)
require(urca)
require(ggplo2)
#********* Transformer the data uploaded into time series*******ذذذذ*************

gdp_index=ts(GDP_Index,frequency = 365,start = c(2000,1))
Inflation_Rate=ts(Inflation_Rate,frequency = 365,start = c(2000,1))
Interest_Rate=ts(Interest_Rate,frequency = 365,start = c(2000,1))
Exchange_Rate=ts(Exchange_Rate,frequency = 365,start = c(2000,1))
Stock_Market_Index=ts(Stock_Market_Index,frequency = 365,start = c(2000,1)) 

print(head(Stock_Market_Index))

plot(Stock_Market_Index,
     col="red",
     lwd=1,
     main="stock_market_index",
     type='l'
     )


#*Steps To Aplied Granger Causality********************************************** 
  

#***** plot_test*****************************************************************


plot(Inflation_Rate, 
     main="Inflation_rate series",
     col="red",
     lwd=1
     )  

plot(Interest_Rate,
     main="Interest_rate series",
     col="blue",
     lwd=1
)  

#****An other Method to plot  the time series***********************************
par(mfrow = c(2,1))

plot(Inflation_Rate,
     col="red",
     main="Inflation_rate series "
     )
plot(Exchange_Rate,col="blue",
     main="Interest_rate series "
     )

#***  ADF_test of  the two series ***********************************************
adf_interest=adf.test(Interest_Rate)
print(adf_interest)

adf_Exchange_Rate=adf.test(Exchange_Rate)
print(adf_Exchange_Rate)


#************* make the second  series stationary  on taking the diff (1)***********************

Exchange_Rate_diff=diff(Exchange_Rate)
print(head(Exchange_Rate_diff))
print(adf.test(diff(Exchange_Rate)))


 #****************Other method to Drow the Two plot in the same fig****************************
par(mfrow =c(2,1))
plot(Exchange_Rate_diff, col="red",
     main="Exchange_rate_diff / Master Oudit University of  Eloued"
     ,lwd=0.5)

plot(Interest_Rate,col="blue",
     main="Interest_rate_diff"
     ,lwd=0.5)


#**** Appling of Granger_causality****************Mixed Integration VAR**********************************  

#****  Step_1:Selecting the Optimal Lag Length***********
require(vars)
data_diff <- data.frame(
  Exchange_Rate_diff = diff(data$Exchange_Rate),
  Interest_Rate = (data$Interest)
)

VARselect(data_diff,lag.max=10,type="const")
print(VARselect(data_diff,lag.max=10,type="const"))


#***************Var model*****************************************
var_model=VAR(data_diff,p=5,type = "const")
print(var_model)

#****** Causlaity_test******************************************
Causality_test=causality(var_model,cause ="Exchange_Rate_diff")
Causality_test

#======================================= COINTEGRATION LECTOR================


#*********** Cointegration test*****************************************************
#****** In this Time  we Applied different series : gdp_index  and Exchange_rate*****

print(adf.test(gdp_index))
print(adf.test(Exchange_Rate))


# =====================================================
# 3. تقدير الانحدار الأحادي (Single Equation)
# ================================================

Engle_granger_model <- lm((gdp_index) ~ (Exchange_Rate))
summary(Engle_granger_model)
# ============================
# 4. استخراج البواقي (Residuals)
# ============================
residuals_eg <- (residuals(Engle_granger_model))
print(head(residuals_eg))
# ============================
# 5. اختبار البواقي على الاستقرارية (ADF test)
# ============================
adf_residuals <- adf.test(residuals_eg)

print(adf_residuals)
# ============================
# 6. التفسير
# ============================
if(adf_residuals$p.value < 0.05){
  cat("Residuals are stationary → Cointegration exists (long-run equilibrium)\n")
} else {
  cat("Residuals are non-stationary → No cointegration\n")
}

