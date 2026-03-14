                    #***************    University of Eloued    ****************************
                    #***************     Dr ;Atmane  medini     ****************************
                    #****************     2025 /2026            ****************************
                    #****************   Open Statistical Software **************************


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
  
#******* Stationary_test*********************************************************
#*****plot_test******************************************************************
plot(gdp_index)
plot(Inflation_Rate, 
     main="Inflation_rate  over time",
     col="red",
     lwd=1
     )  
plot(Interest_Rate,
     main="Interest_rate overtime",
     col="blue",
     lwd=1
)  

#***  ADF_test of  the two series ***********************************************
adf_interest=adf.test(Interest_Rate)
print(adf_interest)
adf_Exchange_Rate=adf.test(Exchange_Rate)
print(adf_Exchange_Rate)

#********************Create a daat Frame for he two series************************

# Create a data frame with the main results
adf_results <- data.frame(
  Series = c("Interest Rate", "Exchange Rate"),
  ADF_Statistic = c(adf_interest$statistic, adf_exchange$statistic),
  P_Value = c(adf_interest$p.value, adf_exchange$p.value),
  Lag_Used = c(adf_interest$parameter, adf_exchange$parameter)
)
# Print the table
print(adf_results)
#***********************************************************************************************

#************* make the second  series stationary  on taking the diff (1)***********************

print(adf.test(diff(Inflation_Rate)))
print(adf.test(diff(Exchange_Rate)))


#****  Results: Both of the series are Stationary, at Level(0) a and the second at I(1)**********
plot(Exchange_Rate_diff, col="red",main="Exchange_rate_diff",lwd=0.5)
plot(Interest_Rate_diff,col="blue",main="Interest_rate_diff",lwd=0.5)

#****  Applied of Granger_causality**************** Methode one**********************************  
#****   Step_1:Selecting the Optimal Lag Length***********

require(vars)
data_diff <- data.frame(
  Exchange_Rate_diff = diff(data$Exchange_Rate),
  Interest_Rate_diff = diff(data$Interest_Rate)
)

VARselect(data_diff,lag.max=10,type="const")

VARselect(data[c("Exchange_Rate_diff","Interest_Rate_diff")],lag.max=10,type="const")

#***************  Var  model ********************
var_model=VAR(data_diff,p=5,type = "const")
var_model

#****** causlaity_test**************************
Causality_test=causality(var_model,cause ="Exchange_Rate_diff")
Causality_test
