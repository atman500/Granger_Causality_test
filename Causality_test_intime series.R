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

#*Steps To Aplied Granger Causality********************************************** 
  

#****An other Method to plot  the time series***********************************
par(mfrow = c(2,1))

plot(Interest_Rate,
     col="red",
     main="Interest_Rate series "
     )

#***************************ADF test of Inflation_rate**************************
adf_interest=adf.test(Interest_Rate)
print(adf_interest)

#**************************Plot  of Exchange rate****************

plot(Exchange_Rate,col="blue",
     main="Exchange_Rate series "
     )

#***  ADF_test of Exchange _rate ***********************************************

adf_Exchange_Rate=adf.test(Exchange_Rate)
print(adf_Exchange_Rate)


#************* make the second  series stationary  on taking the diff (1)***********************

Exchange_Rate_diff=diff(Exchange_Rate)

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
  Interest_Rate = data$Interest[-1]  # The [-1] drops the first row
)

print(head(data_diff))

#***************VaR selected***********************************

VARselect(data_diff,lag.max=10,type="const")
print(VARselect(data_diff,lag.max=10,type="const"))



#***************Var model*****************************************
var_model=VAR(data_diff,p=1,type = "const")
print(var_model)

#****** Causlaity_test******************************************
Causality_test=causality(var_model,cause ="Exchange_Rate_diff")
Causality_test

