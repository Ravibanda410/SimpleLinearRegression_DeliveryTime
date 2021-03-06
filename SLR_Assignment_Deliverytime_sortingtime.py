# -*- coding: utf-8 -*-
"""
Created on Tue Apr 28 16:23:56 2020

@author: RAVI
"""
# For reading data set
# importing necessary libraries
import pandas as pd # deals with data frame  
import numpy as np  # deals with numerical values

DD_ST = pd.read_csv("C:/RAVI/Data science/Assignments/Module 6 Simple linear regression/DataSets/delivery_time.csv")
DD_ST.columns="DeliveryTime","SortingTime"

import matplotlib.pylab as plt #for different types of plots

plt.scatter(x=DD_ST['SortingTime'], y=DD_ST['DeliveryTime'],color='green')# Scatter plot

np.corrcoef(DD_ST.SortingTime,DD_ST.DeliveryTime) #correlation
help(np.corrcoef)

import statsmodels.formula.api as smf
plt.hist(DD_ST["DeliveryTime"])

model = smf.ols('DeliveryTime ~ SortingTime', data=DD_ST).fit()
model.summary()

#values prediction
#Confidence interval Calculation
pred1 = model.predict(pd.DataFrame(DD_ST['SortingTime']))
pred1
print (model.conf_int(0.95)) # 95% confidence interval

res = DD_ST.DeliveryTime - pred1
sqres = res*res
mse = np.mean(sqres)
rmse = np.sqrt(mse)

######### Model building on Transformed Data#############

# Log Transformation
# x = log(SortingTime); y = DeliveryTime
plt.scatter(x=np.log(DD_ST['SortingTime']),y=DD_ST['DeliveryTime'],color='brown')
np.corrcoef(np.log(DD_ST.SortingTime), DD_ST.DeliveryTime) #correlation

model2 = smf.ols('DeliveryTime ~ np.log(SortingTime)',data=DD_ST).fit()
model2.summary()

pred2 = model2.predict(pd.DataFrame(DD_ST['SortingTime']))
pred2
print(model2.conf_int(0.95)) # 95% confidence level

res2 = DD_ST.DeliveryTime - pred2
sqres2 = res2*res2
mse2 = np.mean(sqres2)
rmse2 = np.sqrt(mse2)

# Exponential transformation
plt.scatter(x=DD_ST['SortingTime'], y=np.log(DD_ST['DeliveryTime']),color='orange')

np.corrcoef(DD_ST.SortingTime, np.log(DD_ST.DeliveryTime)) #correlation

model3 = smf.ols('np.log(DeliveryTime) ~ SortingTime',data=DD_ST).fit()
model3.summary()

pred_log = model3.predict(pd.DataFrame(DD_ST['SortingTime']))
pred_log
pred3 = np.exp(pred_log)
pred3
print(model3.conf_int(0.95)) # 95% confidence level

res3 = DD_ST.DeliveryTime - pred3
sqres3 = res3*res3
mse3 = np.mean(sqres3)
rmse3 = np.sqrt(mse3)

############Polynomial model with 2 degree (quadratic model)  ;x = SortingTime*SortingTime; y = DeliveryTime############
#### input=x & X^2 (2-degree); output=y  ####
model4 = smf.ols('DeliveryTime ~ SortingTime+I(SortingTime*SortingTime)', data=DD_ST).fit()
model4.summary()

pred_p2 = model4.predict(pd.DataFrame(DD_ST['SortingTime']))
pred_p2

print(model3.conf_int(0.95)) # 95% confidence level

res4 = DD_ST.DeliveryTime - pred_p2
sqres4 = res4*res4
mse4 = np.mean(sqres4)
rmse4 = np.sqrt(mse4)

###########Polynomial model with 3 degree (quadratic model)  ;x = SortingTime*SortingTime*SortingTime; y = DeliveryTime############
#### input=x & X^2 (2-degree); output=y  ####
model5 = smf.ols('DeliveryTime ~ SortingTime+I(SortingTime*SortingTime)+I(SortingTime*SortingTime*SortingTime)', data=DD_ST).fit()
model5.summary()

pred_p3 = model5.predict(pd.DataFrame(DD_ST['SortingTime']))
pred_p3

print(model5.conf_int(0.95)) # 95% confidence level

res5 = DD_ST.DeliveryTime - pred_p3
sqres5 = res5*res5
mse5 = np.mean(sqres5)
rmse5 = np.sqrt(mse5)

# Log Transformation
# x = log(SortingTime); y = log(DeliveryTime)
plt.scatter(x=np.log(DD_ST['SortingTime']),y=np.log(DD_ST['DeliveryTime']),color='brown')
np.corrcoef(np.log(DD_ST.SortingTime), np.log(DD_ST.DeliveryTime)) #correlation

model6 = smf.ols('np.log(DeliveryTime) ~ np.log(SortingTime)',data=DD_ST).fit()
model6.summary()

pred_log6 = model6.predict(pd.DataFrame(DD_ST['SortingTime']))
pred_log6
pred6 = np.exp(pred_log6)
pred6
print(model6.conf_int(0.95)) # 95% confidence level

res6 = DD_ST.DeliveryTime - pred6
sqres6 = res6*res6
mse6 = np.mean(sqres6)
rmse6 = np.sqrt(mse6)


