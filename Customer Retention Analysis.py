# -*- coding: utf-8 -*-
"""
Created on Sat Mar 20 20:53:08 2021

@author: Kedar
"""

import pandas as pd
import numpy as np

import matplotlib.pyplot as plt
import seaborn as sns
sns.set()

import warnings
warnings.filterwarnings('ignore')

from pandas import ExcelWriter

data = pd.read_csv("K:/Documents/GitHub/Customer_Retention_Analysis/Sales_Dataset.csv", parse_dates=['Order_Date'])

#Feature Selection

features = ['Customer_ID', 'Sno', 'Order_Date', 'Quantity', 'Unit_Price']
data_clv = data[features]
data_clv['TotalSales'] = data_clv['Quantity'].multiply(data_clv['Unit_Price'])
print(data_clv.shape)
data_clv.head()

#Let's look at the descriptive statistics

data_clv.describe()

#Remove the Null Values from Unit_Price as we saw in the R code

data_clv = data_clv[pd.notnull(data_clv['Unit_Price'])]

pd.DataFrame(zip(data_clv.isnull().sum(), data_clv.isnull().sum()/len(data_clv)), columns=['Count', 'Proportion'], index=data_clv.columns)

#Let's look at the data we can use in our project

maxdate = data_clv['Order_Date'].dt.date.max()
mindate = data_clv['Order_Date'].dt.date.min()
unique_cust = data_clv['Customer_ID'].nunique()
tot_quantity = data_clv['Quantity'].sum()
tot_sales = data_clv['TotalSales'].sum()

print(f"The Time range of transactions is: {mindate} to {maxdate}")
print(f"Total number of unique customers: {unique_cust}")
print(f"Total Quantity Sold: {tot_quantity}")
print(f"Total Sales for the period: {tot_sales}")

#From the standpoint of an aggregate model, we can use an old CLV formula approach

customer = data_clv.groupby('Customer_ID').agg({'Order_Date':lambda x: (x.max() - x.min()).days, 
                                                   'Sno': lambda x: len(x),
                                                  'TotalSales': lambda x: sum(x)})

customer.columns = ['Age', 'Frequency', 'TotalSales']
customer.head()

#Calculating Variables for CLV Calculation

Average_sales = round(np.mean(customer['TotalSales']),2)
print(f"Average sales: ${Average_sales}")

Purchase_freq = round(np.mean(customer['Frequency']), 2)
print(f"Purchase Frequency: {Purchase_freq}")

Retention_rate = customer[customer['Frequency']>1].shape[0]/customer.shape[0]
churn = round(1 - Retention_rate, 2)
print(f"Churn: {churn}%")


# Calculating the CLV
Profit_margin = 0.05 

CLV = round(((Average_sales * Purchase_freq/churn)) * Profit_margin, 2)
print(f"The Customer Lifetime Value (CLV) for each customer is: ${CLV}")

#We know that this is an absurd CLV value, the reason being a few customers who will spend way too much money for the company.

#Cohort Analysis and calculation of CLV values for each cohort

# Transforming the data to customer level for the analysis
customer = data_clv.groupby('Customer_ID').agg({'Order_Date':lambda x: x.min().month, 
                                                   'Sno': lambda x: len(x),
                                                  'TotalSales': lambda x: np.sum(x)})

customer.columns = ['Start_Month', 'Frequency', 'TotalSales']
customer.head()

#Calculating CLV for each cohort

months = ['Jan', 'Feb', 'March', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']
Monthly_CLV = []

for i in range(1, 13):
    customer_m = customer[customer['Start_Month']==i]
    
    Average_sales = round(np.mean(customer_m['TotalSales']),2)
    
    Purchase_freq = round(np.mean(customer_m['Frequency']), 2)
    
    Retention_rate = customer_m[customer_m['Frequency']>1].shape[0]/customer_m.shape[0]
    churn = round(1 - Retention_rate, 2)
    
    CLV = round(((Average_sales * Purchase_freq/churn)) * Profit_margin, 2)
    
    Monthly_CLV.append(CLV)

monthly_clv = pd.DataFrame(zip(months, Monthly_CLV), columns=['Months', 'CLV'])
print(monthly_clv)

#BG/NBM

import lifetimes

# Creating the summary data using summary_data_from_transaction_data function
summary = lifetimes.utils.summary_data_from_transaction_data(data_clv, 'Customer_ID', 'Order_Date', 'TotalSales' )
summary = summary.reset_index()
summary.head()

# Create a distribution of frequency to understand the customer frequence level
summary['frequency'].plot(kind='hist', bins=50)
print(summary['frequency'].describe())
one_time_buyers = round(sum(summary['frequency'] == 0)/float(len(summary))*(100),2)
print("Percentage of customers purchase the item only once:", one_time_buyers ,"%")

#Fitting the model

# Fitting the BG/NBD model
bgf = lifetimes.BetaGeoFitter(penalizer_coef=0.05)
bgf.fit(summary['frequency'], summary['recency'], summary['T'])

#Customer Alive Probability
summary['probability_alive'] = bgf.conditional_probability_alive(summary['frequency'], summary['recency'], summary['T'])
summary.head(10)


# Visual representation of relationship between recency and frequency
from lifetimes.plotting import plot_probability_alive_matrix

fig = plt.figure(figsize=(12,8))
plot_probability_alive_matrix(bgf)

t = 30
summary['pred_num_txn'] = round(bgf.conditional_expected_number_of_purchases_up_to_time(t, summary['frequency'], summary['recency'], summary['T']),2)
summary.sort_values(by='pred_num_txn', ascending=False).head(10).reset_index()

#Gamma Gamma
# Checking the relationship between frequency and monetary_value
return_customers_summary = summary[summary['frequency']>0]
print(return_customers_summary.shape)
return_customers_summary.head()

# Checking the relationship between frequency and monetary_value
return_customers_summary[['frequency', 'monetary_value']].corr()


# Modeling the monetary value using Gamma-Gamma Model
ggf = lifetimes.GammaGammaFitter(penalizer_coef=0.001)
ggf.fit(return_customers_summary['frequency'],
       return_customers_summary['monetary_value'])

ggf.summary

#Finding Average Profit for each customer
summary = summary[summary['monetary_value'] >0]
summary['exp_avg_sales'] = ggf.conditional_expected_average_profit(summary['frequency'],
                                       summary['monetary_value'])
summary.head()


# Checking the expected average value and the actual average value in the data to make sure the values are good
print(f"Expected Average Sales: {summary['exp_avg_sales'].mean()}")
print(f"Actual Average Sales: {summary['monetary_value'].mean()}")

# Predicting Customer Lifetime Value for the next 30 days
summary['predicted_clv'] =      ggf.customer_lifetime_value(bgf,
                                                               summary['frequency'],
                                                               summary['recency'],
                                                               summary['T'],
                                                               summary['monetary_value'],
                                                               time=1,     # lifetime in months
                                                               freq='D',   # frequency in which the data is present(T)      
                                                               discount_rate=0.01) # discount rate
summary.head()


summary['manual_predicted_clv'] = summary['pred_num_txn'] * summary['exp_avg_sales']
summary.head()

# CLV in terms of profit (profit margin is 5%)
profit_margin = 0.05
summary['CLV'] = summary['predicted_clv'] * profit_margin
summary.head()

summary['CLV'].describe()

#Finding Hamper Customers
summary['exp_avg_sales'].describe()

eligible_cust = summary[summary.iloc[:,7] >= 250]
eligible = eligible_cust.sort_values('exp_avg_sales', ascending = True).head(200)

Hamper_Customers = eligible['Customer_ID']
Hamper_Customers = pd.DataFrame(data = Hamper_Customers)

#Customers to be retained
one_time = summary.loc[summary['frequency'] == 1]
one_time = one_time['Customer_ID']
one_time_1 = pd.DataFrame(data = one_time)

bottom_20_perc = summary[summary['CLV'] <= np.percentile(summary['CLV'],20)]
bottom_20_perc = bottom_20_perc['Customer_ID']
bottom_20_1 = pd.DataFrame(data = bottom_20_perc)

Retaining_Customers = pd.concat([one_time_1,bottom_20_1])
Retaining_Customers_1 = pd.DataFrame(data = Retaining_Customers)

Retaining_Customers.to_excel(r'K:/Documents/GitHub/Customer_Retention_Analysis/Retaining Customers.xlsx', index = False)
Hamper_Customers.to_excel(r'K:/Documents/GitHub/Customer_Retention_Analysis/Hamper Customers.xlsx', index = False)

