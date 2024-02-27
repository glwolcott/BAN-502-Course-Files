import pandas as pd
import numpy as np
import statsmodels.api as sm
air = pd.read_csv("/Users/gregwolcott/Documents/Python/airquality.csv")
print(air)

air_matrix = air.corr()
#print(air_matrix)

x= air[['Temp','Wind','Solar.R']]
y=air['Ozone']
x=sm.add_constant(x)

model = sm.OLS(y ,x)
results = model.fit()

print(results.params)
print(results.summary())


