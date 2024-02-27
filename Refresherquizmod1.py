import pandas as pd
data = pd.read_csv("/Users/gregwolcott/Documents/UNCW/BAN 502 - Predictive Analytics/Module 1/InventoryData.csv")

#1 number rows
print(len(data))
#2 number rows and columns
print(data.shape)
#3 Print the mean
print(data['On Hand'].mean())
#4 Filter to A
InventoryA = data[data['Supplier'] =='A']
print(InventoryA) 

#5 add on hand ratio
InventoryA['OnHandRatio'] = InventoryA['On Hand'] / InventoryA['Annual Demand']
print(InventoryA)

#6 Set up avg cost per sku
InventoryA = InventoryA[['Item SKU','Cost per Unit ($)']]
InventoryA = InventoryA.groupby('Item SKU',as_index=False,group_keys=True)['Cost per Unit ($)'].mean()
print(InventoryA)