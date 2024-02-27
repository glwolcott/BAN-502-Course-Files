#cars=as_tibble(mtcars)
#str(cars)
diamonddata = diamonds
diamonddata
#1
nrow(diamonddata)
#2
ncol(diamonddata)
#3
ggplot(diamonddata, aes(x=carat, y=price)) + geom_point()
#4
#ggplot(diamonddata, aes(x=carat, y=price,color=cut)) + geom_point()
#5
ggplot(diamonddata, aes(x=carat, y=price)) + geom_point()+ facet_grid(color ~ .)
ggplot(diamonddata, aes(x=carat, y=price)) + geom_point()+ facet_grid(cut ~ .)
#6
Inventory = read_csv("/Users/gregwolcott/Documents/UNCW/BAN 502 - Predictive Analytics/Module 1/InventoryData.csv")
str(Inventory)
#7
mean(Inventory[["On Hand"]])
#8
InventoryA = data_frame(filter(Inventory, Supplier=='A'))
nrow(InventoryA)
#9
InventoryA = mutate(InventoryA, OnHandRatio = `On Hand` / `Annual Demand`)
str(InventoryA)
#10
Newdata <- InventoryA[,colnames(InventoryA)[c(1,4)]]
Newdata

Newdata %>%
  group_by(`Item SKU`) %>%
  summarise(mean_cost = mean(`Cost per Unit ($)`))


