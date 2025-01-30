car_1 <- readr :: read_csv("used_cars_data//Car1.csv")
car_2 <- readr :: read_csv("used_cars_data//Car2.csv")
car_3 <- readr :: read_csv("used_cars_data//Car3.csv")
car_3 <- readr :: read_csv("used_cars_data//Car4.csv")


car_1 <- car_1 [, c("name", "year", "km_driven", "fuel", "seller_type", 
                    "transmission", "owner", "selling_price")]

#Rename Columns
colnames(car_1) <- c("Name", "Year_mfd", "km_drv", "fuel_type", "seller",
                     "transmission", "no_own", "price")
View(car_1)

library(dplyr)

#Recoding 
data_car2 <- car_2 %>% 
  mutate(
    Owner = case_when(
    Owner == "0" ~ "First owner",
    Owner == "1" ~ "Second owner",
    Owner == "3" ~ "Fourth & above owner",
    TRUE ~ as.character(owner)
   )
  )
  


#Recoding for 
