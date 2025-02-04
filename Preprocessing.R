car_1 <- readr :: read_csv("used_cars_data//Car1.csv")
car_2 <- readr :: read_csv("used_cars_data//Car2.csv")
car_3 <- readr :: read_csv("used_cars_data//Car3.csv")
car_4 <- readr :: read_csv("used_cars_data//Car4.csv")
library(dplyr)


car_1 <- car_1 [, c("name", "year", "km_driven", "fuel", "seller_type", 
                    "transmission", "owner", "selling_price")]

#Rename Columns
colnames(car_1) <- c("Name", "Year_mfd", "km_drv", "fuel_type", "seller",
                     "transmission", "no_own", "price")
View(car_1)


#Recoding for car2
car_2 <- car_2 %>% 
  mutate(
    Owner = case_when(
    Owner == "0" ~ "First owner",
    Owner == "1" ~ "Second owner",
    Owner == "3" ~ "Fourth & above owner",
    TRUE ~ as.character(Owner)
   )
  )
  
car_2 <- car_2 [, c("Car_Name","Year","Kms_Driven", "Fuel_Type","Seller_Type",
                    "Transmission", "Owner","Selling_Price")]
#Rename for car 4
colnames(car_2) <- c("Name", "Year_mfd", "km_drv", "fuel_type", "seller",
                     "transmission", "no_own", "price")

#Changing
car_2$price <- as.numeric(car_2$price)

#Changing the doubles to thousand
to_thousands <- function(x) {
  paste0(round(x*10000, 2)) 
}
price <- sapply(car_2$price, to_thousands)

price <- as.numeric(price)

car_2$price<-price

#Merging two colums together in car_4
car_4$Name<- paste(car_4$Make,car_4$Model, sep = " ")
print(car_4)

#RECODING CAR 4
car_4 <- car_4 %>%
  mutate(
    Owner == case_when(
      Owner == "First" ~ "First owner",
      Owner == "Second" ~ "Second owner",
      Owner == "Third" ~ "Third owner",
      Owner == "Fourth" ~ "Fourth & above owner",
      Owner == "4 or More" ~ "Fourth & above owner",
      TRUE ~ as.character(Owner)
      
    )
  )

car_4$Name<- paste(car_4$Make,car_4$Model, sep = " ")
print(car_4)

#Reorder for car 3
car_3 <- car_3 [, c("name","year","km_driven", "fuel","seller_type",
                    "transmission", "owner","selling_price")]
#Rename for car 3
colnames(car_3) <- c("Name", "Year_mfd", "km_drv", "fuel_type", "seller",
                     "transmission", "no_own","price")

#Reorder for car 4
car_4 <- car_4 [, c("Name","Year","Kilometer", "Fuel Type","Seller Type",
                    "Transmission", "Owner","Price")]
#Rename for car 4
colnames(car_4) <- c("Name", "Year_mfd", "km_drv", "fuel_type", "seller",
                     "transmission", "no_own", "price")

Merge_Cars <- bind_rows(car_1, car_2, car_3, car_4)
write.csv(Merge_Cars, "Merge Cars.csv", row.names = F)
