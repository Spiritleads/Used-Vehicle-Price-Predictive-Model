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
  
#Changing the doubles to thousand
to_thousands <- function(x) {
  paste0(round(x*10000, 2)) 
}
selling_price <- sapply(car_2$Selling_Price, to_thousands)
View(selling_price)

car_2$Selling_Price<-selling_price

#Merging two colums together in car_4
car_4<- unite(car_4, Name,car_4$Make,car_4$Model, sep = "-")
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

car_4 <- car_4 [, c("Name","Make","Model","Year", "Kilometer", "Fuel Type", 
                    "Transmission", "Owner","Seller Type","Engine","Max Power",
                   "Max Torque","Drivetrain","Length","Width","Height",
                   "Seating Capacity","Fuel Tank Capacity","Price")]
#Rename Columns
colnames(car_4) <- c("Name", "Year_mfd", "km_drv", "fuel_type", "seller",
                     "transmission", "no_own", "price")
