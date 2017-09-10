Data_Jul_2013 <- read.csv("/Users/Sushama/Desktop/Spring 2016/IS690Q Big Data/Project/Citibike data/2013-07 - Citi Bike trip data.csv")

usertype <- Data_Jul_2013$usertype

birth.year <- Data_Jul_2013$birth.year

gender <- Data_Jul_2013$gender

Customer_gender_0_count = 0
Customer_gender_1_count = 0
Customer_gender_2_count = 0
Subscriber_gender_0_count = 0
Subscriber_gender_1_count = 0
Subscriber_gender_2_count = 0

summary(Data_Jul_2013)

i = 1 
Customer_gender_0_count = 0
for (i in seq (1,nrow(Data_Jul_2013),1)){  
        if(i <= nrow(Data_Jul_2013) && usertype == "Customer" && gender == 0){
                        Customer_gender_0_count = Customer_gender_0_count + 1
        }
        else {break}
}


        
