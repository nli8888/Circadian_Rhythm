#library(data.table)
library(rethomics)

dt <- data.table(t=1:30,
                 x=rnorm(30),
                 y=rnorm(30),
                 is_feeding = rnorm(30) >0
)
dt
dt_region_1 <- data.table(t=1:10,
                          x=rnorm(10),
                          y=rnorm(10)
)
# 15 reads
dt_region_2 <- data.table(t=1:15,
                          x=rnorm(15),
                          y=rnorm(15)
)
dt_region_1[, region_id := 1]

# We make a column animal_id with the value 2
dt_region_2[, region_id := 2]

# we simply put all the rows together an a new data tables called dt
dt <- rbind(dt_region_1, dt_region_2)
dt[,.N,by="region_id"]
dt[,
   list(x_mean = mean(x)),
   by="region_id"
   ]
data(sleep_sexual_dimorphism)
sleep_sexual_dimorphism
key(sleep_sexual_dimorphism)