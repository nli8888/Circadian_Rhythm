#library(data.table)
library(rethomics)

dt <- data.table(t=1:30,
                 x=rnorm(30),
                 y=rnorm(30),
                 is_feeding = rnorm(30) >0
)
dt