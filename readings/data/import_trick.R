# IMPORTING TRICK USING read_csv

library(readr)
library(taylor)

# import some data
data <- taylor_all_songs[1:314,c(1,3,4,5,12:28)]

# extract the shortcut indication of the class of each column
types <- as.character(as.col_spec(data))

# modify fields that you want to change i.e. c (character) to f (factor)
types <- "fDicddididdddddiilfff"

#save both the df and the column specifications
write_csv(data, file = "data/taylorswift.csv")
saveRDS(types, file = "data/taylorswift_col_spec.rds")

# alternatively save both as one rda object
save(data, types, types, file = "readings/data/taylor.rda")

# read in the data and specify the col_types
data_tidy <- read_csv(file = "data/taylorswift.csv",
                      col_names = TRUE,
                      col_types = readRDS("data/taylorswift_col_spec.rds"))

