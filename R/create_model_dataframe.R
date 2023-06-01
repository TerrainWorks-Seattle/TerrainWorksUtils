# define where to find the data and load it.
folder <- "//Mac/Home/Documents/terrainworks/code/sandbox/data/downloaded_3.6/out/"
folder <- "/Users/julialober/Documents/terrainworks/code/sandbox/data/downloaded_3.6/out/"
load(paste0(folder, "ls_1996.Rdata"))
load(paste0(folder, "ls_2007.Rdata"))
load(paste0(folder, "ls_2011.Rdata"))
load(paste0(folder, "nonls_1996.Rdata"))
load(paste0(folder, "nonls_2007.Rdata"))
load(paste0(folder, "nonls_2011.Rdata"))

# remove these lines when the bug is fixed. the $geo field in the 2011 and 2007
# data frames is not stored as an integer value
ls_2011$geo <- ls_2011$geo$GeoClass
ls_2007$geo <- ls_2007$geo$GeoClass

# add year labels to each selection of landslide points and non-ls points
ls_1996$year <- rep(1996, length(ls_1996[, 1]))
ls_2007$year <- rep(2007, length(ls_2007[, 1]))
ls_2011$year <- rep(2011, length(ls_2011[, 1]))
nonls_1996$year <- rep(1996, length(nonls_1996[, 1]))
nonls_2007$year <- rep(2007, length(nonls_2007[, 1]))
nonls_2011$year <- rep(2011, length(nonls_2011[, 1]))

# combine into one data frame.
subset96 <- seq(1, length(nonls_1996[,1]), 10) # 1:length(ls_1996[, 1])
subset07 <- seq(1, length(nonls_2007[,1]), 10) # 1:length(ls_2007[, 1])
subset11 <- seq(1, length(nonls_2011[,1]), 10) # 1:length(ls_2011[, 1])
training_data <- rbind(ls_1996,
                       ls_2007,
                       ls_2011,
                       nonls_1996[subset96, ],
                       nonls_2007[subset07, ],
                       nonls_2011[subset11, ])

training_data$class <- as.factor(training_data$class)
