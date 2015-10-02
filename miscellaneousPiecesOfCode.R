## Cleaning special characters
features <- as.data.frame(apply(features,2,function(x)gsub('[[:punct:]]', '',x)), stringsAsFactors = FALSE)
features <- as.data.frame(apply(features,2,function(x)gsub('\\s+', '',x)), stringsAsFactors = FALSE)

## Changing some columns type
numericCols <- c('day', 'hour', 'latitude', 'longitude', 'month', 'nb_zones', 'revenue_share',
                 'timestamp', 'timezone', 'week_of_year', 'year')

features[,numericCols] <- as.numeric(as.character(unlist(features[,numericCols])))