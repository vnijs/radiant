nasdaq_file <- read.csv('data/nasdaq-company-list.csv', stringsAsFactors = FALSE)
symbol_list <- c("",nasdaq_file[,'Symbol'])
names(symbol_list) <- c("",nasdaq_file[,'Name'])
symbol_list <- as.list(symbol_list)

lastLoadedYahooData = ""