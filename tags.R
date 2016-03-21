
# message("The function *tags* takes four arguments: 1st your pinterest login, 2nd your pinterest password,
# 3rd the URL of the pinterest board, 4th the name you will give to your data file, ending .txt.")
#
# tags <- function(enterLogin, enterPassword, enterURL, dataFile) {
#
#     library(RSelenium)
#     startServer()
#     remDr <- remoteDriver()
#     remDr$open()
#     remDr$setWindowSize(width = 1920, height = 1080)
#
#     remDr$navigate("https://uk.pinterest.com/login/")
#     email <- remDr$findElement(using = 'css selector', ".email")
#     email$sendKeysToElement(list("enterLogin"))
#     pass <- remDr$findElement(using = 'css selector', ".loginPassword input")
#     pass$sendKeysToElement(list("enterPassword", "\uE007"))
#
#     remDr$navigate("enterURL")
#     webElem <- remDr$findElement("css", "body")
#     replicate(10, webElem$sendKeysToElement(list(key = "control", "-")))
#     webElem$sendKeysToElement(list(key = "end"))
#     webElem$sendKeysToElement(list(key = "control", "s"))
#
#     message("In the working directory, choose to save as webpage complete, with a name ending with .txt")

#####################

options(stringsAsFactors = FALSE)

# Open browser (firefox is best), navigate to board (while logged in). Zoom out as far as you can. Make window as wide as possible.
# If you can see ALL of your pinned items at once, that is good.
# Right click, save page. File name: "pins.txt". File type: "webpage, complete"
# The files folder can be deleted, the .txt file should be placed in current working directory.

importTags <- function(fileName="pins.txt") {
# function takes one argument, of the file name for the html of the board. By default, can call it "pins.txt"

    options(warn = -1)
    pinText <- readLines(fileName)
    x <- grep("pinDescription", pinText)
    y <- grep("SocialIconsCounts", pinText)
    integers <- data.frame(x, y)
# finds the index numbers of the pairs of rows either sides of where the pin descriptions are, the "markers"

    pinNo <- grep("span class=\"value\"", pinText, value=TRUE)[1]
    pinNo <- gsub("[^0-9]", "", pinNo)
    if(as.integer(pinNo)!=nrow(integers)) {
        stop("Number of pins does not match amount of pin descriptions.")
    }
# tests to see if reported pin number on the website, matches the amount of description pairs

    centre <- vector("list", nrow(integers))
    for(x in 1:nrow(integers)) {
        y <- integers[x, 1]
        z <- integers[x, 2]
        a <- y + 1
        b <- z - 1
        centre[[x]] <- a:b
    }
# creates an empty list with spaces for every pin description
# the loop extracts the index numbers for all indexes in between the pairs of "markers"
# does this by +1 to lower number, -1 to higher number, then creating a sequence out of these 2
# these index numbers are then saved a vector to each place in the list

    intVec <- integer()
    for(x in 1:nrow(integers)) {
        intVec <- c(intVec, centre[[x]])
    }
# the loop extracts all of the values from the list into one very long integer vector.

    usefulText <- pinText[intVec]
    library(stringr)
    usefulText <- str_trim(usefulText)
# subsets the original txt file to extract the lines of text associated with the index numbers
# removes all of the whitespace with str_trim function from stringr package

    latest <- usefulText[grep("^[a-zA-Z]", usefulText)]
    latest <- paste0(latest, ",")
# selects only text, to remove extraneous symbols/html noise
# places a comma at the end of every element/pin description

    latest <- paste(latest, collapse = "")
    splitText <- strsplit(latest, ",")
    allTags <- str_trim(splitText[[1]])
    allTags <<- allTags
# collapses all of the elements into one giant character vector
# splits the vector into separate elements based on a comma, outputs it within a list
# subsets the list, extracting all of the elements into a vector. Removes any remaining whitespace

}

    ###########################

analyseTags <- function() {
    if(exists("allTags")==FALSE) {
        importTags()
    }
# function runs consecutively after the first one, taking the outputted data frame as argument
# if the first function hasn't been used, runs it automatically with default arguments

    tagsData <- data.frame(table(allTags))
    tagsData <- tagsData[order(tagsData$Freq, decreasing=TRUE), ]
    tagsData <- tagsData[grep("^[^from]", tagsData$allTags), ]
    tagsData <<- tagsData
# creates a data frame showing the frequency of unique elements (descriptions)
# also removes false caught data, where it stated: "from: examplesite.tumblr.com" etc

    colours <- c("black", "blue", "green", "white", "brown", "pale", "light", "red",
                 "yellow", "pink", "grey", "purple", "orange", "breton",
                 "check", "colourful", "colour")
# creates a vector that includes all of the colours I use in my tags

    colourFreq <- data.frame(colour_name=as.character(), colour_freq=as.integer())
    for(x in 1:length(colours)) {
        colour <- tagsData[grep(colours[x], tagsData$allTags), ]
        tempFrame <- data.frame(colour_name=as.character(colours[x]), colour_freq=sum(colour$Freq))
        colourFreq <- rbind(colourFreq, tempFrame)
    }
    colourFreq <- colourFreq[order(colourFreq$colour_freq, decreasing=TRUE), ]
    colourFreq <<- colourFreq
# loop which takes each colour from the colour vector, then searches for elements using that colour within the descriptions
# It then counts the amount of times that colour appears, saving it in a data frame alongside the colour name
# Data frame is added to for each colour, then ordered by frequency

   ############################

# Next, need to:
# Sort out the elements that use /
    slashTags <- tagsData[grep("/", tagsData$allTags), ]
# ((is split based on colour and clothing alternatively, which makes it difficult))

# Do more analysis and grouping, for example, based on tops/bottoms, clothing vs colour etc

}
