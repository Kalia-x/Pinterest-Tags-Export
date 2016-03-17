
# message("The function *tags* takes four arguments: 1st your pinterest login, 2nd your pinterest password, 3rd the URL of the pinterest board, 4th the name you will give to your data file, ending .txt.")
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

# Open browser (chrome is best), navigate to board (while logged in). Zoom out as far as you can. Make window as wide as possible.
# If you can see ALL of your pinned items at once, that is good.
# Right click, save page. File name: "pins.txt". File type: "webpage, complete"
# The files folder can be deleted, the .txt file should be placed in current working directory.
# File should begin: <!DOCTYPE html><!-- saved from url=(0040)https://uk.pinterest.com/kalia_/clothes/ -->

    pinText <- readLines("pins.txt")
    x <- grep("pinDescription", pinText)
    y <- grep("SocialIconsCounts", pinText)

    integers <- data.frame(x, y)
    centre <- vector("list", 253)

    for(x in 1:253) {
        y <- integers[x, 1]
        z <- integers[x, 2]
        a <- y + 1
        b <- z - 1
        centre[[x]] <- a:b
    }

    intVec <- integer()
    for(x in 1:253) {
        intVec <- c(intVec, centre[[x]])
    }

    usefulText <- pinText[intVec]
    library(stringr)
    usefulText <- str_trim(usefulText)

    ints <- grep("^[a-zA-Z]", usefulText)
    latest <- usefulText[ints]
    latest <- paste0(latest, ",")

    latest <- paste(latest, collapse = "")
    splitText <- strsplit(latest, ",")
    allTags <- trim(splitText[[1]])

    ###########################

    tagsData <- data.frame(table(allTags))
    tagsData <- tagsData[order(tagsData$Freq, decreasing=TRUE), ]
    tagsData <- tagsData[grep("^[^from]", tagsData$allTags), ]

    colours <- c("black", "blue", "green", "white", "brown", "pale", "light", "red",
                 "yellow", "pink", "grey", "purple", "orange", "breton",
                 "check", "colourful", "colour")
    options(stringsAsFactors = FALSE)
    colourFreq <- data.frame(colour_name=as.character(), colour_freq=as.integer())
    for(x in 1:length(colours)) {
        colour <- tagsData[grep(colours[x], tagsData$allTags), ]
        tempFrame <- data.frame(colour_name=as.character(colours[x]), colour_freq=sum(colour$Freq))
        colourFreq <- rbind(colourFreq, tempFrame)
    }
    colourFreq <- colourFreq[order(colourFreq$colour_freq, decreasing=TRUE), ]
