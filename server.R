#
# Altmetric data viz with a Shiny app
#

library(shiny)
library(rCharts)
library(reshape)

# Change NA to 0
# aalto_all_m[is.na(aalto_all_m)] <- 0

# Altmetric.com_score value to ceiling (is this way on their web page)
aalto_all_m$Altmetric.com_score <- as.integer(ceiling(aalto_all_m$Altmetric.com_score))

# For some reason, the data type of cols from GooglePlus onwards become not integer in import (perhaps because of NA's). So here, again
# http://stackoverflow.com/questions/3796266/change-the-class-of-many-columns-in-a-data-frame
columns <- c(which(colnames(aalto_all_m) == "GooglePlus"):ncol(aalto_all_m))
aalto_all_m[ ,columns] <- apply(aalto_all_m[ ,columns], 2, function(x) as.integer(as.character(x)))

# Reorder
aalto_all_m <- aalto_all_m[c("id", "DOI", "URL", "Readers_count", "Accounts",
                             "Altmetric.com_score", "Any_type_of_posts", "Blog_posts", "CiteULike",
                             "Connotea", "CrossRef", "Delicious_bookmarks", "Facebook", "Figshare", "F1000", "GooglePlus", "Mendeley", "Nature",
                             "PLOSalm_html_views", "PLOSalm_pdf_views", "PLOSalm_scopus", "PubMed",
                             "Science_news_outlets", "Topsy_tweets", "Twitter", "WoS", "Videos",
                             "Altmetric.com_URL", "Year")]

#aalto_all_m <- aalto_all_m[c("id", "DOI", "URL", "Readers_count", "Accounts",
#                             "Altmetric.com_score", "Any_type_of_posts", "Blog_posts", "Bloglines", "CiteULike",
#                             "Connotea", "CrossRef", "Delicious_bookmarks", "Facebook", "Figshare", "F1000", "GooglePlus", "Mendeley", "Nature",
#                             "PLOSalm_html_views", "PLOSalm_pdf_views", "PLOSalm_scopus", "Postgenomic", "PubMed",
#                             "Science_news_outlets", "ScienceSeeker", "Topsy_tweets", "Twitter", "WoS", "Videos",
#                             "Altmetric.com_URL", "Year")]


shinyServer(function(input, output, session) {
  
  # Render a group of checkboxes with the value selected that has the value of input$m, and then some
##  output$checkbox <- renderUI({
#    
#    metrics <- allmetrics
#    
#    checkboxGroupInput(inputId = "vars",
#                       label = "Choose which other metrics to show:",
#                       choices = metrics,
#                       selected = c(names(metrics[grep(input$m, metrics)]),
#                                    "Altmetric score (ALT)"))
#  })
  
 
  # The user picks up one metrics, and based on that, we sort the data frame in descending order, and show the Top10
  tableValues <- reactive({
    
    cols <- head(aalto_all_m[order(aalto_all_m[[input$m]], decreasing = TRUE), ], n = 10)
    # Show only those cols that are included in the variable vars, plus URL and Year
    tb <- cols[, c(names(cols) %in% input$vars)]
    tb$Altmetric.com_URL <- cols$Altmetric.com_URL
    tb$Year <- cols$Year 
#    tb$Widget <- cols$DOI
    tb  
    
  
  })
  
  
  output$caption <- renderText({
    
    "ALT = Altmetric.com | IS = ImpactStory | PLoS = PLoS ALM"
    
  })
 
  
  
  output$table <- renderTable({   
    
    tb <- tableValues()
    # Make an URL of the details_url column, and color+bold the Top10 metrics chosen
    # About HTML rendering: http://stackoverflow.com/questions/19019709/r-shiny-table-not-rendering-html
    for (i in 1:nrow(tb)) {
      url <- substr(tb$Altmetric.com_URL[i], 47, nchar(tb$Altmetric.com_URL[i]))
      doUrl <- paste("<a href=", tb[i, c("Altmetric.com_URL")], "\" target=\"_blank\">", url, "</a>")
      doBold <- paste("<b><font color=\"red\">", tb[i, c(input$m)], "</font></b>")
 #     doBadge <- paste("<div class=\"impactstory-embed impactstory-small impactstory-tag impactstory-color\" data-show-logo=\"false\" data-badge-size=\"small\" data-id=\"", 
 #                      tb[i, c("Widget")], "\" data-id-type=\"doi\" data-api-key=\"sonkkila-aarznq\"></div>", sep = "")
      tb[i, c("Altmetric.com_URL")] <- doUrl
      tb[i, c(input$m)] <- doBold
 #     tb[i, c("Widget")] <- doBadge
    }        
    tb
      
  }, include.rownames = FALSE, sanitize.text.function = function(s) s)

# sub("Widget", "<script type=\"text/javascript\" src=\"http://impactstory.org/embed/v1/impactstory.js\"></script>", s))
  
  
  
  
  # Plot a chart based on the values in the table
  output$chart <- renderChart({
    
    myData <- tableValues()
    myData[is.na(myData)] <- 0
    # Drop two cols
    myData2 <- as.data.frame(lapply(myData[ ,!colnames(myData) %in% c("Year", "Altmetric.com_URL")], as.integer, stringsAsFactors = FALSE))
    
    # t <- "http://www.altmetric.com/details.php?citation_"
    # nchar(t) # =46
    # Make an id string from the rest. Needs to be modified if the URL changes
    myData2$id <- substr(myData$Altmetric.com_URL, 47, nchar(myData$Altmetric.com_URL))
      
    # Transform data for plotting
    myDataM <- melt(myData2, id.vars="id")
    p2 <- nPlot(value ~ id, data = myDataM, group = "variable", type = "multiBarChart")
    p2$chart(reduceXTicks = FALSE)
    p2$xAxis(staggerLabels = TRUE)
    p2$set(dom="chart")
  
    return(p2)
    
  })
  
})

