

source("amaz_helpers.R")

# Define UI for application 
#journal, sandstone, simplex, spacelab
#cerulean, cosmo, cyborg, darkly, flatly, journal, lumen, paper, readable, sandstone, simplex, slate, spacelab, superhero, united, yeti
ui<- fluidPage(  
   theme = shinytheme("journal"),
                 
# Application title
                headerPanel(""),
# ASIN ID
  sidebarLayout(
    sidebarPanel(
      HTML('</br>'),
      textInput("caption", ("Enter ASIN ID from Amazon.in"), "B07FBD8JBB", placeholder = "B07FBD8JBB"),
      actionButton("goButton", "Click to scrape !"
                    ,style = "width:100%;"
                   ),
      HTML('</br>'),
      HTML('</br>'),
      downloadButton('download', "Download" , style = "width:100%;" ),
      helpText(br(),paste("Download Customer Reviews for any product on 'Amazon.in'"),
               br()),
      
      # imageOutput("myGIF"),
# titlePanel(
#   title = tags$link(rel="icon",type="image/ico", href="www/icons8-amazon-480.ico"),"Find more from your customer's feedback"),
tags$head(
  tags$style(type = "text/css",HTML("th { text-align: center; }
                                    input {
                                    text-align: center;
                                    }
                                    
                                    <input type='Enter ASIN ID from Amazon' style='text-align:center;'/> 

                                    .box-header{background:#d2d2d2; color:#d83000; text-align:center;}

                                    ```{r message=FALSE, warning=FALSE, results='hide'}
                                    
                                    "))),width=2),


# main panel 
    
    mainPanel(
      tabsetPanel(type = "tabs", 
                  tabPanel("Reviews",
                           # helpText(paste("Select a product from Amazon.in website to download customer reviews & analyze it"),
                           #          br(),
                           #          br()),
                           HTML('</br>'),
                           (valueBoxOutput("productname",width = 12)),HTML('</br>'),
                           column(valueBoxOutput("price",width = 12),width=2),  HTML('</br>'),
                           column(valueBoxOutput("pages",width = 12),width=3),  
                           column(valueBoxOutput("averagerating_now",width = 12),width=3),
                           column(uiOutput("amazon_graphui",width = 12),width=4),
                           # HTML('</br>'),
                           # HTML('</br>'),
                           # imageOutput("myImage"),
                           dataTableOutput("view")%>% withSpinner(type = getOption("spinner.type", default = 8),size=1.5,color="#0dc5c1")
                          ),
               
                  tabPanel(
                    "Insightful Reviews",
                           HTML('</br>'),
                           helpText(paste("This tab allows you to calculate eight types of emotion present within the product review."),
                                    br(),
                                    br(),
                                    paste("The following types of emotion are calculated:"),
                                    br(),
                                    br(),
                                    tags$b(paste("Anger, Anticipation, Disgust, Fear, Joy, Sadness, Surprise, and Trust.")),
                                    br(),
                                    paste("The emotions calculated are the 8 basic universal emotions conveyed by humans in all cultures."),
                                    br(),
                                    paste("Each bar represents the overall percentage of each emotion present within the uploaded product review.")),
                                    HTML('</br>'),br(),
                           uiOutput("nrcsentiment"), HTML('</br>'),
                    # downloadButton(outputId = "downloadeight",label = "Download Emotional Sentiment Barplot"),
                           HTML('</br>'),
                           #Important graph 2
                    # uiOutput("nrcsentiment_2"), HTML('</br>'),
                    # downloadButton(outputId = "downloadeight_cloud",label = "Download Wordcloud"),
                           HTML('</br>')
                    # , HTML('</br>'), HTML('</br>')
                           # verbatimTextOutput("ins_reviews")
                           # textInput("text_summary", label = "Interpretation", value = "Enter text...")
                    )
      ), width=10                         
    ))
)

server<- function(input, output) {
  
  # output$myImage <- renderImage({
  #   # A temp file to save the output.
  #   # This file will be removed later by renderImage
  #   input$goButton
  #   #  if(input$Username== "nm01@gmail.com")({ 
  #   prod_code<- isolate(as.factor(input$caption))
  #   
  #   ImgNode <- doc %>% html_nodes("#cm_cr-product_info .product-image img")
  #   link <- html_attr(ImgNode, "src")
  #   download.file(url = link,destfile = "test.png", mode='wb')
  #   outfile <- tempfile(fileext = 'test.png')
  #   
  #   # Generate the PNG
  #   png(outfile, width = 400, height = 300)
  #   # hist(rnorm(input$obs), main = "Generated in renderImage()")
  #   # dev.off()
  #   
  #   # Return a list containing the filename
  #   list(src = outfile,
  #        contentType = 'image/png',
  #        width = 400,
  #        height = 300,
  #        alt = "This is alternate text")
  # }, deleteFile = TRUE)
  
 website<- reactive({
    input$goButton
    prod_code<- isolate(as.factor(input$caption))
    url <- paste0("https://www.amazon.in/dp/",prod_code,"/#customerReviews")
    doc <- read_html(url)
    return(doc)
  })
  
 scrape_pages<- reactive({
   website<- website()
   #No. of pages
   Product_page <-  html_nodes(website, "#acrCustomerReviewText") %>% html_text() %>% gsub("\n", "", .) %>% trim()%>%gsub(" customer reviews","",.)%>% gsub(",", "", .)
   count<- (gsub(" ratings",'',Product_page))
   count<-as.numeric(gsub("([0-9]+).*$", "\\1", count))
   count_1<- paste0(count," ratings")
   return(count_1)
})
 
 scrape_price<- reactive({
   website<-website()

   Product_page<-html_nodes(website, "#priceblock_dealprice")%>% html_text() %>% gsub("\n", "", .) %>% trim()
   Product_page_1<-html_nodes(website, "#priceblock_ourprice")%>% html_text() %>% gsub("\n", "", .) %>% trim()
   Product_page_2<-html_nodes(website, "#priceblock_saleprice")%>% html_text() %>% gsub("\n", "", .) %>% trim()
   
   if (identical(Product_page,character(0)) != T ) {
     conditional_price<-Product_page
   } else if (identical(Product_page_1,character(0)) != T) {
     conditional_price<-Product_page_1
   } else {
     conditional_price<-Product_page_2
   }
   
   # url <- paste0("https://www.amazon.in/dp/","B079HVX8D6")
   # website <- read_html(url)
   # price<- gsub("\u20b9",'',conditional_price) %>% gsub(".00", "" )%>%stri_replace_all_charclass( "\\p{WHITE_SPACE}", "")
   # price<-round(as.numeric(price),0)
    price<- gsub("\u20b9",'',conditional_price) %>%stri_replace_all_charclass( "\\p{WHITE_SPACE}", "")
   # price<-round(as.numeric(price),0)
 })
 
 scrape_product_name<- reactive({
   website<-website()
   #Name of product
   Product_name <- html_nodes(website, "#productTitle") %>% html_text() %>% gsub("\n", "", .) %>% trim()
   name<- as.character(Product_name)
 })
  
 
 scrape_rating<- reactive({
   website<-website()
   # prod_code<- "B07FBD8JBB"
   # url <- paste0("https://www.amazon.in/dp/",prod_code,"/#customerReviews")
   # website <- read_html(url)

   Product_page <- html_nodes(website, ".a-fixed-left-grid-col .a-row .a-size-medium") %>% html_text() %>% gsub("\n", "", .) %>% trim()
   Product_page_1<- paste0(Product_page," stars")
   # averagestar<- (gsub(" out of 5",'',Product_page))
   return(Product_page_1)
                         })
 
  output$pages <-shinydashboard::renderValueBox({
                 scrape_pages<-scrape_pages()
                 # shinydashboard::valueBox(paste0(emo::ji("thought")," ",scrape_pages),"",color="aqua",icon = icon(""))
                 shinydashboard::valueBox(paste0(scrape_pages)," ",color="aqua",icon = icon(""))
                                               })
  
  output$price <-shinydashboard::renderValueBox({
                 scrape_price<-scrape_price()
                 #stri_unescape_unicode('\u20b9'),
                 shinydashboard::valueBox(paste0(stri_unescape_unicode('\u20b9')," ",scrape_price),"  ",color="aqua")
                                                })
  
  output$productname <-shinydashboard::renderValueBox({
                       scrape_product_name<-scrape_product_name()
                       fluidRow(column(12,offset = 4,
                       shinydashboard::valueBox(paste0(scrape_product_name)," ",color="aqua",icon = icon(""),width=NULL)))
                                                     })
  
  output$averagerating_now <-shinydashboard::renderValueBox({
     # scrape_rating<- averagestar
    scrape_rating<-scrape_rating()
    scrape_rating< as.numeric(scrape_rating)
    shinydashboard::valueBox(paste0(scrape_rating)," ",color="aqua",icon = icon(""))
  })
  

  #Create graphics table
  
  myproduct <- reactive({
    website<-website()
    input$goButton
    prod_code<- isolate(as.factor(input$caption))
    # url <- paste0("https://www.amazon.in/dp/",prod_code)
    # doc <- read_html(url)
    
    # url <- paste0("https://www.amazon.in/dp/","B07FBD8JBB")
    # doc <- read_html(url)
    # website<-doc
    
    # Product_page <- html_nodes(website, "#acrCustomerReviewText") %>% html_text() %>% gsub("\n", "", .) %>% trim()%>%gsub(" customer reviews","",.)%>% gsub(",", "", .)
    # # pattern is by finding a set of numbers in the start and capturing them
    # pages<-as.numeric(gsub("([0-9]+).*$", "\\1", Product_page))/10
    # pages<- ceiling(pages)
    
    scrape_pages<- scrape_pages()
    count<- gsub(" ratings",'',scrape_pages)
    count<-as.numeric(gsub("([0-9]+).*$", "\\1", count))/10
    pages<- ceiling(count)
    ######################################## Reviews loop ############################################
     # pages<-1
    reviews_all <- NULL
    for(page_num in 1:pages){
      
      url <- paste0("http://www.amazon.in/product-reviews/",prod_code,"/?pageNumber=", page_num)
      doc <- read_html(url)
      
      # url <- paste0("http://www.amazon.in/product-reviews/","B076BPPXT1","/?pageNumber=", page_num)
      # doc <- read_html(url)
      
      #B07L6J4ZRR ; B076BPPXT1
      # url <- paste0("http://www.amazon.in/product-reviews/","B07L6J4ZRR")
      # doc <- read_html(url)
      
      # a-size-base a-link-normal review-title a-color-base review-title-content a-text-bold
      
      Reviews <- doc %>%  html_nodes(".a-row .review-title") %>%  html_text() %>% trim
      # Reviews
      
      Star_rating <- doc %>%  html_nodes('.a-fixed-right-grid-col .review-rating .a-icon-alt') %>%  html_text()
      # .a-row .a-link-normal .a-icon-alt
      
      Review_Date <- doc %>%  html_nodes(".a-fixed-right-grid-col .review-date") %>%  html_text()
      
      reviews_all <- rbind(reviews_all, cbind( Reviews,Star_rating,Review_Date))
    }
    
    df_reviews_All<- data.frame(reviews_all)
    
    
    # Remove duplicate rows of the dataframe using cyl and vs variables
    # df_reviews_All<- distinct(df_reviews_All, Reviews, .keep_all= T)
    
    count<-  nrow(df_reviews_All$Reviews)
    
    #Removing customized stopwords 1
    words_to_remove <- c('Reviewed in India on ' )
    
    remove_stopwords_1 <- vapply(words_to_remove, 
                                 function(x) gsub(x, '', df_reviews_All$Review_Date), 
                                 character(length(df_reviews_All$Review_Date)))
    
#Removing customized stopwords 2
    words_to_remove <-    c('.0 out of 5 stars' )
    remove_stopwords_2 <- vapply(words_to_remove, 
                                 function(x) gsub(x, '', df_reviews_All$Star_rating), 
                                 character(length(df_reviews_All$Star_rating)))
    
#Final task
    df_reviews_All$Review_Date<-NULL
    df_reviews_All$Star_rating<-NULL
    reviews_star_date_final<- cbind(df_reviews_All,remove_stopwords_2,remove_stopwords_1)
    names(reviews_star_date_final)[1] <- "Customer_reviews"
    names(reviews_star_date_final)[2] <- "Star_rating"
    names(reviews_star_date_final)[3] <- "Review_date"
    reviews_star_date_final<- data.frame(reviews_star_date_final)
    return(reviews_star_date_final)
                            })
  
  output$amazon_graph<- renderPlotly({
    withProgress(message = 'Scraping Reviews',
                 value = 0, {
                   for (i in 1:3) {
                     incProgress(1/3)
                     Sys.sleep(0.25)
                   }
                 }
                 ,env = parent.frame(n=1))
    myproduct<- myproduct()
    myproduct<-table(myproduct$Star_rating)
    myproduct<-data.frame(t(t(myproduct)))%>% select(Var1, Freq)
    myproduct$Var1<- paste0(myproduct$Var1," star ")
    p1 <- plot_ly(x = ~myproduct$Freq, y = ~reorder(myproduct$Var1, myproduct$Var1), name = 'Household',
                  type = 'bar', orientation = 'h',
                  marker = list(color = '#FFBA00',
                                line = list(color = '', width = 1)) ) %>%
      layout(yaxis = list(showgrid = FALSE, 
                          showline = FALSE,
                          domain= c(0, 0.85),
                          showticklabels = T,
                          tickangle=0, title = "\r\n "),
             xaxis = list(title = "",
                          zeroline = FALSE,
                          showline = FALSE,
                          showticklabels = FALSE,
                          showgrid = FALSE)) %>%
      add_annotations(xref = 'myproduct$Freq', yref = 'myproduct$Var1',
                       x = myproduct$Freq * 1-0.4,
                       y = myproduct$Var1,
                      text = paste(round((myproduct$Freq/sum(myproduct$Freq))*100,0), '%'),
                      font = list(family = 'Arial', size = 12, color = '#337cff'),
                      showarrow = FALSE)
    
    
  })
  

#Data view 
  output$view <- renderDataTable({
    withProgress(message = 'Scraping Reviews',
                 value = 0, {
                   for (i in 1:3) {
                     incProgress(1/3)
                     Sys.sleep(0.25)
                   }
                 }
                 ,env = parent.frame(n=1))
                                
                                  myproduct<- myproduct()
                      
                                  as.datatable(formattable(myproduct, align =c("l","c","c"),
                                              list(Star_rating = color_bar("#FFBA00"))),filter = 'top',selection="multiple", escape=FALSE, 
                                              options = list(sDom  = '<"top">lrt<"bottom">ip',pageLength = 10, lengthChange = FALSE))
                                  
                                  })
  
  
  output$download <- downloadHandler(
    withProgress(message = 'Scraping Reviews',
                 value = 0, {
                   for (i in 1:3) {
                     incProgress(1/3)
                     Sys.sleep(0.25)
                   }
                 }
                 ,env = parent.frame(n=1)),
                                     filename = function(){"Download.csv"}, 
                                     content = function(fname){
                                     write.csv(myproduct(), fname, row.names=F)
                                                              }
                                    )
  
  output$amazon_graphui<-renderUI({
    withProgress(message = 'Scraping Reviews',
                 value = 0, {
                   for (i in 1:3) {
                     incProgress(1/3)
                     Sys.sleep(0.25)
                   }
                 }
                 ,env = parent.frame(n=1))
    # if(USER$Logged==TRUE)
    # {box(width=12,
         plotlyOutput("amazon_graph", height=180)%>% withSpinner(color="#0dc5c1")
         # ,solidHeader = F,status = "info")}
  })
  
  
  output$nrcsentiment<-renderUI({
    # if(USER$Logged==TRUE)
    # {box(width=12,
         plotlyOutput("nrcsentiment_graph", height=400)%>% withSpinner(color="#0dc5c1")
         # solidHeader = F,status = "info")}
  })     
  
  output$nrcsentiment_2<-renderUI({
    # if(USER$Logged==TRUE)
     {box(width=12,
    plotOutput("nrcsentiment_graph_2", height=800 )%>% withSpinner(color="#0dc5c1"),
    solidHeader = F,status = "info")}
  })  
  
  # NRC Sentiment graph
 output$nrcsentiment_graph<-renderPlotly({
    withProgress(message = 'Creating BarPlot',
                 value = 0, {
                   for (i in 1:3) {
                     incProgress(1/3)
                     Sys.sleep(0.25)
                   }
                 }
                 ,env = parent.frame(n=1))
    #Graph code
    scrape_product_name<- scrape_product_name()
    data<-myproduct()
    data<- data%>% select(Customer_reviews)
    # CLEANING TWEETS
    data$Customer_reviews=gsub("&amp", "", data$Customer_reviews)
    data$Customer_reviews = gsub("&amp", "", data$Customer_reviews)
    data$Customer_reviews = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", data$Customer_reviews)
    data$Customer_reviews = gsub("@\\w+", "", data$Customer_reviews)
    data$Customer_reviews = gsub("[[:punct:]]", "", data$Customer_reviews)
    data$Customer_reviews = gsub("[[:digit:]]", "", data$Customer_reviews)
    data$Customer_reviews = gsub("http\\w+", "", data$Customer_reviews)
    data$Customer_reviews = gsub("[ \t]{2,}", "", data$Customer_reviews)
    data$Customer_reviews = gsub("^\\s+|\\s+$", "", data$Customer_reviews)
    
    data$Customer_reviews <- iconv(data$Customer_reviews, "UTF-8", "ASCII", sub="")
    
    
    
    # Emotions for each tweet using NRC dictionary
    emotions <- get_nrc_sentiment(data$Customer_reviews)
    emo_bar = colSums(emotions)
    emo_sum = data.frame(count=emo_bar, emotion=names(emo_bar))
    emo_sum$emotion = factor(emo_sum$emotion, levels=emo_sum$emotion[order(emo_sum$count, decreasing = TRUE)])
    
    
    # Visualize the emotions from NRC sentiments
    p <- plot_ly(emo_sum, x=~emotion, y=~count, type="bar", color=~emotion, hoverinfo="text", text= ~count) %>%
         layout(xaxis=list(title="Emotional Sentiment Barplot"), yaxis = list(title = "Word count"),showlegend=FALSE,
         title=paste0(scrape_product_name
                      # HTML('</br>'),HTML('</br>') 
                      ))
    
  
  })
  
  
  # NRC Sentiment graph 2- WORDCLOUD
 output$nrcsentiment_graph_2<-renderPlot({
    scrape_product_name<- scrape_product_name()
    data<-myproduct()
    ####
    data<- data%>% select(Customer_reviews)
    # CLEANING TWEETS
    data$Customer_reviews=gsub("&amp", "", data$Customer_reviews)
    data$Customer_reviews = gsub("&amp", "", data$Customer_reviews)
    data$Customer_reviews = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", data$Customer_reviews)
    data$Customer_reviews = gsub("@\\w+", "", data$Customer_reviews)
    data$Customer_reviews = gsub("[[:punct:]]", "", data$Customer_reviews)
    data$Customer_reviews = gsub("[[:digit:]]", "", data$Customer_reviews)
    data$Customer_reviews = gsub("http\\w+", "", data$Customer_reviews)
    data$Customer_reviews = gsub("[ \t]{2,}", "", data$Customer_reviews)
    data$Customer_reviews = gsub("^\\s+|\\s+$", "", data$Customer_reviews)
    
    data$Customer_reviews <- iconv(data$Customer_reviews, "UTF-8", "ASCII", sub="")
    
    
    
    # Emotions for each tweet using NRC dictionary
    emotions <- get_nrc_sentiment(data$Customer_reviews)
    
    ####
    
    wordcloud_tweet = c(
      paste(data$Customer_reviews[emotions$anger > 0], collapse=" "),
      paste(data$Customer_reviews[emotions$anticipation > 0], collapse=" "),
      paste(data$Customer_reviews[emotions$disgust > 0], collapse=" "),
      paste(data$Customer_reviews[emotions$fear > 0], collapse=" "),
      paste(data$Customer_reviews[emotions$joy > 0], collapse=" "),
      paste(data$Customer_reviews[emotions$sadness > 0], collapse=" "),
      paste(data$Customer_reviews[emotions$surprise > 0], collapse=" "),
      paste(data$Customer_reviews[emotions$trust > 0], collapse=" ")
    )
    
    # create corpus
    corpus = Corpus(VectorSource(wordcloud_tweet))
    
    # remove punctuation, convert every word in lower case and remove stop words
    
    corpus = tm_map(corpus, tolower)
    corpus = tm_map(corpus, removePunctuation)
    corpus = tm_map(corpus, removeWords, c(stopwords("english")))
    corpus = tm_map(corpus, stemDocument)
    
    # create document term matrix
    
    tdm = TermDocumentMatrix(corpus)
    
    # convert as matrix
    tdm = as.matrix(tdm)
    tdmnew <- tdm[nchar(rownames(tdm)) < 11,]

    # column name binding
    colnames(tdm) = c('anger', 'anticipation', 'disgust', 'fear', 'joy', 'sadness', 'surprise', 'trust')
    colnames(tdmnew) <- colnames(tdm)
    comparison.cloud(tdmnew, random.order=FALSE,
                      colors = c("#00B2FF", "red", "#FF0099", "#6600CC", "green", "orange", "blue", "brown"),
                     title.size=2, max.words=150, match.colors = T)
    
    # commonality.cloud(tdm, random.order=FALSE, scale=c(5, .5),colors = brewer.pal(4, "Dark2"), max.words=400)
    
  
  })
  
#### Save Barplot ######
output$downloadeight <- downloadHandler(
  filename = function() { paste("Download Emotional Sentiment Barplot",'png',sep = ".") },
  content = function(file) {
    # if(input$download6=="png")
    #   png(file)
    # else if (input$download6=="jpeg")
    #   jpeg(file)
    # else if (input$download6=="bmp")
    #   bmp(file)
    # else if (input$download6=="pdf")
    #   pdf(file)
    withProgress(message = 'Downloading Emotional Sentiment Barplot',
                 value = 0, {
                   for (i in 1:3) {
                     incProgress(1/3)
                     Sys.sleep(0.25)
                   }
                 },env = parent.frame(n=1))
    ggsave(file,nrcsentiment_graph())})
  
# ##### Save Wordcloud ######  
#   output$downloadeight_cloud <- downloadHandler(
#     filename = function() { paste("Download Wordcloud",'png',sep = ".") },
#     content = function(file) {
#       # if(input$download6=="png")
#       #   png(file)
#       # else if (input$download6=="jpeg")
#       #   jpeg(file)
#       # else if (input$download6=="bmp")
#       #   bmp(file)
#       # else if (input$download6=="pdf")
#       #   pdf(file)
#       withProgress(message = 'Downloading Wordcloud',
#                    value = 0, {
#                      for (i in 1:3) {
#                        incProgress(1/3)
#                        Sys.sleep(0.25)
#                      }
#                    },env = parent.frame(n=1))
#       ggsave(file,nrcsentiment_graph_2())})
  
  
  ## Download code for wordcloud picture download ####
  
  output$downloadeight_cloud <- downloadHandler(
    filename = function() { paste("WordCloud.png") },
    content = function(file) {
      # if(input$download3=="png")
      #   png(file)
      # else if (input$download3=="jpeg")
      #   jpeg(file)
      # else if (input$download3=="bmp")
      #   bmp(file)
      # else if (input$download3=="pdf")
      #   pdf(file)
      ggsave(nrcsentiment_graph_2)
      # dev.off()
    })
 
 
 output$myGIF <- renderImage({
   list(src = "www/Amazon_truck.gif",
        contentType = 'image/gif',
         width = 180,
         height = 150
        # alt = "This is alternate text"
   )}, deleteFile = FALSE)
 
#Downloading image
# ImgNode <- doc %>% html_nodes("#cm_cr-product_info .product-image img")
# link <- html_attr(ImgNode, "src")
}

shinyApp(ui, server)