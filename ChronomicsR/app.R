library(shiny)
#load dataset
cardiacdrugs <- read.table("data/cardiacdrugs.tsv", sep = "\t", header = TRUE)


# Use a fluid Bootstrap layout
ui <- fluidPage(    
  
  # Give the page a title
  titlePanel(
    h2(img(src = "logo.png", height = 50), strong("COMPARATIVE CHRONOMICS"), style = "font-family: 'Playfair Display SC','Times New Roman',Times,serif;"),
    windowTitle = "chronomics.shinyapps.io"),
  
  # Generate a row with a sidebar
  sidebarLayout(      
    
    # Define the sidebar with one input
    sidebarPanel(
      
      tags$head(
        tags$style("body {background-color: lightgrey; }")
      ),
      
     # SET DROP DOWN FOR DRUG TO PLOT
      
     h3("Choose your plot"),
      htmlOutput("drug_selector"),  
      htmlOutput("gene_selector"),
   
     hr(),
     h3("Funding"),
     p("This data visualisation was generated through the Cambridge Genomics Cross Cutting theme"),
     img(src = "medgen.png", height = 80),
     hr(),
     h3("Acknowledgement"),
     p("Please cite the pre-print:"),
     em("Seed LM, Hearn TJ, (2020) A systematic review of utilisation of diurnal timing information in clinical trial design for Long QT syndrome.
medRxiv 2020.07.23.20160978")
     
     
      ),
    # Create a spot for the plot
    mainPanel( align="center",
      h3(strong("DRUG TIMING IN CARDIAC CLINICAL TRIALS"), align = "center", style = "font-family: 'Playfair Display SC','Times New Roman',Times,serif;"),
      
     plotOutput("drugPlot"),
     
p("This tool is intended to help trial designers consider the best time of day to dose drugs. To use simply select a drug and choose one of its cycling target genes. 
       The graph displays the diurnal oscillation in human QTc interval, and overlays a sine curve representing the gene expression of your drug target. You can download your graph by clicking on the link below. Oscillations are based on the predictions in", em("Ruben MD et al., (2018) Sci Transl Med. 10(458):eaat8806. PMID: 30209245.")),
     
     hr(),
     downloadButton("downloadData", "Download"),
p(br(),strong("Disclaimer:"), "This tool is not a validated clinical decision aid. This information is provided without any assurance that it is accurate and we assume no responsibility for any aspect of clinical decision making taken or administered with the aid of this information. Any reliance placed on this information is strictly at the user's own risk. ")
     
    )
    
  )
)


# Define a server for the Shiny app
server <- function(input, output) {
  
  output$drug_selector = renderUI({
    # set drug for searching genes

  available_drugs <- cardiacdrugs$drug

    selectInput("drug", label = "Start typing your drug here", 
                choices = unique(available_drugs),
                  selected = "Valproic Acid")
  })
  
  output$gene_selector = renderUI({ 
    #SEARCH FOR GENES for variable plot
    
    available_genes <- cardiacdrugs[cardiacdrugs$drug == input$drug,1]
    
    selectInput("GOI", label = "Select gene of interest", 
                choices = unique(available_genes),
                selected = "ABAT")
  })    
  
    # Fill in the spot we created for a plot
  output$drugPlot <- renderPlot({
  
   # Render a plot

t = seq(1,24,1)
w = (2*pi)/24
y = 10*sin(w*t)+410

par(bg = "light grey")
plot(t,y, type = "l", xlim=c(0,24), xaxs="i", xaxt="n", xlab="Time (h)", ylab="QTc interval (ms)", cex.lab = 1.5, cex.axis = 1.5)
xtick<-seq(0, 24, by=8)
axis(side=1, at=xtick, labels = FALSE)
text(x=xtick, par("usr")[3], labels = xtick, pos = 1, offset = 1, xpd = TRUE, cex = 1.5)

## could put secondary axis in with this, but would need to add right space: y2tick<-seq(0,1, by=0.25)
## axis(side = 4, at = y2tick)

DD <- seq(0,6, by = 0.2)
LL <- seq(6,18, by = 0.2)
DD2 <- seq(18,24, by = 0.2)

abline(v = DD, col = "grey40", lwd = 10)
abline(v = LL, col = "goldenrod1", lwd = 10)
abline(v = DD2, col = "grey40", lwd = 10)
lines(t,y, type = "l", lwd = 2, col = "white")
box()

DRUG = input$GOI
C = cardiacdrugs[cardiacdrugs$drug == input$drug & cardiacdrugs$cycler_symb == input$GOI,6]
C= (C-(pi))
C = 10*sin(w*t-C)+410

lines(t,C, type = "l", lwd = 2, col = "cyan3")

## add legend
legend("bottomright", c("QTc", DRUG), col=c("white", "cyan3"), lty=c(1,1), lwd = 2, cex = 1.5,
       inset=c(0,1), xpd=TRUE, horiz=TRUE, bty="n"
)


## Now let's add a line to indicate the peak of our gene and add a dot to indicate this on the sine curve ##

peak <- max(C)
peak.x <- match(peak, C)
abline(v = peak.x, col = "cyan3", lty = 3, lwd = 2)
##points(peak.x, peak, col="cyan3", cex = 1.5, lwd = 2, pch = 19)
text(peak.x, 410, labels = DRUG, pos = 4, col="cyan3", cex = 1.5)

## Now finally let's add a label to the bottom to indicate what the x intercept is
ZT.time <- paste(peak.x,":00")
## text(x=peak.x, par("usr")[3], labels = ZT.time, pos = 1, xpd = TRUE, col="cyan3", cex = 1.5)
text(peak.x, 408, labels = ZT.time, pos = 4, col="cyan3", cex = 1.5)

## Here is your nice graph ##
    
    }, height = 400, width = 600)

  #define my download
  output$downloadData <- downloadHandler(
    filename = "my_graphs.png",
    content = function(file) {
 png(file, height = 400, width = 600)

## same as above but for download ##            

      t = seq(1,24,1)
      w = (2*pi)/24
      y = 10*sin(w*t)+410
      plot(t,y, type = "l", xlim=c(0,24), xaxs="i", xaxt="n", xlab="Time (h)", ylab="QTc interval (ms)", cex.lab = 1.5, cex.axis = 1.5)
      xtick<-seq(0, 24, by=8)
      axis(side=1, at=xtick, labels = FALSE)
      text(x=xtick, par("usr")[3], labels = xtick, pos = 1, offset = 1, xpd = TRUE, cex = 1.5)
      DD <- seq(0,8, by = 0.2)
      LL <- seq(8,19.8, by = 0.2)
      DD2 <- seq(20,24, by = 0.2)
      abline(v = DD, col = "grey40", lwd = 10)
      abline(v = LL, col = "goldenrod1", lwd = 10)
      abline(v = DD2, col = "grey40", lwd = 10)
      lines(t,y, type = "l", lwd = 2, col = "white")
      box()
      DRUG = input$GOI
      C = cardiacdrugs[cardiacdrugs$drug == input$drug & cardiacdrugs$cycler_symb == input$GOI,6]
      C = 10*sin(w*t-C)+410
      lines(t,C, type = "l", lwd = 2, col = "cyan3")
      peak <- max(C)
      peak.x <- match(peak, C)
      abline(v = peak.x, col = "cyan3", lty = 3, lwd = 2)
      text(peak.x, 410, labels = DRUG, pos = 4, col="cyan3", cex = 1.5)
      ZT.time <- paste(peak.x,":00")
      text(peak.x, 408, labels = ZT.time, pos = 4, col="cyan3", cex = 1.5)
    
      dev.off()
     
    },
 contentType = 'image/png'
  )

  
}
# Run the app ----
shinyApp(ui = ui, server = server)
