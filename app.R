# K99 to R00 Sankey Network App ver. 1.0
# By By Christopher Solis, PhD 2023
# Florida State University 
# This work was supported by Florida State University and the National Institutes of Health grant R00 HL151825
# Citation: Woitowich Nicole C, Hengel Sarah R, Vilgalys Tauras P, Babdor Joël, Tyrrell Daniel J (2023) Analysis of NIH K99/R00 Awards and the Career Progression of Awardees eLife 12:RP88984. https://doi.org/10.7554/eLife.88984.1

# Load the necessary libraries
library(shiny)
library(data.table)
library(networkD3)
library(markdown)
library(jsonlite)

# Define the UI
ui <- fluidPage(
  titlePanel("K99 to R00 Sankey Network App ver. 1.0"),
  sidebarLayout(
    sidebarPanel(
      selectInput("source_col", "Source Column", "Organization_Name_K99"),
      selectInput("target_col", "Target Column", "Organization_Name_R00"),
      selectInput("source_color_col", "Source Color Column", "Top_25_NIH_funding_K99"),
      selectInput("target_color_col", "Target Color Column", "Top_25_NIH_funding_R00"),
      selectInput("state", "Choose between sorting options:",
                  list("Sort by link size", "Sort by source size", "Sort by target size")
                  ),
      bookmarkButton(),
      HTML('<p style="font-size: 8px;">By Christopher Solis, PhD 2023  <br> 
      This work is available under a GNU GPLv3 License</p>')
    ),
    mainPanel(
      fluidRow(
        tags$div(style = "padding:1em;",
                 markdown("#### Background

            This Sankey diagram tool is related to Figure 3 from Woitowich et al.
            2023 Analysis of NIH K99/R00 Awards and the Career Progression of 
            Awardees eLife 12: RP88984. In this work, the records from all the
            sucessful K99 to R00 awards from 2007 to 2022 are presented. While 
            most of the database entries were made available in this version, 
            users must pay caution when interepting the meaning of the link's 
            color as links from the same node are colored based on the color of 
            the node itself.
            
            #### Instructions
            
            1. Select a source variable (e.g. Organization_Name_K99).
            2. Select a target variable (e.g. Organization_Name_R00).
            3. Select a color palette for the source (e.g. Top_25_NIH_funding_K99).
            4. Select a color palette for the target (e.g. Top_25_NIH_funding_R00).
            4. Select a sorting method (e.g. Sort by source size).
            5. Select Bookmark to keep this visaulization.
            
            #### Disclaimer
            
            This work was supported by the National The opinions expressed in this article are the author's own and do 
            not reflect the view of the National Institutes of Health (NIH), 
            the Department of Health and Human Services (DHHS), or the 
            United States government.
            
            #### Acknowledgements
            
            We thank Chris Pickett for his valuable input when preparing this 
            publication. T.V. is supported by NIH F32-GM140568. D.J.T. is 
            supported by NIH R00-AG068309. S.R.H is supported by NIH 
            K99-ES033738. C.S. is supported by NIH R00-HL151825.
            ")
                 
            
        )
      ),
      fluidRow(
        sankeyNetworkOutput("sankeyPlot", height = 5000, width = 1000)
      )
    )
  )
)

# Define the server logic
server <- function(input, output, session) {
  # Read the CSV file from the 'data' folder
  csv_path <- "data/All_K99_R00_matched_Sankey_updated.csv" # Update with your actual file name
  sk_dataset <- fread(csv_path)
  col_names <- colnames(sk_dataset)
  updateSelectInput(session, "source_col", choices = col_names)
  updateSelectInput(session, "target_col", choices = col_names)
  updateSelectInput(session, "source_color_col", choices = col_names)
  updateSelectInput(session, "target_color_col", choices = col_names)
  #source_col <-"Organization_Name_K99" # for debugging
  #target_col <-"Organization_Name_R00" # for debugging
  
  
  
  output$sankeyPlot <- renderSankeyNetwork({
    
    source_col <- input$source_col
    target_col <- input$target_col
    source_color_col <- input$source_color_col
    target_color_col <- input$target_color_col
    SortingOutput <- input$state

    # Function SortByLink: Sort by link size (i.e., N)
    SortByLink <- function(sk_dataset,source_col,target_col) {
      t1 <- sk_dataset[, .N, by = c(source_col, target_col)]
      t1 <- t1[order(t1$N, decreasing = TRUE), ]
    }
    
    # Function SortBySource: Sort by source node size (i.e., source_col)
    SortBySource <- function(sk_dataset,source_col,target_col) {
      t1 <- sk_dataset[, .N, by = c(source_col, target_col)]
      t1 <- t1[order(t1$N, decreasing = TRUE), ]
      t2 <- t1[, .(Sum_N = sum(N)), by = source_col][order(-Sum_N), ][, Sum_N := NULL]
      t1 <- t1[order(match(t1[[source_col]], t2[[source_col]])), ]
    }
    
    # Function SortByTarget: Sort by target node size (i.e., target_col)
    SortByTarget <- function(sk_dataset,source_col,target_col) {
      t1 <- sk_dataset[, .N, by = c(source_col, target_col)]
      t1 <- t1[order(t1$N, decreasing = TRUE), ]
      t2 <- t1[, .(Sum_N = sum(N)), by = target_col][order(-Sum_N), ][, Sum_N := NULL]
      t1 <- t1[order(match(t1[[target_col]], t2[[target_col]])), ] 
    }
    
    # t1 <- SortByLink(sk_dataset,source_col,target_col) # For debugging
    # t1 <- SortBySource(sk_dataset,source_col,target_col) # For debugging
    # t1 <- SortByTarget(sk_dataset,source_col,target_col) # For debugging
    
    # Function SelectSort: Switch between sorting functions depending on input 
    SelectSort <- function(sk_dataset,source_col,target_col, type) {
      switch(type,
             "Sort by link size" = SortByLink(sk_dataset,source_col,target_col),
             "Sort by source size" = SortBySource(sk_dataset,source_col,target_col),
             "Sort by target size" = SortByTarget(sk_dataset,source_col,target_col))
    }
    
    t1 <- SelectSort(sk_dataset,source_col,target_col,SortingOutput)
    
    node_names <- unique(c(as.character(t1[[target_col]]), as.character(t1[[source_col]])))
    nodes <- data.frame(name = node_names)
    #nodes$name <- paste(seq_len(nrow(nodes)), ". ", nodes$name)
    
    links <- data.frame(
      source = match(t1[[source_col]], node_names) - 1,
      target = match(t1[[target_col]], node_names) - 1,
      value = t1$N
    )
    # Implement chosen sorting method
    links <- links[order(links$value, decreasing = TRUE), ]
    
    
    
    ## Create Color palette for nodes and links
    # Calculate the number of unique labels
    num_unique_labels <- unique(c(sk_dataset[[source_color_col]],sk_dataset[[target_color_col]]))

    # Generate a sequence of distinct colors
    label_colors <- rainbow(length(num_unique_labels))

    # Create additional columns "source_Color" and "source_Color" based on the "label_colors" color palette 
    sk_dataset$source_color <- label_colors[match(sk_dataset[[source_color_col]], unique(sk_dataset[[source_color_col]]))] 
    sk_dataset$target_color <- label_colors[match(sk_dataset[[target_color_col]], unique(sk_dataset[[target_color_col]]))] 
    
    #Color_table <- sk_dataset[, unique(Organization_Name_K99), by = "Color"]    
    #colnames(Color_table)[1] <- "Color"
    #colnames(Color_table)[2] <- "Node"
    
    Color_table_source <- subset(sk_dataset, select = c(source_col,"source_color")) 
    Color_table_target <- subset(sk_dataset, select = c(target_col,"target_color")) 
    colnames(Color_table_source)[1] <- "Node"
    colnames(Color_table_target)[1] <- "Node"
    colnames(Color_table_source)[2] <- "Color"
    colnames(Color_table_target)[2] <- "Color"
    Color_table <- rbind(Color_table_source, Color_table_target) 
    Color_table <- unique(Color_table)

    sankey_widget <- sankeyNetwork(
      Links = links, 
      Nodes = nodes, 
      Source = "source",
      Target = "target", 
      Value = "value", 
      NodeID = "name",
      sinksRight = TRUE,
      units = 'awardee(s)',
      colourScale = JS(
        sprintf(
          'd3.scaleOrdinal()  
  .domain(%s)
  .range(%s)',
          jsonlite::toJSON(Color_table$Node),
          jsonlite::toJSON(Color_table$Color)
        )
      ),
      fontSize = 12, 
      fontFamily = "arial", 
      nodeWidth = 10, 
      height = 5000, 
      width = 1000, 
      iterations = 0
    )

    # Define your JavaScript code
    combinedJS <- paste(
      'function(el, x, data) {',
      '  // First JavaScript function code',
      '  d3.select(el).selectAll(".link")',
      '    .style("stroke", d => d.source.color);',
      '',
      '  // Create a custom color scale function dynamically',
      '  var myColors = d3.scaleOrdinal().domain(',
      toJSON(num_unique_labels),
      ').range(',
      toJSON(label_colors),
      ');',
      '',
      '  // Create the legend based on the nodes data',
      '  var svg = d3.select("svg");',
      '  var uniqueGroups = ',
      toJSON(num_unique_labels),
      ';',
      '  var legendX = 25;',
      '  var legendY = 4200;',
      '  var colors = myColors.range();',
      'svg.append("text").attr("x", legendX - 10).attr("y", legendY - 20).text("Legend").style("font-size", "18px").attr("alignment-baseline", "middle")',
      '  for (var i = 0; i < uniqueGroups.length; i++) {',
      '    var group = uniqueGroups[i];',
      '    var color = colors[i];',
      '    svg.append("circle").attr("cx", legendX).attr("cy", legendY).attr("r", 4).style("fill", color);',
      '    svg.append("text").attr("x", legendX + 10).attr("y", legendY).text(group).style("font-size", "10px").attr("alignment-baseline", "middle");',
      '    legendY += 20;',
      '  }',
      '}',
      sep = '\n'
    )
    
    
    htmlwidgets::onRender(x = sankey_widget, jsCode = combinedJS)
  })

  }


# Run the Shiny app
shinyApp(ui = ui, server = server, enableBookmarking = "url")


