#' interactive use of HumanPrimaryCellAtlasData
#' @import shiny
#' @importFrom plotly ggplotly renderPlotly plotlyOutput
#' @import ggplot2
#' @examples
#' if (interactive()) hpca_app()
old_hpca_app = function() {
 ui = fluidPage(
  sidebarLayout(
   sidebarPanel(
    textOutput("msg"),
    helpText("Data source is HumanPrimaryCellAtlasData from Bioconductor celldex package"),
    helpText("Select PCs for plotting"),
    helpText("Hover over points for info"),
    helpText("If PCs are identical, boxplots are used for the selected component"),
    radioButtons("dim1", "PC for x", choices=1:4, selected=1, inline=TRUE),
    radioButtons("dim2", "PC for y", choices=1:4, selected=2, inline=TRUE), width=3),
   mainPanel(
    tabsetPanel(
     tabPanel("main", 
      plotlyOutput("pca")
      ),
     tabPanel("about",
      helpText("AnVILBestPractices is a Bioconductor package
that reviews approaches to managing analytic software for
NHGRI AnVIL"),
      helpText("This app is included to illustrate
how shiny might be used in an AnVIL workspace")
     )
    )
   )
  )
 )

server = function(input, output) {
  output$msg = renderText(sprintf("hpca_app in AnVILBestPractices %s",
    as.character(packageVersion("AnVILBestPractices"))))
  hp = get_hpca()
  types = c("B_cell", "iPS_cells", "Tissue_stem_cells", 
    "Monocyte", "Endothelial_cells", "T_cells", "DC", 
    "Macrophage")
  hplim = hp[, which(hp$label.main %in% types)]
  pc = irlba::prcomp_irlba(t(SummarizedExperiment::assay(hplim)), n=10)
  getdf = reactive({
     an = as.numeric
     data.frame(PCx=pc$x[,an(input$dim1)], PCy=pc$x[,an(input$dim2)], celltype=factor(hplim$label.main))
  })
  output$pca = renderPlotly({
     mydf = getdf()
     if (input$dim1 != input$dim2) pl = ggplot(mydf, 
            aes(x=PCx, y=PCy, text=celltype, colour=celltype)) + 
            geom_point() + xlab(paste0("PC", input$dim1)) + ylab(paste0("PC", input$dim2))
     else pl = ggplot(mydf, aes(x=celltype, y=PCx, colour=celltype)) + geom_boxplot() +
            ylab(paste0("PC", input$dim1)) + theme(axis.text.x = element_blank())
     pl
     })
}
runApp( list(ui=ui, server=server) )
}


   
