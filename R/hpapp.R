#' interactive use of HumanPrimaryCellAtlasData
#' @import shiny
#' @import celldex
#' @importFrom plotly ggplotly renderPlotly plotlyOutput
#' @import ggplot2
#' @examples
#' if (interactive()) hpca_app()
#' @export
hpca_app = function() {
 refs = grep("Data$", ls(asNamespace("celldex")), value=TRUE)
 options(shiny.maxRequestSize = 50 * 1024^2)
 ui = fluidPage(
  sidebarLayout(
   sidebarPanel(
    textOutput("msg"),
    helpText("Data source from celldex:"),
    radioButtons("ref", "ref", choices=refs, selected="HumanPrimaryCellAtlasData"),
    helpText("sc-RNA-seq dataset to classify:"),
    fileInput("newdat", NULL, buttonLabel="newdat", multiple=FALSE),
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
      tableOutput("newdatmeta"),
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
  output$newdatmeta = renderTable(input$newdat)
  
  output$msg = renderText(sprintf("hpca_app in AnVILBestPractices %s",
    as.character(packageVersion("AnVILBestPractices"))))
  getref = reactive({
#   validate(need(nchar(input$newref$datapath)>0, "waiting for reference selection"))
#   ref = get(load(input$newref$datapath))
#print(ref)
   ref = get(input$ref)() # input$ref is name of function in celldex, which we call
   min_num_cells = 10
   ttab = table(ref$label.main)
   ttabok = ttab[ttab>min_num_cells] 
   types = names(ttabok)
   reflim = ref[, which(ref$label.main %in% types)]
   an = as.numeric
   pc = irlba::prcomp_irlba(t(SummarizedExperiment::assay(reflim)), n=10)
   pcdf=data.frame(PCx=pc$x[,an(input$dim1)], PCy=pc$x[,an(input$dim2)], celltype=factor(reflim$label.main))
   list(reflim=reflim, pcdf=pcdf)
  })
#  types = c("B_cell", "iPS_cells", "Tissue_stem_cells", 
#    "Monocyte", "Endothelial_cells", "T_cells", "DC", 
#    "Macrophage")
  getdf = reactive({
  })
  output$pca = renderPlotly({
     reflist = getref()
     mydf = reflist$pcdf
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


   
