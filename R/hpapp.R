#' interactive use of HumanPrimaryCellAtlasData
#' @rawNamespace import(shiny, except=c(dataTableOutput, renderDataTable))
#' @import celldex
#' @import shinytoastr
#' @import shinyFiles
#' @import shinycssloaders
#' @import DT
#' @importFrom plotly ggplotly renderPlotly plotlyOutput
#' @import ggplot2
#' @examples
#' if (interactive()) hpca_app()
#' @export
hpca_app = function() {
 refs = grep("Data$", ls(asNamespace("celldex")), value=TRUE)
 options(shiny.maxRequestSize = 50 * 1024^2)
 ui = fluidPage(
  useToastr(),
  sidebarLayout(
   sidebarPanel(
    textOutput("msg"),
    helpText("Data source from celldex:"),
    radioButtons("ref", "ref", choices=refs, selected="HumanPrimaryCellAtlasData"),
    helpText("sc-RNA-seq dataset to classify:"),
#    fileInput("newdat", NULL, buttonLabel="newdat", multiple=FALSE),
    shinyFilesButton('newdat', label='File select', multiple=FALSE,
      title="Please select a file"),
    helpText("Select PCs for plotting"),
    helpText("Hover over points for info"),
    helpText("If PCs are identical, boxplots are used for the selected component"),
    radioButtons("dim1", "PC for x", choices=1:4, selected=1, inline=TRUE),
    radioButtons("dim2", "PC for y", choices=1:4, selected=2, inline=TRUE), width=3),
   mainPanel(
    tabsetPanel(
     tabPanel("PCA:ref", 
      plotlyOutput("pca")
      ),
     tabPanel("newdats", 
      helpText("Allow up to a minute for 5000 cells..."),
      downloadButton("downloadData", "Download [when table appears]"),
      shinycssloaders::withSpinner(
        DT::dataTableOutput("sout")
       )
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

server = function(input, output, session) {
  vols = c(Home=fs::path_home())
  shinyFileChoose(input, "newdat", roots=vols)
#  output$newdatmeta = renderTable(input$newdat)
  output$sout = DT::renderDataTable({
    ss = singrun()
    as.data.frame(ss$table$preds)
    })
  singrun = reactive({
   validate(need(!is.integer(input$newdat),"waiting for file"))
   fp = shinyFiles::parseFilePaths(vols, input$newdat)
   toastr_info("starting SingleR")
   sout = do_SingleR(path=fp$datapath)
#   toastr_info("starting projection")
#   vout = viz_pca(sout) # sce
#   toastr_info("done")
   list(table=sout) #, viz=vout)
   })
  output$finplot = renderPlot({
    singrun()$viz
    })


       output$downloadData <- downloadHandler(
         filename = function() {
           # Use the selected dataset as the suggested file name
           paste0(basename(tempfile()), ".csv")
         },
         content = function(file) {
           # Write the dataset to the `file` that will be downloaded
           write.csv(S4Vectors::as.data.frame(singrun()$table$preds), file)
         })
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


   
#' @import S4Vectors
#' @import SummarizedExperiment
viz_pca = function(pred) {
 sce = pred$input
 is_sparse = inherits(SummarizedExperiment::assay(sce), "dgCMatrix")
 if (is_sparse) {
    assay(sce, "counts", withDimnames=FALSE) = 
        as.matrix(assay(sce))  # can't do PCA on dgCMatrix?
    }
 sce = scater::logNormCounts(sce)
 sce2 = BiocSingular::runPCA(sce, 
    BSPARAM=BiocSingular::IrlbaParam(), 
    BPPARAM=BiocParallel::MulticoreParam())
 sce2$pruned.labels = pred$preds$pruned.labels
 scater::plotPCA(sce2, color_by="pruned.labels")
}
