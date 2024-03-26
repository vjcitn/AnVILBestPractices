

#' ingestion and conversion
#' @param path character(1) relative path to h5ad file
#' @importFrom zellkonverter readH5AD
ingest_h5ad = function(path="/home/vincent/tenx3k.h5ad") {
  zellkonverter::readH5AD(path)
}

#' use SingleR to obtain scores and labels of classification of cells in an h5ad
#' @importFrom SingleR SingleR
#' @param sce defaults to NULL, can be instance of SingleCellExperiment
#' @param path character(1) path to h5ad file
#' @param ref a SummarizedExperiment as managed by celldex
#' @param ref.type character(1) "label.main" or "label.fine"
#' @param min.common numeric(1) minimum number of features that
#' must be found in common between the SingleCellExperiment
#' to be classified, and the reference.
#' @param assay.type.test numeric(1) defaults to 1, index into assays() of
#' SingleCellExperiment, see Note
#' @param clprocid defaults to NULL; if non-null, should be an
#' instance of the Rcollectl_process R6 class defined in Rcollectl.
#' `clprocid$process$is_alive()` should be TRUE
#' @param \dots passed to SingleR::SingleR
#' @note If sce is NULL and path ends in 'h5ad', zellkonverter::readH5AD 
#' is used to ingest the h5ad resource at path.  If path ends in 'rds' or 'rda',
#' object is ingested and must inherit from SingleCellExperiment.
#' @note Error is thrown if there are fewer than `min.common` features
#' shared between sce and ref
#' @note On Linux systems, the output of `Rcollectl::start()`
#' can be passed for collection of CPU and memory usage statistics.
#' After the run, use `cl_stop(...)` to use methods from Rcollectl
#' to retrieve and visualize statistics.
#' @examples
#' if (requireNamespace("Rcollectl")) {
#'   clid = Rcollectl::cl_start()
#'   Rcollectl::cl_timestamp(clid, "pre-data")
#'   p3k = TENxPBMCData::TENxPBMCData("pbmc3k")
#'   Rcollectl::cl_timestamp(clid, "3k loaded")
#'   rownames(p3k) = make.names(rowData(p3k)$Symbol, unique=TRUE)
#'   bpp = BiocParallel::MulticoreParam()
#'   pred3k = do_SingleR(p3k, clprocid=clid, BPPARAM=bpp)
#'   table(pred3k$preds$pruned.labels)
#'   head(pred3k$preds)
#'   Rcollectl::cl_stop(clid)
#'   Sys.sleep(2) # needed for collectl serialization
#'   path = Rcollectl::cl_result_path(clid)
#'   Rcollectl::plot_usage(Rcollectl::cl_parse(path)) +
#'       Rcollectl::cl_timestamp_layer(path) +
#'       Rcollectl::cl_timestamp_label(path) +
#'       ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1))
#' } else {
#'   p3k = TENxPBMCData::TENxPBMCData("pbmc3k")
#'   rownames(p3k) = make.names(rowData(p3k)$Symbol, unique=TRUE)
#'   pred3k = do_SingleR(p3k, clprocid=clid)
#'   head(pred3k$preds) 
#' }
#' @export
do_SingleR = function(sce=NULL, path="/home/vincent/tenx3k.h5ad", 
    ref=celldex::HumanPrimaryCellAtlasData(), 
    ref.type = "label.main",
    min.common = 1000, assay.type.test=1L, 
    clprocid=NULL, ...) {
  if (!is.null(clprocid) && !clprocid$process$is_alive()) stop("clprocid set but process is not alive")
  if (!is.null(clprocid)) Rcollectl::cl_timestamp(clprocid, "init")
  stopifnot(ref.type %in% c("label.main", "label.fine"))
  if (is.null(sce)) {
   is_h5ad = length(grep("h5ad$", path)==1)
   is_rda = length(grep("rda$", path)==1)
   is_rds = length(grep("rds$", path)==1)
   chk = sum(c(is_h5ad, is_rda, is_rds))
   if (chk != 1) stop("path must have suffix h5ad, rda or rds")
   if (is_h5ad) sce = ingest_h5ad(path)
   else if (is_rda) sce = get(load(path))
   else if (is_rds) sce = get(readRDS(path))
   stopifnot(inherits(sce, "SingleCellExperiment"))
   }
  if (!is.null(clprocid)) Rcollectl::cl_timestamp(clprocid, "pre-singler")
  preds = SingleR(test=sce, 
    ref=ref, labels=ref[[ref.type]], assay.type.test=assay.type.test,...)
  clans = NULL
  if (!is.null(clprocid)) Rcollectl::cl_timestamp(clprocid, "post-singler")
  if (!is.null(clprocid)) {  # FIXME could make this optional to allow embedding in longer trace process
     Rcollectl::cl_stop(clprocid)
     Sys.sleep(2)
     clans = Rcollectl::cl_parse(
       Rcollectl::cl_result_path(clprocid)
       )
     }
  list(input=sce, preds=preds, clans=clans)
}

