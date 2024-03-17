#' Get a Single Cell Experiment
#' @importFrom celldex HumanPrimaryCellAtlasData
#' @import SummarizedExperiment
#' @examples
#' hp = get_hpca()
#" dim(assay(hp))
#' table(hp$label.main)
#' @export
get_hpca = function()
  celldex::HumanPrimaryCellAtlasData()

#' placeholder
#' @examples
#' foo()
#' @export
foo = function() 1
