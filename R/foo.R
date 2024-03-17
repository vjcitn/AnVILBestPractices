#' Get a Single Cell Experiment
#' @importFrom celldex DatabaseImmuneCellExpressionData
#' @import SingleCellExperiment
#' @examples
#' di = get_dice()
#' table(di$label.main)
#' @export
get_dice = function()
  celldex::DatabaseImmuneCellExpressionData()

#' placeholder
#' @examples
#' foo()
#' @export
foo = function() 1
