#' run_scrublet_seurat
#'
#' @param seurat_obj
#' @param split.by
#' @param assay
#' @param env_name
#'
#' @return
#' @export
#'
#' @examples
run_scrublet_seurat <- function(
    seurat_obj,
    split.by = NULL,
    assay = "RNA",
    env_name = "r-reticulate"
) {
  # ------------------------------ #
  #   Basic checks
  # ------------------------------ #
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("Package 'reticulate' is required but not installed.")
  }
  library(reticulate)

  if (!is.null(split.by) && !split.by %in% colnames(seurat_obj@meta.data)) {
    stop(paste0("split.by variable '", split.by, "' not found in meta.data"))
  }

  # Use specified conda environment
  use_condaenv(env_name, required = TRUE)

  # Import Scrublet
  scr <- import("scrublet")

  # ------------------------------ #
  #   Internal function: Run Scrublet
  # ------------------------------ #
  run_scrub <- function(mat) {
    scrub <- scr$Scrublet(t(mat))
    res <- scrub$scrub_doublets()

    list(
      score = as.numeric(res[[1]]),
      pred  = res[[2]]
    )
  }

  # ------------------------------ #
  #   Extract count matrix
  # ------------------------------ #
  counts_matrix <- GetAssayData(seurat_obj, assay = assay, slot = "counts")

  # ------------------------------ #
  #   With groups: Run by group
  # ------------------------------ #
  if (!is.null(split.by)) {
    groups <- seurat_obj@meta.data[[split.by]]
    unique_groups <- unique(groups)

    all_scores <- numeric(ncol(seurat_obj))
    all_preds  <- character(ncol(seurat_obj))

    for (g in unique_groups) {
      message("Running scrublet for group: ", g)

      # cell_ids <- colnames(seurat_obj)[groups == g]
      sub_mat <- counts_matrix[, groups == g, drop = FALSE]
      print(dim(sub_mat))

      res <- run_scrub(as.matrix(sub_mat))

      all_scores[groups == g] <- res$score
      all_preds[groups == g]  <- res$pred
    }

  } else {
    # ------------------------------ #
    #   Without groups: Run overall
    # ------------------------------ #
    res <- run_scrub(counts_matrix)
    all_scores <- res$score
    all_preds  <- res$pred
  }

  # ------------------------------ #
  #   Write to meta.data
  # ------------------------------ #
  seurat_obj$doublet_score  <- all_scores
  seurat_obj$is_doublet     <- all_preds

  return(seurat_obj)
}
#' run_scrublet
#'
#' @param counts_matrix genes x cells
#' @param env_name
#'
#' @return
#' @export
#'
#' @examples
run_scrublet <- function(counts_matrix, env_name = "r-reticulate") {
  # Ensure reticulate is available
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("Package 'reticulate' is required but not installed.")
  }
  library(reticulate)

  # 1. Specify the Python environment to use
  use_condaenv(env_name, required = TRUE)

  # 2. Import Scrublet module
  scr <- import("scrublet")

  # 3. Initialize Scrublet object
  scrub <- scr$Scrublet(t(counts_matrix))

  # 4. Run detection
  res <- scrub$scrub_doublets()

  # 5. Extract results
  doublet_scores <- as.numeric(res[[1]])
  predicted_doublets <- as.logical(res[[2]])

  # 6. 返回结果
  return(list(
    doublet_scores = doublet_scores,
    predicted_doublets = predicted_doublets
  ))
}
