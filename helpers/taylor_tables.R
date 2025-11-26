#--------------------------------------------------------------------------------
#-----------------                     1                        -----------------
#--------------------------------------------------------------------------------

# This is a simple function to make nice tables out of all the various
#  estimated taylor rule models. It is not quite modular, but it is
#   compatible for table-making with all sets of specifications we run.

taylor_regression_to_table <- function(models, caption) {
  
  # Adapts table size (for captions) according to type (semi-modular)
  if (length(models) == 2) {
    model_names = c("TR", "TR w/ SR")
  } else if (length(models) == 4) {
    model_names = c("TR", "TR w/ SR", "TR", "TR w/ SR")
  }
  
  # Creates table
  table <- jtools::export_summs(models, 
                                vcov = sandwich::NeweyWest,
                                model.names = model_names,
                                digits = 4)
  huxtable::caption(table) <- caption
  # Save logic with dynamic filename generation
  if (save_figures) {
    
    # Cleans name in order to be fit for a file name
    clean_name <- tolower(caption)
    clean_name <- gsub("[[:punct:]]", "", clean_name)
    clean_name <- gsub("\\s+", "_", clean_name)
    file_path <- paste0("figures/TR_", clean_name, ".tex")
    
    # Save
    huxtable::quick_latex(table, file = file_path)
  }
  return(table)
}



#--------------------------------------------------------------------------------