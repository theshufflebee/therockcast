#--------------------------------------------------------------------------------

#           This script creates the raw data plots

#--------------------------------------------------------------------------------

# We essentially just call the raw data plotter helper function which does the 
#  rest. Note that that function is not very modular, and only works on data
#   which has the same variables. We put it in a function in order to hide it
#    in the companion paper rmarkdown file as the methodology behind a plot
#     is not quite interesting in the context of this class.

raw_data_plotter(data = data)



#--------------------------------------------------------------------------------