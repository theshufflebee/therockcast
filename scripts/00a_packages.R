#--------------------------------------------------------------------------------

#           This script is made to load all packages

#--------------------------------------------------------------------------------


# Run helper function on all packages 

package_loader("tinytex")      # LaTeX support
package_loader("ggplot2")      # plots
package_loader("AER")          # NW formula
package_loader("forecast")     # time series forecasting
package_loader("here")         # directory finder
package_loader("dplyr")        # data management
package_loader("tidyr")        # also data management
package_loader("lubridate")    # data dates management
package_loader("zoo")          # for lagging
package_loader("jtools")       # regression tables
package_loader("huxtable")     # also regression tables
package_loader("lmtest")       # regression tests
package_loader("data.table")   # for data filtering
package_loader("sandwich")     # regression errors
package_loader("texreg")       # arima tables
package_loader("R.matlab")     # for loading matlab data (shadowrates)
package_loader("tseries")      # time series tests
package_loader("eurostat")     # eurostat data
package_loader("fredr")        # fredr data
package_loader("rdbnomics")    # inflation expectations data
package_loader("mFilter")      # HP filter
package_loader("knitr")        # tables
package_loader("kableExtra")   # tables extra
package_loader("doParallel")   # for parallelization 
package_loader("foreach")      # for speedy loops
package_loader("car")          # for easy hypothesis testing
package_loader("ggridges")     # for fancy density plots
package_loader("strucchange")  # Chow & BP tests for structural breaks
package_loader("transx")       # for hamilton filter
package_loader("webshot2")     # for png tables



#--------------------------------------------------------------------------------