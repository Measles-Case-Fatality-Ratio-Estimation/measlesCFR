# Overview of the MeaslesCFR Package
The measlesCFR package is a system for estimating measles case fatality ratios (CFRs) at the country, year, and age level for low- and middle-income countries. Users can inform covariate predictors in both/either the historic or future time periods, and the package will provide the resulting measles CFR estimates. For more detail on package functions, please view the package documentation in the measlesCFR.pdf in this repository.

# Running the MeaslesCFR Package
The measlesCFR package requires the use of a Docker container. This ensures that all of the
package dependencies are installed and that it will work on different operating
systems (e.g., Windows, Mac). The Docker image is hosted at: https://hub.docker.com/r/measlescasefatalityratio/measles_cfr

Below are the steps for running the package (steps modified from Reed Sorensen's "Health Metrics Toolbox":
1. Install Docker Desktop from https://www.docker.com/products/docker-desktop.
  - Step 1 is a one-time installation. For subsequent uses of the measlesCFR package, begin at Step 2.
2. Open the command line
  - If using Windows: Type “cmd” into the search box
  - If using Mac: Type "terminal" into the search box
3. On the command line run: `docker run --rm -p 8787:8787 -e ROOT=TRUE -e PASSWORD=dockpass measlescasefatalityratio/measles_cfr`
  - The initial run may take awhile, but will be quicker on subsequent runs
  - Optionally, you can add the -v tag to give a container access to a location on your
    computer’s file system. The tag expects the file system location, followed by a colon,
    followed by /mnt (e.g. -v /path/to/local/directory:/mnt). This would go immediately after the
    word dockpass in the example above. Then when you are inside the session, you can access
    files at /path/to/local/directory by instead referring to /mnt. For people using Windows,
    note that the usual Windows file path will need to be modified slightly. For example,
    C:\Users\myname\data would need to be converted to //c/Users/myname/data.
4. In a browser, go to localhost:8787. Enter user name “rstudio” and password “dockpass”.
  - Step 4 will open a virtual session of Rstudio. Subsequent steps will be completed in Rstudio.
5. `library(reticulate)`
6. `reticulate::use_python("/opt/miniconda/envs/mrtool-0.1.0/bin/python")`
7. `mrtool <- import("mrtool")`
8. `library(measlesCFR)`
9. Start using the package, e.g., `predictCFR(country = "ETH")`

## Additional Notes
- Users can view accepted low- and middle-income ISO3 country codes by calling `listOfCountries()`.
- Users can access the vignette with `vignette("measles-vignette",  package="measlesCFR")` after running the install steps.
- Users can view the documentation for functions with `help(<function_name>)` e.g., `help(predictCFR)`. They can also view more information about the package in the measlesCFR.pdf in this repository.

## More Docker Information
A Docker container is a computing environment that contains everything needed to run an application (e.g., measlesCFR in an Rstudio session), including all the dependent libraries. A Docker container is an instance of a Docker image, which is created from a Dockerfile. The measlesCFR image is hosted on Docker Hub.

Users can install other R packages in the Rstudio session that is launched by the Docker container, but they will need to re-install the packages whenever they restart the container.

