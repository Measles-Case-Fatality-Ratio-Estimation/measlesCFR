# Running the MeaslesCFR Package
The measlesCFR package uses a Docker container. This ensures that all of the
package dependencies are installed and that it will work on different operating
systems (e.g. Windows, Mac). The Docker image hosted at: https://hub.docker.com/r/measlescasefatalityratio/measles_cfr

Below are the steps for running the package (steps modified from Reed Sorensen's "Health Metrics Toolbox":
1. Install Docker Desktop from https://www.docker.com/products/docker-desktop.
2. On the command line run: `docker run --rm -p 8787:8787 -e ROOT=TRUE -e PASSWORD=dockpass measlescasefatalityratio/measles_cfr`
  - The initial run may take awhile, but will be quicker on subsequent runs
  - Optionally, you can add the -v tag to give a container access to a location on your
    computer’s file system. The tag expects the file system location, followed by a colon,
    followed by /mnt (e.g. -v /path/to/local/directory:/mnt). This would go immediately after the
    word dockpass in the example above. Then when you are inside the session, you can access
    files at /path/to/local/directory by instead referring to /mnt. For people using Windows,
    note that the usual Windows file path will need to be modified slightly. For example,
    C:\Users\myname\data would need to be converted to //c/Users/myname/data.
3. In a browser, go to localhost:8787. Enter user name “rstudio” and password “dockpass”.
4. `library(reticualte)`
5. `reticulate::use_python("/opt/miniconda/envs/mrtool-0.1.0/bin/python")`
6. `mrtool <- import("mrtool")`
7. `library(measlesCFR)`
8. Start using the package e.g. `predictCFR(country = "ETH")`


