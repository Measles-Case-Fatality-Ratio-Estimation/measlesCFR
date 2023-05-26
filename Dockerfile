FROM tschaffter/rstudio
# install library
RUN apt-get update -y && apt-get install libgmp3-dev -y
# install conda environment
RUN conda update conda -y && \
    conda create -n mrtool-0.1.0 python=3.8 -y
SHELL ["conda", "run", "-n", "mrtool-0.1.0", "/bin/bash", "-c"]
# install all python dependencies
RUN pip install dill numpy scipy matplotlib pandas xarray && \
    conda install -c conda-forge cyipopt=0.3.0 -y && \
    pip install pycddlib xspline && \
    pip install git+https://github.com/zhengp0/limetr.git@master && \
    pip install git+https://github.com/ihmeuw-msca/mrtool.git@v0.0.1 \
	pip install git+https://github.com/ihmeuw-msca/spmat.git@v0.0.9
SHELL ["/bin/bash", "-c"]
# install R dependencies
RUN R -e "update.packages(ask = FALSE)" && \
    R -e "install.packages('remotes')" && \
    R -e "library(remotes); Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS='true'); install_github('Measles-Case-Fatality-Ratio-Estimation/measlesCFR', force=TRUE, build_vignettes=TRUE)"
