
### Purpose:

1) The analysis will be re-run on your computer within a docker container to replicate the operating system and package versions where the original analysis took place.

2) These results will then imported into the analysis package on your local computer that you will clone in step 2).

3) This will give you access to both the code that was used to run the analysis and the resulting output.

### Instructions
1) Unless you already have them installed, install [git](https://git-scm.com/book/en/v2/Getting-Started-Installing-Git) and [docker](https://www.docker.com).

2) Clone the repository using the following on the command prompt: 

`git clone https://github.com/jasonserviss/mesenchymalSubsetAnalysis.git`

3) Change directory into the repository: 
 
`cd mesenchymalSubsetAnalysis`

4) Get the docker image by typing the following on the command line (this may take several minutes): 

`docker pull jasonserviss/mesenchymalsubsetanalysis`

5) Run the analysis with the following command (note: this will take some time, hours not days, as the whole analysis will re-run. The script will use 2 cores by default.):

`docker run -v $PWD:/home/mesenchymalSubsetAnalysis jasonserviss/mesenchymalsubsetanalysis Rscript -e "source('/home/mesenchymalSubsetAnalysis/inst/scripts/runAllScripts.R')"`

6) Exit docker and clean-up the downloaded image:

Press `Ctrl C` to exit docker. Then: 
`docker rm $(docker ps -a -q)`
`docker rmi $(docker images -q)`

6) In the analysis package directory you will now find:

  data: data that was either input to or output from the analysis

  inst/scripts: scripts used to run the analysis

  inst/*.Rmd and *.html: analysis files where the results from the analysis are explained and plotted

  R: Custom R functions used during the analysis

