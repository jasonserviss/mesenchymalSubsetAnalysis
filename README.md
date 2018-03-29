### Purpose

1) The analysis will be re-run on your computer within a docker container to replicate the operating system and package versions where the original analysis took place.

2) These results will then imported into the analysis package on your local computer that you will clone in step 2).

3) This will give you access to both the code that was used to run the analysis and the resulting output.

### Instructions
1) Unless you already have them installed, install [git](https://git-scm.com/book/en/v2/Getting-Started-Installing-Git) and [docker](https://www.docker.com). Once docker is installed the default max CPU and max memory usage will probably need to be adjusted in the advanced preferences. I ran the analysis with 7.5GB of memory, 4GB of swap, and 4 CPUs. 

2) Clone the repository using the following on the command prompt: 

<pre><code>git clone https://github.com/jasonserviss/mesenchymalSubsetAnalysis.git</code></pre>

3) Change directory into the repository: 
 
<pre><code>cd mesenchymalSubsetAnalysis</code></pre>

4) Get the docker image by typing the following on the command line (this may take several minutes): 

<pre><code>docker pull jasonserviss/mesenchymalsubsetanalysis</code></pre>

5) Run the analysis with the following command (note: this will take some time, hours not days, as the whole analysis will re-run. The script will use 2 cores by default.):

<pre><code>docker run -v $PWD:/home/mesenchymalSubsetAnalysis jasonserviss/mesenchymalsubsetanalysis Rscript -e "source('/home/mesenchymalSubsetAnalysis/inst/scripts/runAllScripts.R')"</code></pre>

6) Exit docker and clean-up the downloaded image:

<pre><code>docker ps -a | awk '{ print $1,$2 }' | grep jasonserviss/mesenchymalsubsetanalysis | awk '{print $1 }' | xargs -I {} docker rm {}</code></pre>

<pre><code>docker rmi 'jasonserviss/mesenchymalsubsetanalysis'</code></pre>

7) In the analysis package directory you will now find:

  * data: data that was either input to or output from the analysis

  * inst/scripts: scripts used to run the analysis

  * inst/*.Rmd and *.html: analysis files where the results from the analysis are explained and plotted

  * R: Custom R functions used during the analysis

8) If so desired, you can install the package, giving you access to the data and functions used to generate the data by typing the following from within R (install devtools package if not already installed):

<pre><code>devtools::install('path to package root')</code></pre>

