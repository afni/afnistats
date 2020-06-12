# afnistats, a possible alternative for packaging AFNI's R code

This is a a demo of what an "afnistats" packaging would look like. This would help automate testing/dependency management to move towards an improved installation experience across OSes/R versions.

It follows as best it can the guidance of ["R packaging"](http://r-pkgs.had.co.nz/intro.html). This is helpfully summarized in the [Rstudio cheatsheet](https://github.com/rstudio/cheatsheets/raw/master/package-development.pdf).

The principle efforts in refactoring the code here are for the purposes of :
- establishing compatible with R's packaging infrastructure
- adding tests for a basic guarantee of functionality
- reducing redundancy by having user inputs, their default values, and their help messages specified in a single location
- add basic input validation in an automated manner (check for number of arguments and their type for the various arugments)
- make use of R's documentation infrastructure for convenient access from within R
- specify dependencies in a standard way
- attempt to conform a little better to [community styleguides](https://style.tidyverse.org/)

# Work to be done

This is far from a complete work. Things to consider for improvement:

+ consider compatibility with R_io.so. R has [extensive support](http://r-pkgs.had.co.nz/src.html#clang) for compiling C as part of packaging building but a more realistic solution for distribution with the rest of AFNI is to assume R_io.so is already installed. This would be trivial on conda to specify a dependency. On other OSes, specifying dependencies using homebrew/apt package managers would work to a degree but limited support would be offered there. Looking into how to specify dependencies on system libraries within RCran is likely a reasonable way forward
+ extend the list of refactored programs beyond MBA and RBA. This would take time. As common code across various tools is extracted into generic functions this task will become easier and easier to do. For now it is somewhat depending (taking hours-days for each refactored script)
+ consider [how R specifies dependencies](http://r-pkgs.had.co.nz/description.html#dependencies) on tools that are used via system calls
+ there are a few minor details to be compliant with CRAN but overall the package is very close to fully compatible.
+ there are some tests failing. When I stopped working on this I was attempting to write some code for some basic validation of input data given the model specification (as in are the variables sufficiently crossed/replicated etc). Jumping back a few commits to a work version may be a more sensible starting point. Though the more extensive data validation would be nice to have. For MBA, inadequate data may not raise an error until a few weeks in to training the model...


# Testing the package out

With docker from the repository directory:

```
sudo docker run --rm -ti -v $PWD:/home/rstudio/work -e PASSWORD=hello -e ROOT=TRUE -e USERID=`id -u` -p 8787:8787 rocker/tidyverse
```

Going to a localhost:8787 to access an Rstudio instance and load the afnistats.proj file in the work directory. Follow the standard packaging operations as described in the packaging cheatsheet. 

A sensible start would be running the following commands in the R console:

```
devtools::install_deps(upgrade="never")
devtools::check()
```

# To test versions of R

Circleci is setup to run an installation and some basic checks for every commit to github across differnt versions of R. It is not especially useful but you can run these locally using the [local circleci interface](https://circleci.com/docs/2.0/local-cli/#installation). So for example:

```
circleci local execute --job test_3.6.3

```
