# From Data to Models

This repository includes data and R scripts for the tutorial:

   **From Data to Models: classification, prediction, and synthesis**
   
given by [Carlo A. Furia](https://bugcounting.net) at USI in October 2019.

All data is from public datasets; see files `sources` for links to the
original sources.

## Requirements to run the tutorial

   1. Install the **R** platform from <https://stat.ethz.ch/CRAN/> by
      following the instructions for your operating system
   
   2. (Optional, but recommended) Install the **RStudio** IDE from
      <https://www.rstudio.com/products/rstudio/download/> by choosing
      the *free* version of *RStudio Desktop* and following the
      instructions for your operating system
	  
   3. Get a snapshot of this repository:
      - Download the archive file <https://github.com/bugcounting/data2models/archive/master.zip>
      - Unpack it into a directory in your system (we'll call it `root` in these instructions)
		 
   4. Open RStudio and issue the following commands from the pull-down menus:
      - File -> Open File -> pick file
        `root/data2models-master/spam/pull_data.R`
	  - Session -> Set Working Directory -> To Source File Location
	  - Code -> Source
	  
	  This will download the remaining libraries and data


## Credits and references

   - The book *[Machine learning for
     hackers](http://shop.oreilly.com/product/0636920018483.do)* by
     Drew Conway and John Myles White (O'Reilly, 2012) is a practical
     presentation of several machine learning techniques (including
     naive Bayes classifiers and linear regression) with complete code
     examples in R. The example of Bayesian spam classifier is based
     on chapter 3 of the book.
   
   - Dirk Schumacher, the author of the `rpicosat` library used in the
     Sudoku example, has a [blog
     post](http://www.dirk-schumacher.net/2017/07/23/solve-sudokus-with-r-and-a-sat-solver/)
     where he describes how to build the same propositional Sudoku
     constraints encoding we use in `sat/sat.R` but using a different
     approach in R.
 
