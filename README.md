# LangComplexity

Here, you can find scripts and datasets for the analysis of cross-linguistic data in the context of the project "Canonical Rate & Language Properties"

Structure of the pipeline:
* [LangComplex](https://github.com/LAAC-LSCP/LangComplexity/blob/master/LangComplex.R) 
  * *Description:* Analysis on children CP wrt. to their language complexity (e.g. different syllable complexity levels);
* [Adult_CR_analysis](https://github.com/LAAC-LSCP/LangComplexity/blob/master/Adult_CR_analysis.R)
  * *Description:* Analysis on adult CP wrt. to their language complexity;
* [PhonComplex](https://github.com/LAAC-LSCP/LangComplexity/blob/master/PhonComplex.R)
  * *Description:* Qualitative analysis of phonetical complexity (i.e. vowels and consonants) based on Maddieson classification;
* [Numeric_Consonants_and_Vowels](https://github.com/LAAC-LSCP/LangComplexity/blob/master/Numeric_Consonants_and_Vowels.R)
  * *Description:* Quantative analysis of phonetical complexity (i.e. vowels and consonants) 

Data processed from annotated ELAN files into readable xslx/cvc datasets are located in [Data](https://github.com/LAAC-LSCP/LangComplexity/tree/master/Data) folder.
 
Datasets are located in the following folders:
* [RawData]() contains original data that was used in the analysis
* [Data](https://github.com/LAAC-LSCP/LangComplexity/tree/master/Data) 

You can either make changes to the analysis pipeline, or simply reproduce the pipeline.

##  To make changes to the analysis pipeline

You need to be a coauthor of this github repo. Ask Chiara or Alex to add you.

You may need to download GitHub Desktop and follow [this tutorial](https://docs.github.com/en/desktop/contributing-and-collaborating-using-github-desktop/cloning-a-repository-from-github-to-github-desktop) to clone the current repository.  You only need to do this once.

From now on, you can make changes to the code locally on your machine which will be tracked automatically. To push your changes to the remote repository on GitHub, click *Push origin*. Next time you launch the binder these changes will show up.

To get changes others have made, click "pull".

## To reproduce the pipeline:

1. Launch the Binder

Click on the badge to launch the Binder:

[![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/psilonpneuma/LangComplexity/master)

2. Run `LangComplex.R`

**TO DO:** :vertical_traffic_light:

1. In the top right corner, click "New" :arrow_right: "Rstudio"
2. In the console (the left-side panel) in Rstudio, type `source("LangComplex.R")` and press return

**N.B.:** If you save this notebook, it **will not** be saved to the GitHub repo.
Pushing changes back to the GitHub repo through the container is not possible with Binder.
**Any changes you have made to files inside the Binder will be lost once you close the browser window.**

Author: Chiara Semenzin, edits by Alejandrina Cristia

