# Deep Learning for Social Media Image Classification in European Natural Sites

[![Python](https://img.shields.io/badge/Python-3.8+-blue.svg)](https://www.python.org/)
[![R](https://img.shields.io/badge/R-4.0+-blue.svg)](https://cran.r-project.org/)
[![License](https://img.shields.io/badge/License-MIT-green.svg)](LICENSE)
[![Paper](https://img.shields.io/badge/Paper-in%20preparation-orange.svg)]()

This repository contains the code and statistical analyses for the paper:

**"An open-source deep-learning model for classifying the content of social media images across European natural sites"**  
*Barrere, J., Macia, F. M., Chivulescu, S., van Dijk, J., van Eupen, M., Giuca, R. C., Luque, S., Roche, P., Schirpke, U., Tappeiner, U., Tardieu, L., Zulian, G., & Lenormand, M., currently in preparation.*

The code requires prior installation of [R](https://cran.r-project.org/) and [Python](https://www.python.org/), and is structured as follows:

- The notebook **`AI_Flickr.ipynb`** contains the code to train the deep-learning model and export the outputs of the different experiments. To run, it needs access to GPU calculation resources, and a data folder containing the images and a CSV file `annotations.csv`, which will be made public.
- The files **`requirements.txt`** and **`install.sh`** are used by the main notebook to install all dependencies for the deep-learning model.
- **`model.py`** contains all functions and classes required for the deep-learning model.
- The different R scripts in the **`paper/`** directory contain the code used to format the outputs of the notebook and produce the tables and figures for the paper. In addition to the outputs of the notebook, the scripts require a few additional files in the data folder (on land use, climate, and site boundaries) which can be made available upon request.
