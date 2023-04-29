# Capstone-Project for the Course 'Data Mining in R' 
## University of Lucerne, Spring 2023
## Patrick Stöckli (14-103-675)

This GitHub repository contains a capstone-project that was written for the course 'Data Mining in R' at the University of Lucerne in spring 2023. The project aims to analyse a limited number of Swiss parliamentarian speeches from the 50th legislation (30.11.2015 - 01.12.2019) and to visualize the results appealingly.

## Content

The core of the project is a sentiment analysis using ChatGPT from OpenAI. The main questions were:
- How positive / negative do politicians from certain parties speak about the motions of politicians from the own or other parties?
- How left or right and conservative or liberal are the speeches in the statements about the motions?

## Folders and Files

[code](code)

Here you find all the .R-files, numbered accordingly to the steps in the project. Note that for the download of the speeches, the basic data must be available / provided, see als the documentation of the package [swissparl](https://github.com/zumbov2/swissparl) which is used to retrieve the data.

[data](data)

Here you find all relevant data sets.

[output](output)

Here you find the final report as well as all the plots.

[credentials](credentials)

Note that this folder is empty. You must put an .txt in this folder which is named *openAI_api-key* and includes your own secret API key from OpenAI. You can create an own OpenAI key [here](https://platform.openai.com/account/api-keys). Note that the amount of free usage is limited.
