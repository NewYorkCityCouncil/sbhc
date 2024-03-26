# Documentation 

## Conventions

Must meet all three: [How to name files  - Jennifer Bryan](https://www.youtube.com/watch?v=ES1LTlnpLMk)

   - [x]  Machine-readable
   - [x]  Human-readable
   - [x]  Sortable 


### File Names

***

> dataset-name_time-granularity_grouping_year.extension

***

+ Lower case underscore naming convention

+ Descriptive file names

+ An underscore separates the different descriptors

+ Hyphens are used in place of spaces within descriptors


**Descriptors**

1. Order file should be run (00-10)
2. Dataset Name, Source, Location
3. Time granularity (hourly, daily, minutely, yearly, etc.)
4. Grouping categorizer ex ‘by-age’, ‘by-cd’, ‘by-cd-age’
5. Date or Year


**Examples:**

+ acs_unemployment_by-cd_2018.csv
+ :cry: N_per_day_age_pop.csv → :partying_face: nys_doc-population_daily_by-age

<br>

<br>


### Variable Names

***

+ No dots in variables names (not python friendly)
+ Lower case underscore naming convention
+ Descriptive 
+ Comment first use of variable (if not completely obvious)

<br>

<br>

### Files, Rmd, ipynbs, scripts, etc 

***

+ Functions and prior scripts should be referenced at the top of every file.
+ Each file should do one thing. 
  + Functions or complicated cleaning or scraping should be put into its own script. 
    + Often is a .R or .PY script
  + There can be a file that calls or references previous files. (One file to run it all or to produce final outputs)
    + Often is a .RMD or .IPYNB or bash file
+ Avoid hard-coding values into your code. 
+ Recommend DRY code. Turn repeated code into a function that takes the changes as parameters.
+ Files that depend on other files require a numbered file name.

<br>

<br>

### Code chunks have comments to :

***

+ Delineate sections of code 
+ Explain parameter choices of functions or hard-coded values
+ Explain first use of variable (if not completely obvious)

<br>

<br>

### Repositories

***

There are two types of repositories: project and collection.

For project repositories: 

+ Each data request is its own repo.
+ The Readme, About, and Tags are filled out.
+ No passwords are ever shown on a repo.
+ If there is no private data, then repo is destined to be public.
+ When cleaning up a repo, old, unused code/data/visuals can be placed into an 'archived' called folder.

