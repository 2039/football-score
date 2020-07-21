# football-score

master project 2020

## Install guide

It's assumed that C++ and R are already installed.

R dependencies:

* TMB  
* optparse  
* jsonlite  
* Matrix  
* expm  
* skellam  

optional dependencies:

* rstan  
* tmbstan  

Depending on your workspace, these may be installed automatically when you run the program.

To download the scripts, `cd` into your install directory and run

```sh
git clone https://github.com/2039/football-score.git
```

## Data

The `data/` folder must be filled with data before use. This should contain three files, `scores.csv`, `teams.csv`, `scores_full.csv`.

The header of each file is:

* `scores.csv` : `home_team,away_team,home_team_score,away_team_score`  
* `teams.csv` : `key,name`  
* `scores_full.csv` : `Hjemmelag,Bortelag,Hjemmeskår,Borteskår,Runde,Dato,Dag,Tid,Bane,Kampnummer`  

The translation of the `scores_full.csv` header is : `home_team,away_team,home_score,away_score,round,date,weekday,time,stadium,match_id`. We note that the type of `home_team` and `away_team` differ in the tables.

The type of each row is:

* `scores.csv` : `int,int,int,int`  
* `teams.csv` : `int,str`  
* `scores_full.csv` : `str,str,int,int,int,date,str,time,str,int`  

where `date` is formatted as a `dd-mm-yyyy` string and `time` a `hh:mm` string.

## Usage

Depending on your workflow, this step may vary.


Using Rscript, you may run one of `scoremodel-cont-var.R`, `scoremodel-disc-var.R`, `scoremodel-time-independent.R`.

The model must currently be changed manually, by setting the `MODEL` variable. It can be `[cont_var|cont_rw]` in `-cont-var.R` or `[disc_var|disc_rw|disc_wn]` in `-disc-var.R`.

If you want to save the resulting parameters to file, you may use CLI arguments:

* `-s --save [Ø|filename]`  
* `-f, --format [json|csv|dat]`  

So saving the parameters for `disc_var`, you may run

```sh
Rscript scoremodel-disc-var.R --save --format json
```

The format should be specified when saving.

## Notes

there are none.
