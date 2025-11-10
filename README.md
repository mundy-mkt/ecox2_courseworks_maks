# 📘 Econometrics II – Home Assignments 

**Disclaimer:**  
The project is hosted on GitHub. To ensure everything works correctly (file paths, structure, dependencies), please **clone the repository** instead of downloading individual files.

```bash
git clone https://github.com/mundy-mkt/ecox2_courseworks_maks
```

## Repo structure

📦 repository_root
│
├── 📁 hw1/ # Homework 1 (less relevant here)
│ ├── 📁 code/ # R scripts / Rmd 
│ ├── 📁 data/ # Data used for HW1
│ └── 📄 LaTeX_Econ_II__HW1__2025.pdf # Official homework assignment (problem description)
│
├── 📁 hw2/ # HOMEWORK 2 
│ │
│ ├── 📁 code/ # All executable code and the final report
│ │ ├── HA2_report.Rmd # Main RMarkdown file (full analysis pipeline)
│ │ ├── HA2_report.pdf # Final homework report (compiled output)
│ │ ├── HA2_report.log # Log generated during compilation (automatically created)
│ │ └── Problem 1 HA2 Huskov Stetsko… # PDF solution of the 1st task
│ │
│ ├── 📁 data/ # All datasets used in the HW2 analysis
│ │ ├── child_formal_care.xlsx
│ │ ├── gdp_per_capita.xlsx
│ │ ├── gen_empl_gap.xlsx
│ │ ├── gender_pay_gap.xlsx
│ │ ├── ict_spec_sex.xlsx
│ │ └── parental_leave_data.xlsx
│ │
│ └── 📄 Econ_II_HW2_2025.pdf # Official homework assignment (problem description)
│
├── 📁 renv/ # renv internal folder (auto-managed, do not edit)
├── 📄 renv.lock # Exact list of package versions (reproducibility)
├── 📄 .Rprofile # Ensures automatic renv project activation
├── 📄 .gitignore
└── 📄 README.md # General explanation of the project and structure

## Project Setup (R + renv)

This repository uses [**renv**](https://rstudio.github.io/renv/) for reproducible R package management. The lockfile is `renv.lock` (not `renv.log`). Restoring from it will install the exact package set into a project-local library, optionally reusing a global cache for speed.

### Quick start

1.  **Open R/RStudio in the project root** (the folder containing `renv.lock` and `renv/`).
2.  **Install `renv` to your *user* library** (once per machine): `r install.packages("renv")`
3. **In most cases, the project auto-activates via renv/activate.R.** If not, run: ```renv::activate()```
4. **Finish with** ```renv::install()``` to install all the packages.
