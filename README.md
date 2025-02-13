# Investigating the Seasonality of RSV and Influenza since the Emergence of SARS-COV-2

_Abstract_

The seasonality of influenza and respiratory syncytial virus (RSV) has been well-documented. The emergence of COVID-19 and the subsequent public health measures implemented around the globe in 2020 introduced unprecedented disruptions to their transmission patterns. This research project examines the changes in influenza and RSV seasonality both before and after the pandemic, comparing trends from 2017-2020 (pre-COVID) to those in 2021-2024 (post-COVID) across multiple regions. Using techniques including time-lag correlation and bootstrapping analyses, we found that influenza peaks have shifted earlier post-COVID by 1 to 10 weeks in most regions. RSV trends have been more variable. These findings have important implications for vaccine timing and public health preparedness, highlighting the need for continued surveillance and country-specific intervention strategies.


This GitHub contains the repository to fully reproduce each step in the report generating process

# Getting Started with the Project

By the end of this README, the project directory should have the same structure as this: 

![Imgur Image](https://i.imgur.com/fWPBTdJ.png)

### 1. Download/Clone the Repository

You can download or clone the repository using [Git](https://git-scm.com/downloads) or by downloading from Github.

- **To clone the repository using Git**:
  ```bash
  git clone https://github.com/evmcnb/sanofi-rsv-flu.git
  ```

- **To download as a ZIP file**:
  Go to the repository page on GitHub and click the "Code" button, then select "Download ZIP." Extract the ZIP file to your desired location.

### 2. R Working Directory

All R scripts are designed to be executed within the sanofi-rsv-flu folder.

```R
setwd("/path/to/sanofi-rsv-flu")
```

The packages required must be install manually. The only package not available on CRAN is bbplot which can be found here: https://github.com/bbc/bbplot


### 3. PYHTON ONLY FROM HERE - Create a Virtual Environment

Navigate to the project folder and create a virtual environment:

If you don't have `virtualenv` installed, you can install it using `pip`. Run the following command:

```bash
pip install virtualenv
```

Then the virtual environment must be installed:

```bash
cd /path/to/sanofi-rsv-flu
virtualenv venv
```

### 4. Activate the Virtual Environment

Before running the project, activate the virtual environment by following the instructions for your operating system:

- **macOS/Linux**:
  ```bash
  # Activate the virtual environment
  source venv/bin/activate
  ```

- **Windows (Command Prompt)**:
  ```cmd
  # Activate the virtual environment
  venv\Scripts\activate
  ```

- **Windows (PowerShell)**:
  ```powershell
  # Activate the virtual environment
  .\venv\Scripts\Activate
  ```

### 5. Install Requirements

After activating the virtual environment, install the necessary dependencies using `pip`:

```bash
pip install -r requirements.txt
```

### 6. Deactivating the Virtual Environment

When you're done working on the project, you can deactivate the virtual environment by running:

```bash
deactivate
```
