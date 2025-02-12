# Bayesian Tennis Experiment
## Can humans learn and use bimodal priors in complex sensorimotor behaviour?
**Experimental task**
![Alt Text](./task.gif)

This repo contains data and code for the analysis. Contains the following folders:

- **Code**
  - Python script to clean raw data and prepare data for the statistical analysis
  - R-scripts for the multilevel regression analysis and plots
    - Separate scripts for day 1, 2, 3, a combined script for day 2+3, a script for the control experiment, and a script to calculate the percentage of no hits and backswing hits
  - Execute the R scripts with the R-studio project "statistical_analysis"
- **Data**
  - Prepared data for statistical analysis
- **Experimental_protocols**
  - Files specifying the ball position (i.e., the true stimulus) and uncertainty condition (slow/moderate/fast) for every trial in the experiment
- **Plots**
  - All plots saved for each participant in each condition on each day and the summary plots of the aggregated plots of all participants
- **Raw_data**
  - Raw data after the experiment. There are two folders for each day: a folder for the participants ID 0-15 and 16-31. 24 participants in total (ID: 1-12, 15-19, 22-28) and for the control experiment (ID: 1-14, 16-20, 22-26). The remaining IDs served as a reserve
- **Simulation**
  - Jupyter notebook for Bayesian predictions. Reads experimental protocol and estimates participants’ response based on a Bayesian model

### Experiment in short
- Subjects (N=24) return tennis serves in VR on 3 days
- 3 days x 480 trials = 1,440 trials
- We control the ball locations (i.e., true stimulus) 🡪 follows a bimodal distribution
- We manipulate uncertainty of sensory information 🡪 3 levels of ball speeds: slow, moderate, fast

### Information on the analysis
- **Main DV:** Estimation error = horizontal difference between racket centre and ball
  - In R script = “horizontal_difference”
- **Multilevel regression analysis**
  - We quantify the bimodal-prior effect by a “jump” between the left and right sides of the distribution
  - In R-script = “left_right” term
