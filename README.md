# portfolio-heuristics

This repo contains code and data accompanying the paper "Psychological heuristics for portfolio decisions" by myself, Simon Algorta, Dieudonne Kantu, Konstantinos Katsikopoulos and Ozgur Simsek. The paper is currently under review and a citation will be added once available.

Code for the simulation experiment is in the *simulation* folder.
Code for the behavioural lab experiment is in the *behavioural_lab* folder.

### Simulation experiment

To reproduce our results from the paper,

- Run **run_experiment.R**,
- Run **process_simulation_output.R**,
- Run **plot_simstudy_figures.R**.

The first few lines of **run_experiment.R** create the few 100 datasets used in the simulations. The rest of the file, which re-runs the experiment, takes a long time to run! 

The folders *data* and *results* are empty in the repo but are included as output from the code above will be saved into these directories.

To run a single or small set of simulations, see **illustrative_example.R**

All functions used to run the portfolio selection problem are contained in *simulation/fns*, with the exception of **experiment_master_fns.R**.

### Behavioural experiment

The two tasks are contained in *behavioural_lab/tasks/task[12]*. To view the tasks as they would have been viewed by study participants, open the **ui.R** file in those folders and run the app. The task instructions are in **Instructions1.Rmd**. Note the "submit" button will not work in the app as this linked to a server that is no longer in use. A instructional video used to walk participants through the task is available at https://youtu.be/5kzl6QcoOCo.

To reproduce our results from the paper,

- Run **make_simulated_data.R** and **make_selection_data.R**,
- Run **infer_heuristics_task_selections.R** and **infer_heuristics_simulated_selections.R**
- Run **analysis_and_plots_for_paper.R**

The folders *output* and *results* are empty in the repo but are included as output from the code above will be saved into these directories.
