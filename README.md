# thesis-code
Repository of Master's Thesis "Nonparemetric variable selection under latent confounding"

# Code files for the Section 4

## File hierarchy

In brief, our workflow for the thesis is organised into 7 experiments

- a `exp1` folder containing all codes for Experiment 1,
- a `exp2` folder containing all codes for Experiment 2,
- a `exp3` folder containing all codes and output plots (as .png) for Experiment 3,
- a `exp4` folder containing all codes and output plots for Experiment 4,
- a `exp5` folder containing all codes for Experiment 5,
- a `exp6` folder containing all codes and output plots for Experiment 6,
- a `exp7` folder containing all codes and plots for Experiment 7,
    - a `results` folder containing all `.RData` outputs for Experiment 7.

## Replicating instructions for Folder `exp7`

  1. Run  `exp7_PCA.R` and save the outputs into folder `results`.
  2. Generating datasets using `Export_dataset.R` and run the IPython notebook `VAE_Latent_Confounder_Finder.ipynb`.
    3. Save the generated latent confounder estimation files into your self-defined directory.
    4. Run `exp7_VAE.R` using the self-defined directory in step 3 and save the outputs into folder `results`.
    5. Run `kernel_density_plot.R` to generate plots.


## Recommended steps (for Microsoft Windows)

For Experiment 2 - 7, it is recommended to use scientific computing clusters like Euler.

In Terminal, log in with

```bash
$ ssh your_user_name@euler.ethz.ch
```

Load the required modules with

```bash
$ env2lmod
$ module load r/4.0.2
$ module load gmp/6.1.2
```

Start to run the R files with 8 cores and waiting time 10 hours

```bash
$ bsub -o experiments -n 8 -W 10:00 -R "rusage[mem=2048]" "R --vanilla --slave <file_name.R > result_output"
```



In R GUI (or RStudio), please install the following packages beforehand:

- `FOCI`
- `ggplot2`
- `overlapping`
- `pracma`
- `dplyr`
- `hrbrthemes`

It is recommended to run the `.ipynb` file using [Google Colab](https://colab.research.google.com/).

