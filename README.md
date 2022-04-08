
&nbsp;

---
# **Snakemake Template** 


&nbsp;
&nbsp;


---

# **Requirements**
- miniconda (https://docs.conda.io/en/latest/miniconda.html)
- a conda environment containing the latest version of snakemake
---

&nbsp;


---
# **How to run the pipeline**

## **Run locally**
- For running the snakemake pipeline locally, we can first perform a dry run using this command:
```
snakemake --profile profile/ --cores <nb_cores> -np
```
- If no error messages are displayed (in red), we can run the pipeline using this command:
 ```
snakemake --profile profile/ --cores <nb_cores>
```

## **Run in a cluster**
- For running the snakemake pipeline in a cluster, we can first perform a dry run using this command:
```
snakemake --profile profile_slurm/ -np
```
- If no error messages are displayed (in red), we can run the pipeline using this command:
 ```
snakemake --profile profile_slurm/
```

---

