# R-package for validating data using DataHarmonizer schemas

This package provides functionality in R for parsing DataHarmonizer templates and validating data using the schema's rules.

## Usage:

First, acquire the schema.yaml file from the DataHarmonizer templates. 
Next, load the schema into R:

```{r}
schema <- load_schema(file = "path/to/schema.yaml")
```

Import the data you'd like to validate into a dataframe in R, and validate it:

```{r}
df <- read.csv(file = "/path/to/datafile.csv"
validate(schema, df)
```

This will print a list of validation passes and failures.
