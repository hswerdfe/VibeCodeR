# VibeCodeR

**VibeCodeR** is an RStudio add-in package designed to improve your development workflow by integrating useful RStudio add-ins powered by R functions and user-friendly interfaces. It provides tools to generate function skeletons, insert Roxygen documentation, and streamline code editing with interactive support for large language models (LLMs).

## Key Features

### 🔧 RStudio Add-ins

This package includes the following RStudio add-ins:

- **Generate Roxygen Comment**  
  Inserts a Roxygen skeleton above the current function to facilitate documentation.

- **Generate Function**  
  Prompts the user to input specifications, then inserts a function template at the current cursor position. *(Interactive)*

- **Generate Changes**  
  Allows the user to describe code changes, preview the suggested modifications, and approve updates. *(Interactive)*

- **VibeCodeR Defaults**  
  Generates or checks default project configurations for VibeCodeR. *(Interactive)*


### 📁 Custom Project Template

**VibeCodeR** registers a project template in RStudio's **New Project** wizard. The template includes:

- Default `Rproj` configuration
- Sample skeleton files
- Custom icons and resources

This template is ideal for testing new workflows involving large language models (LLMs).


## Installation

You can install the development version from GitHub using:

```r
# Install from GitHub
# install.packages("devtools")
devtools::install_github("hswerdfe/VibeCodeR")
```

Make sure you have the devtools package installed.

## Usage

### 🧩 Accessing Add-ins
Once installed, open RStudio and navigate to:

```r
Addins > [Select an add-in from the VibeCodeR package]
```
or use the Command Palette (`Ctrl + Shift + P`), and type `VibeCodeR` then select the action you would like.


Follow the prompts in the add-in to insert templates or perform the desired action.


### ➕ Using the Project Template

 #. Open RStudio.
 #. Click `File > New Project`.
 #. Choose `New Directory > VibeCodeR Project`.
 #. Complete the setup using the provided wizard


## Dependencies

This package depends on:

 * `diffobj`, `ellmer`, `glue`, `here`,  `janitor`, `magrittr`, `miniUI`, `purrr`, `rstudioapi`, `shiny`,  `snakecase`, `sodium`, `stringr`, `tibble`, `utils` 

These will be installed automatically if not already present.


## Development

To run tests:

```r
library(testthat)
testthat::test_package("VibeCodeR")
```

## Limitations

- **Dependency on `ellmer`:**  
  VibeCodeR currently relies heavily on the [`ellmer`](https://ellmer.tidyverse.org/) package for interfacing with large language models. While this provides a solid foundation, we do not yet allow users to customize LLM parameters such as temperature, max tokens, or model type—these settings default to what's configured in `ellmer`.

- **No Built-in LLM Configuration:**  
  The package does not offer a built-in way to modify LLM parameters via the add-in interface or function calls. All LLM behavior depends on `ellmer`'s internal defaults.

- **Environment Variables Required:**  
  Users must provide access credentials for any LLM APIs (e.g., OpenAI) by setting the appropriate environment variables (e.g., `GEMINI_API_KEY`). These are not bundled with the package and must be configured by the user before use.




## File Structure Highlights
 * `inst/rstudio/addins.dcf`: Registers the RStudio add-ins.
 * `R/`: Contains the R functions backing the add-ins.
 * `inst/rstudio/templates/`: Holds default project template files and resources.

## Contributing

Contributions are welcome! If you'd like to add features or fix bugs, please submit a pull request or open an issue.

## License

This package is licensed under the MIT License. See LICENSE for details.
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)


## Author
 Howard Swerdfeger