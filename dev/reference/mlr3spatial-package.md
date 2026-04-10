# mlr3spatial: Support for Spatial Objects Within the 'mlr3' Ecosystem

Extends the 'mlr3' ML framework with methods for spatial objects. Data
storage and prediction are supported for packages 'terra', 'raster' and
'stars'.

## Learn mlr3

- Book on mlr3: <https://mlr3book.mlr-org.com>

- Use cases and examples gallery: <https://mlr3gallery.mlr-org.com>

- Cheat Sheets: <https://github.com/mlr-org/mlr3cheatsheets>

## mlr3 extensions

- Preprocessing and machine learning pipelines:
  [mlr3pipelines](https://CRAN.R-project.org/package=mlr3pipelines)

- Analysis of benchmark experiments:
  [mlr3benchmark](https://CRAN.R-project.org/package=mlr3benchmark)

- More classification and regression tasks:
  [mlr3data](https://CRAN.R-project.org/package=mlr3data)

- Connector to [OpenML](https://www.openml.org):
  [mlr3oml](https://CRAN.R-project.org/package=mlr3oml)

- Solid selection of good classification and regression learners:
  [mlr3learners](https://CRAN.R-project.org/package=mlr3learners)

- Even more learners: <https://github.com/mlr-org/mlr3extralearners>

- Tuning of hyperparameters:
  [mlr3tuning](https://CRAN.R-project.org/package=mlr3tuning)

- Hyperband tuner:
  [mlr3hyperband](https://CRAN.R-project.org/package=mlr3hyperband)

- Visualizations for many mlr3 objects:
  [mlr3viz](https://CRAN.R-project.org/package=mlr3viz)

- Survival analysis and probabilistic regression:
  [mlr3proba](https://CRAN.R-project.org/package=mlr3proba)

- Cluster analysis:
  [mlr3cluster](https://CRAN.R-project.org/package=mlr3cluster)

- Feature selection filters:
  [mlr3filters](https://CRAN.R-project.org/package=mlr3filters)

- Feature selection wrappers:
  [mlr3fselect](https://CRAN.R-project.org/package=mlr3fselect)

- Interface to real (out-of-memory) data bases:
  [mlr3db](https://CRAN.R-project.org/package=mlr3db)

- Performance measures as plain functions:
  [mlr3measures](https://CRAN.R-project.org/package=mlr3measures)

## Suggested packages

- Parallelization framework:
  [future](https://CRAN.R-project.org/package=future)

- Progress bars:
  [progressr](https://CRAN.R-project.org/package=progressr)

- Encapsulated evaluation:
  [evaluate](https://CRAN.R-project.org/package=evaluate),
  [callr](https://CRAN.R-project.org/package=callr) (external process)

## Package Options

- `"mlr3.debug"`: If set to `TRUE`, parallelization via
  [future](https://CRAN.R-project.org/package=future) is disabled to
  simplify debugging and provide more concise tracebacks. Note that
  results computed with debug mode enabled use a different seeding
  mechanism and are not reproducible.

- `"mlr3.allow_utf8_names"`: If set to `TRUE`, checks on the feature
  names are relaxed, allowing non-ascii characters in column names. This
  is an experimental and temporal option to pave the way for text
  analysis, and will likely be removed in a future version of the
  package. analysis.

## References

Becker M, Schratz P (2025). *mlr3spatial: Support for Spatial Objects
Within the 'mlr3' Ecosystem*. R package version 0.6.1.9000,
<https://mlr3spatial.mlr-org.com>.

## See also

Useful links:

- <https://mlr3spatial.mlr-org.com>

- <https://github.com/mlr-org/mlr3spatial>

- Report bugs at <https://github.com/mlr-org/mlr3spatial/issues>

## Author

**Maintainer**: Marc Becker <marcbecker@posteo.de>
([ORCID](https://orcid.org/0000-0002-8115-0400))

Authors:

- Patrick Schratz <patrick.schratz@gmail.com>
  ([ORCID](https://orcid.org/0000-0003-0748-6624))
