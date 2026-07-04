# Contributing to rfcipDemand

Thank you for considering a contribution. Bug reports, feature suggestions,
and pull requests are welcome via
[GitHub issues](https://github.com/ftsiboe/rfcipDemand/issues) and pull
requests.

## Contributor License Agreement (required)

All contributions require agreement to the project's
[Contributor License Agreement](CLA.md). In short: you keep ownership of
your work, and you grant the maintainer (TERRA ANALYTICS LLC) a broad
license to it — including the right to sublicense — so the project retains
the flexibility to be offered under additional license terms in the future.

To agree, include this line in the description of your first pull request:

> I have read and agree to the rfcipDemand CLA (CLA.md), and this agreement
> covers this and my future contributions to the project.

Pull requests without CLA agreement cannot be merged. If you are
contributing on behalf of an employer, please confirm in the same statement
that you are authorized to do so.

## Practical guidelines

Development follows standard R-package practice: run
`devtools::document()`, `devtools::test()`, and `devtools::check()` before
opening a pull request; new functionality should come with tests in
`tests/testthat/` (offline — mock network access, see the existing test
files for patterns); and data.table column names used non-standardly should
be added to `R/PACKAGE_GLOBALVARIABLES.R`.

The `data-raw/` tree contains the research workflows that build the
package's released data and the accompanying manuscript; changes there are
generally reserved for the maintainer, but issues pointing out problems are
very welcome.

Please also note the project [code of conduct](code_of_conduct.md) and
[security policy](SECURITY.md).
