# ready4

``` r
library(ready4)
```

## Motivation

Health economic models should ideally be both “living” (kept up to date)
and “transferable” (easy to selectively modify for use in a new context)
and applied in research that is reproducible. These goals [can be hard
to achieve in
practice](https://www.ready4-dev.com/docs/getting-started/motivation/).

## Implementation

The `ready4` R library provides bare bones foundational elements of a
[prototype software
framework](https://link.springer.com/article/10.1007/s40273-024-01378-8)
to: (i) develop living and transferable health economic models; and (ii)
apply those models to implement reproducible health economic analyses.
The framework is extended by multiple other R packages - see the
framework’s [project website](https://www.ready4-dev.com/) for details.

## Use

For most users, the main applications of the ready4 library will be to:

- [find themed collections of health economic model modules that have
  been developed with the ready4
  framework](https://ready4-dev.github.io/ready4/articles/V_04.md);

- [itemise available model modules and sub-modules and find examples of
  how to use
  them](https://ready4-dev.github.io/ready4/articles/V_05.md); and

- [apply these modules to undertake health economic analyses using a
  simple and consistent
  syntax](https://ready4-dev.github.io/ready4/articles/V_02.md).

For the above purposes, the ready4 library will be typically used in
conjunction with the
[ready4use](https://ready4-dev.github.io/ready4use/) and
[ready4show](https://ready4-dev.github.io/ready4show/) libraries.

For users with software development expertise, the ready4 library also
provides:

- [a template model
  module](https://ready4-dev.github.io/ready4/articles/V_01.md) that
  facilitates modular implementations of health economic models using an
  [object-oriented
  paradigm](https://ready4-dev.github.io/ready4/articles/V_03.md);

- [a simple programming syntax to make available to
  end-users](https://ready4-dev.github.io/ready4/articles/V_07.md); and

- [tools to partially automate the maintenance of an open source
  modelling project’s
  website](https://ready4-dev.github.io/ready4/articles/V_06.md).

Developers that wish to author health economic models as R libraries
with ready4 may wish to consider using the
[ready4pack](https://ready4-dev.github.io/ready4pack/) library. However,
that library (along with its two primary dependencies
[ready4fun](https://ready4-dev.github.io/ready4fun/) and
[ready4class](https://ready4-dev.github.io/ready4class/)) is not yet
well documented and the ready4 framework remains a prototype. It is
therefore recommended to [first contact the package
authors](https://mph-economist.netlify.app/#contact) before undertaking
R library development with ready4.

## Further reading

- A [pre-print manuscript](https://arxiv.org/pdf/2403.17798) discussing
  the need for health economic models to be transparent, reusable and
  updatable;

- An 2024 [article in
  Pharmacoeconomics](https://link.springer.com/article/10.1007/s40273-024-01378-8)
  describing the ready4 prototype software framework;

- The [ready4 prototype software framework
  website](https://www.ready4-dev.com/);

- The [project website of the readyforwhatsnext
  model](https://readyforwhatsnext.com/) that is being developed with
  the ready4 software framework; and

- A [pre-print
  manuscript](https://www.medrxiv.org/content/10.1101/2021.07.07.21260129v4)
  describing a utility mapping study implemented with modules of the
  readyforwhatsnext model.
