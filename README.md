![pareto](http://upload.wikimedia.org/wikipedia/commons/5/55/Vilfredo_F._D._Pareto.jpg)

`pareto` is an OCaml statistics library, based on [GSL] [gsl], which provides:

* Common statistical tests for significant differences between samples.
* Uniform interface for common discrete and continuous probability distributions.
* Sample statistics, quantile estimation, kernel density estimation.
* Resampling methods: jackknife, BCa bootstrap.

**Note**: before using `pareto` make sure you understand some of the subtleties
in OCaml GSL bindinds. See section **ERROR HANDLING** in GSL [README] [README].

[gsl]: http://www.gnu.org/software/gsl
[README]: https://bitbucket.org/mmottl/gsl-ocaml

Installation
------------

Make sure you have `gsl-ocaml`, or install it with [OPAM](http://opam.ocamlpro.com):

```bash
$ opam install gsl
```

Then go with the usual OASIS routines:

```bash
$ ./configure
$ make  # And you're done!
```

### Examples _(optional)_

To build examples:

```bash
$ ./configure --enable-examples
$ make
```

Here's a simple t-test:

```ocaml
open Statistics

let open Distributions.Normal in
let v = sample ~size:10 standard in
let open Tests in
let { test_statistic = t; test_pvalue } =
  T.one_sample v ~mean:0. ~alternative:TwoSided ()
in begin
  printf "One-sample T-test for true mean = 0.0\n";
  printf "t = %f, P-value: %f\n" t test_pvalue;
end
```

### Documentation _(optional)_

To build API documentation:

```bash
$ make doc
```

### Tests _(optional)_ [![Build Status][status-image]][status]

To build and run tests:

```bash
$ ./configure --enable-tests
$ make test
```

[status]: https://drone.io/github.com/superbobry/pareto/latest
[status-image]: https://drone.io/github.com/superbobry/pareto/status.png
