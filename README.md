![pareto](http://upload.wikimedia.org/wikipedia/commons/5/55/Vilfredo_F._D._Pareto.jpg)

Installation
------------

Make sure you have `gsl-ocaml`, or install it with [OPAM](http://opam.ocamlpro.com):

```bash
$ opam install gsl
```

Then go with the usual OASIS routines:

```bash
$ ./configure --enable-examples
$ make  # And you're done!
```

Examples
--------

Examples for different `pareto` modules can be found in `examples/`
directory. Here's a simple t-test:

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
