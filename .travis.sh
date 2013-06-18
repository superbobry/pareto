# Hacking the build into Travis-CI "C" environment
# Inspired by
#     http://blog.mlin.net/2013/02/testing-ocaml-projects-on-travis-ci.html

DEBS='camlp4 camlp4-extra libgsl0-dev'

# OPAM version to install:
export OPAM_VERSION=1.0.0

# OPAM packages needed to build tests:
export OPAM_PACKAGES='ocamlfind gsl ounit'

# Install OCaml
sudo apt-get update -q -y
sudo apt-get install -q -y ocaml-nox ${DEBS}

# Install OPAM
curl -L https://github.com/OCamlPro/opam/archive/${OPAM_VERSION}.tar.gz \
    | tar xz -C /tmp
pushd /tmp/opam-${OPAM_VERSION}
./configure
make
sudo make install
opam init --auto-setup
eval `opam config -env`
popd

# Install packages from OPAM repository.
opam install -q -y ${OPAM_PACKAGES}

# Compile & run tests
./configure --enable-tests && make test
