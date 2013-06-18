# Hacking the build into Travis-CI "C" environment
# Inspired by
#     http://blog.mlin.net/2013/02/testing-ocaml-projects-on-travis-ci.html

DEBS='libgsl0-dev'

# OPAM version to install:
export OPAM_VERSION=1.0.0

# OPAM packages needed to build tests:
export OPAM_PACKAGES='ocamlfind gsl ounit'

# Install OCaml
sudo apt-get update -q -y
sudo apt-get install -q -y ${DEBS}

# Install OPAM
wget http://www.ocamlpro.com/pub/opam_installer.sh
sudo sh ./opam_installer.sh /usr/local/bin 4.00.1
opam init --auto-setup
eval `opam config -env`

# Install packages from OPAM repository.
opam install -q -y ${OPAM_PACKAGES}

# Compile & run tests
./configure --enable-tests && make test
