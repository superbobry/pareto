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

# HACK(superbobry): make sure 'opam_installer.sh' won't initialized
# OPAM for us.
mkdir $HOME/.opam
sudo sh ./opam_installer.sh /usr/local/bin
rmdir $HOME/.opam
opam init --comp=4.00.1 --auto-setup -q -y
eval `opam config -env`

# Install packages from OPAM repository.
opam install -q -y ${OPAM_PACKAGES}

# Compile & run tests
./configure --enable-tests && make test
