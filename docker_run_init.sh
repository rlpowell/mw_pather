
set -x
set -e

export PATH=$(stack path --bin-path):$PATH

# Clean up old versions of things
./clean.sh

# Get haste
cd /tmp/
wget http://haste-lang.org/downloads/ghc-7.10/haste-compiler-0.5.5.1_ghc-7.10.3-linux.tar.bz2
tar -xf haste-compiler-0.5.5.1_ghc-7.10.3-linux.tar.bz2

# Install haste
cd /tmp/haste-compiler/
sudo ./install.sh

# Generate the client side .js file
cd ~/src/mw_pather/
hastec mw_pather.hs

# Copy the client side to our web space
mkdir -p /home/rlpowell/public_html/media/public/mw_pather/
cp index.html mw_pather.js /home/rlpowell/public_html/media/public/mw_pather/

# Generate the "server" side mw_pather executable
stack setup
stack install

# Run the executable
cp /home/rlpowell/.local/bin/mw_pather .
./mw_pather
