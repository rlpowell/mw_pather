
set -x
set -e

export PATH=$(stack path --bin-path):$PATH

# Clean up old versions of things
./clean.sh

cd ~/src/mw_pather/

# Link to the cache already in docker
rm -rf .stack-work
ln -s /tmp/mw_pather/.stack-work .stack-work

# Generate the client side .js file
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
