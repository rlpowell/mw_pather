
set -x
set -e

# Set our path to use the right stack
export PATH=/home/rlpowell/.local/bin/:$PATH
# Then use it to add more things to the path
export PATH=$(stack path --bin-path):$PATH

# Clean up old versions of things
./clean.sh

cd ~/src/mw_pather/

# Generate the client side .js file
hastec mw_pather.hs

# Copy the client side to our web space
mkdir -p /home/rlpowell/public_html/media/public/mw_pather/
cp index.html mw_pather.js /home/rlpowell/public_html/media/public/mw_pather/

# Generate the "server" side mw_pather executable
stack --version
stack setup
stack install

# Run the executable
cp /home/rlpowell/.local/bin/mw-pather .
./mw-pather
