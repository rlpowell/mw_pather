sudo docker kill mw_pather
sudo docker rm mw_pather
sudo docker rmi rlpowell/mw_pather
sudo docker build -t rlpowell/mw_pather .

# Run the docker to generate the javascript client side, and
# generate and run the executable/Haskell server side
sudo docker run --name mw_pather -v /var/run/docker.sock:/var/run/docker.sock --volumes-from volume--stack--global --volumes-from volume--local--stack_all --volumes-from volume--stack-work--stack_all -p 0.0.0.0:2225:22 -p 0.0.0.0:8085:8085 -v /home/rlpowell/src/mw_pather/:/home/rlpowell/src/mw_pather/:rw -v /home/rlpowell/public_html/:/home/rlpowell/public_html:rw -p 0.0.0.0:24601:24601 -t -i rlpowell/mw_pather bash -x /tmp/docker_run_init.sh
