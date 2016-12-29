docker create -v /home/rlpowell/.stack --name volume--stack--global rlpowell/shell /bin/true
docker create -v /home/rlpowell/.local --name volume--local--mw_pather rlpowell/shell /bin/true
docker create -v /home/rlpowell/src/mw_pather/.stack-work --name volume--stack-work--mw_pather rlpowell/shell /bin/true
