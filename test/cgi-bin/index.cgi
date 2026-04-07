#!/bin/sh

echo "Content-Type: text/plain"
echo "Status: 200"
echo ""
if test -n "$PATH_INFO"; then
    echo "PATH_INFO: $PATH_INFO"
else
    echo "index.cgi"
fi
