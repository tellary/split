#!/bin/bash

DIR=$(dirname $0)
pushd $DIR
npx @automerge/automerge-repo-sync-server
popd
