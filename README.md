# wubstuts
Application that tries to find out info (like plugin versions) on websites you are running on your server

The first goal is to find wordpress and drupal sites in a dir and output their version, name and the plugins they use

## Installation
First you checkout the project

    git clone git@github.com:MarkArts/wubstuts.git
    cd wubstuts/

Then make a copy of settings-example.json called example.json and change the file according to your system

    cp config/settings-example.json config/settings,json
    nano settings.json

Then you init a cabal sandbox

    cabal sandbox init

Then

    cabal install

## Running and debugging
To debug the program you can use

    cabal repl

from the root dir

To run the program you first need to build it then execute it

    cabal install
    cabal build
    dist/build/wubstuts/wubstuts