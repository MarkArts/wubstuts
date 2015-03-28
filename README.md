# wubstuts
Application that tries to find out info (like plugin versions) on websites you are running on your server

The first goal is to find wordpress and drupal sites in a dir and output their version, name and the plugins they use

## Installation
First you checkout the project

    git clone git@github.com:MarkArts/wubstuts.git
    cd wubstuts/

Then you init a cabal sandbox

    cabal sandbox init

Then  

    cabal install