██████╗ ███████╗ █████╗ ██████╗ ███╗   ███╗███████╗
██╔══██╗██╔════╝██╔══██╗██╔══██╗████╗ ████║██╔════╝
██████╔╝█████╗  ███████║██║  ██║██╔████╔██║█████╗
██╔══██╗██╔══╝  ██╔══██║██║  ██║██║╚██╔╝██║██╔══╝
██║  ██║███████╗██║  ██║██████╔╝██║ ╚═╝ ██║███████╗
╚═╝  ╚═╝╚══════╝╚═╝  ╚═╝╚═════╝ ╚═╝     ╚═╝╚══════╝


\\Requirements:

8BallPool was developed as a final project for CS 3110, and as such we are assuming ocaml and opam are already installed in the system. The only other dependencies are js_of_ocaml and its related packages, which you can install by running
`opam install js_of_ocaml js_of_ocaml-ppx js_of_ocaml-lwt js_of_ocaml-ocamlbuild`

\\How to compile:

We've provided a Makefile that handles building and compiling the program. The command to run is `make demo`. As our pool engine is a web app, you can access the game by opening index.html is a web browser of your choice as long as it has HTML canvas support. We recommend Google Chrome as this was developed and tested on Chrome.

After these steps, you should be good to go! Have fun!
