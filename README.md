# juxt

I determined that most people who were given this task would probably opt for some kind of web backend using some sort of SQL database. I decided that I wanted to do something different. This conception of the problem writes and reads from json (for persistent storage) and uses the data as an environment inside of a REPL interpreter that takes simple commands, and then writes to the json file at the end. Obviously this has major disadvantages when it comes to scalability but I decided this was the most interesting way to solve the problem in-memory for a small-scale application that won't really be taking any asynchronous requests. This application could be expanded into a miniature database language, but the parsing is currently obviously very primitive.

## Installation

Utilises only standard library, so no install is needed.

## Usage

$ lein run

Starts the REPL.

Commands:

INSERT (takes flight data, and inserts into the environment)
UPDATE (takes flight data, updates the environment. Actually - it just calls insert!)
RETRIEVE (takes an even number of arguments, including 0. Filters the environment by the conditions and prints the result E.G., RETRIEVE ID F222 will return all flights with that ID.)
REMOVE (takes one argument - ID, and removes it from the environment)
DONE (writes the new environment to json and closes the REPL)
