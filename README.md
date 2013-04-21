fufzig
======

Fufzig is a flexible url fetcher like wget written in Erlang

Usage
=====

First you have to compile the erl files to beam files:
    ```shell
    $ make
    ```

Than you can run the program:
    ```shell
    $ ./fufzig
    $ ./fufzig www.example.com
    $ ./fufzig -o output_dir -r+ http://www.example.com/some/path
    $ ./fufzig -o output_dir -r++ http://www.example.com/some/path
    ```
