fufzig
======

Fufzig is a flexible url fetcher like wget written in Erlang

## Usage

``` bash
$ make
```

## Usage

``` bash
$ ./fufzig 
fufzig [-o DIR] [-r|-r+|-r++] URL
  -o   DIR to set the output directory
  -r   limit recursive download to the sub directory of the initial URL
  -r+  limit recursive download to host of the initial URL
  -r++ do not limit the recursive download
```

# Examples

* Download single file 
``` bash
$ ./fufzig www.example.com
```

* Dive into sub directories 
``` bash
$ ./fufzig -o output_dir -r http://www.example.com/some/path
```

* Fetch all references files from the host 
``` bash
$ ./fufzig -o output_dir -r+ http://www.example.com/some/path
```

* Fetch all references files from all hosts 
``` bash
$ ./fufzig -o output_dir -r++ http://www.example.com/some/path
```
