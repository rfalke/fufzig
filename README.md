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
fufzig [-o DIR] [-r|-r+|-r++] [-i PATTERNS] [-p THREADS] URL
  -o DIR       set the output directory [defaults to '.']
  -r           limit recursive download to the sub directory of the initial URL
  -r+          limit recursive download to host of the initial URL
  -r++         do not limit the recursive download
  -i PATTERNS  only visit urls which match PATTERNS
  -p THREADS   download with THREADS in parallel

PATTERNS consists of a delimeter char and a list of positive and negative
patterns separated by the delimter char. The url is matched against each
pattern and the first match decided (i.e. the url is accepted or rejected).
Empty parts mean no match.
Example: -i ',/good,/subdir/bad,/subdir'
         means accept urls containing '/good' and '/subdir' but not if they 
         also contain '/subdir/bad'.
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
