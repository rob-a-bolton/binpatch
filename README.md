binpatch
--------

Creates and applies binary patch files.  
Create a patch with e.g.  
`binpatch -c -s path/to/src/file -d path/for/patch_file 13:50 300:10`  
To create the file `path/for/patch_file` that stores bytes 13 through 63 and bytes 300 through 310.  
Apply this patch to another file with `binpatch -a -s path/for/patch_file -d path/of/file_to_patch` to overwrites bytes 13-63 and 300-310 with those that were extracted from the src file and stored in the patch.  

Why does this exist?
--------------------
I wanted to databend some images but by blending multiple audio tracks of the image with different effects applied and mastered at different levels.  
The merging before audacity exports these was destroying the TIFF structure and manually patching these files with a hex editor or head/tail was getting irritating.  

Building
--------
Make sure you have OCaml and dune installed, and run `dune build`.  
Depends on [cmdliner](https://opam.ocaml.org/packages/cmdliner/).

Bugs
----
It'll check the src file is big enough to extract the specified ranges, but won't check on the destination file while patching.  
This shouldn't be a problem because the files should be of equal length if you're using this for what I am, but if it becomes a problem them submit an issue and I'll get around to fixing it. Patch is, as always, welcome.

License: GPLv3
