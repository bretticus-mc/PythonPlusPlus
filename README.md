# Python++
### Build the PythonPP compiler

```
ocamlbuild -pkgs llvm pythonpp.native
```

### Run the Python compiler and generate llvm code
```
./pythonpp.native -l example.py > example.out
```

### Run the llvm code
```
lli example.out
```