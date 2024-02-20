# Build

First, you need to load the git submodules.

```
make submodules
```

If you only want to use `Lily` locally to try it out, run:<br>
NOTE: Currently there is no script to install Lily on the machine.

```bash
./scripts/patches/enable_local.sh
```

## Linux

Make sure you are using `Clang` 16 as your default compiler, if not, run:

```bash
export CC=clang # or clang-16
export CXX=clang++ # or clang++-16
```

After launching the CMake configuration:

```bash
make configure # We use Ninja
```

Then, the build:

```bash
# If you don't have a very powerful machine or are already using a
# lot of resources, it's worth trying to add a restriction on the
# number of possible tasks, with the -j option.
ninja -C build -j <n_jobs>
```

Launch `lilyc`:<br>
NOTE: This is the command to use the Lily compiler.

```bash
./bin/lilyc -h # local version
lilyc -h # installed version
```

Launch `lily`:<br>
NOTE: This is the command to use some utility tools for the Lily language (build, init, new, ...).

```bash
./bin/lily -h  # local version
lily -h # installed version
```

## MacOS

Make sure you are using `Clang` 16 as your default compiler, if not, run:

```bash
# Brew installation
export CC=$(brew --prefix llvm@16)/bin/clang 
export CXX=$(brew --prefix llvm@16)/bin/clang++ 

# No-brew installation
export CC=clang
export CXX=clang++ 
```

After launching the CMake configuration:

```bash
make configure # We use Ninja
```

If LLVM is not found by CMake run:

```bash
# Brew installation
export LLVM_DIR=$(brew --prefix llvm@16)
```

Then, the build:

```bash
# If you don't have a very powerful machine or are already using a
# lot of resources, it's worth trying to add a restriction on the
# number of possible tasks, with the -j option.
ninja -C build -j <n_jobs>
```

Launch `lilyc`:<br>
NOTE: This is the command to use the Lily compiler.

```bash
./bin/lilyc -h # local version
lilyc -h # installed version
```

Launch `lily`:<br>
NOTE: This is the command to use some utility tools for the Lily language (build, init, new, ...).

```bash
./bin/lily -h  # local version
lily -h # installed version
```

## Windows

...

## BSD

...
