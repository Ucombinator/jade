# Jade: The Java Decompiler

This is the decompiler portion of the Jade project.

## Requirements

The only requirement is to have a copy of the [Java
JRE](http://www.oracle.com/technetwork/java/javase/downloads/index.html)
installed so that `java` can be run.

Building the tool automatically downloads the other parts that are needed.

## Building

To compile the project for use, simply run the following from the
`decompiler/` folder in your Jade source directory:

```
./bin/sbt assembly
```

## Usage

After building simply run:

```
./bin/jade
```
