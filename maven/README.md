# Parsing Maven

These notes were started after a two-month hiatus from working on the project. They represent an effort to re-learn all
of the things I forgot to write down previously.

## Project overview

Our end-goal is to have a snapshot copy of every artifact in Maven. To accomplish this, we first need to determine how
much space we will need. My notes indicate that a full clone of every artifact in Maven will take some 10 TiB total,
which is not prohibitive in itself but is certainly not ideal.

To that end, we've decided to reduce the number of artifacts we will consider. Specifically, the goal now is to just
obtain a working snapshot of the *latest* version of each artifact. This actually requires two things:

1. Obtain the latest version of every artifact available.
2. Satisfy the prerequisites for each of those artifacts.

Of course, we don't want to actually download everything up-front, so our plan is to obtain all of the `.pom` files and
`maven-metadata.xml` files to be able to build a dependency graph between all of the available artifacts. These
documents should be sufficient for us to determine exactly which artifacts to download, which means we can minimize our
space requirements as well as the time spent downloading artifacts.

## Parsing Maven metadata files

[Here is a reference for `maven-metadata.xml` files.](http://maven.apache.org/ref/3.2.5/maven-repository-metadata/index.html)

[Here is the detailed reference for various tags.](http://maven.apache.org/ref/3.2.5/maven-repository-metadata/repository-metadata.html)

The `maven-metadata.xml` file is an XML document that provides some high-level organizational information about a
project or artifact. Specifically, it can tell us which artifact in a project is the most recent version.

We should be extracting the latest version by looking at:

```
<metadata ...>
  ...
  <versioning>
    ...
    <release>x.x.x</release>
    ...
  </versioning>
  ...
</metadata>
``` 

This should give us the latest release version of an artifact. Some `maven-metadata.xml` files do not seem to specify
this field, and we are unsure whether this is always intended. (There seem to be some repositories on Maven which do not
always follow the conventions.)

TODO: Finish description of metadata file parsing.

## Parsing POM files

[Here is a reference document for POM files.](https://maven.apache.org/pom.html)

Essentially, a POM file is an XML document that provides some information about a project. All projects must have an
associated POM file for each artifact.

We can use POM files to determine the dependencies for a given artifact. These should be specified as a list in the
format of:

```
<project ...>
  ...
  <dependencies>
    ...
      <dependency>
        <groupId>group</groupId>
        <artifactId>artifact</artifactId> 
        <version>x.x.x</version>
        ...
      </dependency>
    ...
  </dependencies>
  ...
</project>
```

This would specify that the current project has a dependency on `group.artifact-x.x.x`.

POM files can inherit from parent POM files. So, for example, a project could define a POM file and each of its
artifacts can then inherit from it. This is specified by use of the `<parent>` tag.

TODO: Finish description of POM parsing.

## Python scripts and modules

Here, I will document the various Python scripts and modules used in this project and their intended uses.

### Brief overview

| File Name                             | Short Description |
|---------------------------------------|-------------------|
| `build_index.py`                      | Writes blob numbers, names, and sizes to file for each blob in the maven-central repository. |
| `check_basenames.py`                  | Computes a diff for two lists, where the second list is meant to be a subset of the first list. |
| `download_blob.py`                    | Downloads a blob from maven-central by name. | 
| `download_blobs.py`                   | Attempts to download a list of blobs, verifying their checksums along the way. |
| `extract_latest_version_pom_names.py` | Attempts to determine the filename of the POM for the latest version of each project based on the `maven-metadata.xml` files. |
| `iterate_index.py`                    | Provides a helper function to iterate over the index file built with `build_index.py`. |
| `organize_poms.py`                    | Moves downloaded files into a proper nested directory structure (since Google Cloud's download functionality puts everything in a flat directory). |
| `suffix_trie.py`                      | Provides an implementation of a TrieNode, intended for analyzing the filenames of files stored in Maven. |
| `uniqsemisort.py`                     | Sort-of sorts input lines lexicographically, but has a fixed buffer size to handle large file inputs. |

### Detailed overview

#### `build_index.py`

Takes in a Google Cloud Storage (GCS) authorization file (TODO: find link for this) and writes a list of every blob in
the Maven Central repository ("bucket"). The information written is just each blob's number in the bucket, its file
name, and its size in bytes. The file is output as a tab-separated file (`.tsv`) such as:

```
1\tREADME.md\t1234
2\tindex.html\t2222
...
```

This file is intended to be used as a snapshot of the current state of the Maven Central repository. We use this 
throughout the process to do things like build a list of all `maven-metadata.xml` files, determine the total size of all
blobs, and more.

Usage is straightforward. Acquire a GCS auth file for your GCS account (necessary), and then call this program as:

```
./build_index.py path/to/auth-file.json [path/to/desired/index.tsv] [--max-results INT]
```

#### `check_basenames.py`

TODO: Write this description.

#### `download_blob.py`

Downloads a specified blob from the Maven Central repository. This is mostly intended for testing.

#### `download_blobs.py`

Downloads a list of blobs from the Maven Central repository. This can be used to download the whole repository, or any
subset for which you have a list of file names.

GCS provides their own download tool, but unfortunately it does not maintain directory structure. This is particularly
irksome when dealing with files such as `maven-metadata.xml`, as you will only end up with one copy of that file
(despite there being many thousands throughout the repository). The `download_blobs.py` script will recreate the
desired directory structure automatically, though this comes at the cost of speed.

#### `extract_latest_version_pom_names.py`

This script attempts to determine the latest version of a project by analyzing the project's `maven-metadata.xml` file.
The XML files must have already been downloaded. You can provide a list of all the XML files you want it to analyze,
allowing for small batch testing.

#### `iterate_index.py`

This module merely provides a sort of for-each function which will apply a passed-in function to each element of the
index file created by `build_index.py`.

#### `organize_poms.py`

The GCS-provided command-line download utility places all downloaded files into a single flat directory. This can
complicate a number of things in our workflow. However, the utility is useful because it can handle concurrent
downloads and can pause/resume.

After using the utility to download all of the POM files, this script can reorganize those files into their desired
original directory structure.

#### `suffix_trie.py`

This module provides classes that can be used to read the index built with `build_index.py` and construct a trie based
on the file names. This is useful for determining how many of each kind of file there are, and their summed sizes.

#### `uniqsemisort.py`

We wanted to determine whether there were any missing `.md5` or `.sha1` files (or checksum files for which there are no
base files). To do this, we took the index built with `build_index.py` and truncated the filenames to remove any `.md5`
or `.sha1` extensions. However, this produces a file with duplicate file names that are not quite sorted any more:

```
foo.bar             -> foo.bar
foo.bar.asc         -> foo.bar.asc
foo.bar.asc.md5     -> foo.bar.asc
foo.bar.asc.sha1    -> foo.bar.asc
foo.bar.md5         -> foo.bar
foo.bar.sha1        -> foo.bar
```

This script provides a custom semi-sort, which only sorts lines against a relatively small buffer of recent lines. This
means it is significantly faster than true-sorting the entire file and then removing duplicates.

(We later determined that the `uniq` utility can be specified to handle this for us and it's considerably faster than
our implementation... but that's neither here nor there.)
