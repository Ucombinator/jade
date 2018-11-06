#!/usr/bin/env python3

"""
The index contains all files in the Maven repository. We want to produce a new list of all the file names we would
expect to see if we took the hash files (*.md5 and *.sha1) and stripped their extensions. This creates a sorting problem
when *.asc files are involved, though.

Consider the following transformation from full path names to path names with *.md5 and *.sha1 stripped:

    foo.bar             -> foo.bar
    foo.bar.asc         -> foo.bar.asc
    foo.bar.asc.md5     -> foo.bar.asc
    foo.bar.asc.sha1    -> foo.bar.asc
    foo.bar.md5         -> foo.bar
    foo.bar.sha1        -> foo.bar

The `uniq` command expects the input to be sorted, but this input is not sorted. Unfortunately the output of simply
stripping the hash extensions is 36M lines, which makes first using `sort` prohibitively expensive in terms of time.

Solutions:

1. Run `sort` and then run `uniq`. It'll take a while, but it'll finish eventually.
2. Put every file name into a set, then sort the set and write the results.
3. Implement a custom `uniq`-like function which keeps a buffer to look back and ensure the element is placed in the
   correct place.
4. Implement a custom `uniq` and `sort` in a single pass.

We're going with option 3 for now.
"""

import sys


def uniqsemisort(basename_file: str, buffer_length: int):
    buffer = []

    def evict():
        print(buffer.pop(0))

    def process_line(line: str):
        if not buffer:
            buffer.append(line)
            return
        i = -1
        while abs(i) <= len(buffer) and line < buffer[i]:
            i -= 1
        if abs(i) > len(buffer):
            raise RuntimeError(f"Could not process line '{line}'")
        if line == buffer[i]:
            # If the lines are equal then we don't need to add it again. This is the `uniq` portion of the sort.
            return
        else:
            if i + 1 == 0:
                buffer.append(line)
            else:
                buffer.insert(i + 1, line)
            # If we're beyond the maximum length, perform eviction.
            if len(buffer) > buffer_length:
                evict()

    line_no = 0
    if basename_file is not None:
        bf = open(basename_file)
    else:
        bf = sys.stdin
    try:
        for raw_line in bf:
            process_line(raw_line.strip())
            line_no += 1
    except:
        print(file=sys.stderr)
        print(f"Processed {line_no} lines before being interrupted.", file=sys.stderr)
        print(file=sys.stderr)
        raise
    finally:
        bf.close()

    while buffer:
        evict()


if __name__ == '__main__':
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument('--basename-file', '-f', default=None)
    parser.add_argument('--max-buffer-length', '-l', type=int, default=100)
    args = parser.parse_args()

    uniqsemisort(args.basename_file, args.max_buffer_length)
