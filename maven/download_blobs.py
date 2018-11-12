#!/usr/bin/env python3

import contextlib
import hashlib
import os
import re
import requests
import sys

from os.path import abspath, dirname, isfile, join
from typing import Callable

MAVEN_BUCKET = 'maven-central'
BASE_URL = f'https://storage.googleapis.com/{MAVEN_BUCKET}/'
MD5_RE = re.compile(rb'[0-9a-fA-F]{32}')
OUTPUT = sys.stdout
ERR_OUTPUT = sys.stderr
CHUNK_SIZE = 4096


########
# Custom exception classes
####


class MavenIndexingException(Exception):
    def __init__(self, message: str):
        self.message = message


class DownloadException(MavenIndexingException):
    pass


class NoSuchBlobException(DownloadException):
    def __init__(self):
        super().__init__("Could not download blob.")


class NoSuchMd5BlobException(DownloadException):
    def __init__(self, name: str):
        super().__init__(f"Could not download md5 blob: {name}.")


class RegexException(MavenIndexingException):
    pass


class Md5RegexException(RegexException):
    def __init__(self):
        super().__init__("Could not regex match md5 sum.")


class BrokenMatchException(RegexException):
    def __init__(self):
        super().__init__("Regex match broken.")


class MismatchedMd5Exception(MavenIndexingException):
    def __init__(self, actual: str, expected: str):
        super().__init__(f"Actual md5 ({actual}) did not match expected md5 ({expected}).")


########
# Custom printing functions
####


def show(*args, **kwargs):
    print(*args, file=OUTPUT, **kwargs)


def eshow(*args, **kwargs):
    print(*args, file=ERR_OUTPUT, **kwargs)


########
# Helper functions
####


@contextlib.contextmanager
def error_context(name: str):
    """
    Catches errors raised within the context and prints their messages with the given name prepended. Intended for use
    in a `with` statement, such as:

        with error_context('foo'):
            bar()

    An error raised within the `bar()` function call would be caught, and its message would be printed via `eshow` as:

        > foo\tError message.
    """
    try:
        yield
    except KeyboardInterrupt:
        # Allow a deliberate KeyboardInterrupt to terminate program execution.
        raise
    except DownloadException as e:
        eshow(f"{name}\t{e.message}")
    except Exception as e:
        message = type(e).__name__
        if len(e.args) >= 1:
            message += ": " + e.args[0]
        eshow(f"{name}\t{message}")


def compute_md5_for_file(file_name: str) -> str:
    """
    Given a file, computes that file's md5 sum. Reads the file one chunk at a time.

    :param file_name: the name of file to hash
    :return: a string containing the md5 sum
    """
    md5_hash = hashlib.md5()
    with open(file_name, 'rb') as f:
        for chunk in iter(lambda: f.read(CHUNK_SIZE), b''):
            md5_hash.update(chunk)
    return md5_hash.hexdigest()


def read_md5_from_md5_file(file_name: str) -> str:
    """
    Reads an md5 value from a `.md5` file on Maven. A file `foo.md5` contains the md5 sum for the file `foo` in the same
    directory. The `.md5` files generally contain only an md5 sum or else an md5 sum followed by the file path.

    :param file_name: the name of file to read
    :return: the md5 sum found at the start of that file, if available
    """
    with open(file_name, 'rb') as f:
        contents = f.read()
    expected_md5 = MD5_RE.match(contents)
    if expected_md5 is None:
        raise Md5RegexException()
    expected_md5 = expected_md5.group(0).decode('utf-8')
    if not expected_md5:
        raise BrokenMatchException()
    return expected_md5


def get_from_url(session: requests.Session, url: str) -> requests.Response:
    """
    Produces an HTTP response object obtained by issuing a GET request to the designated URL.

    :param session: the current open HTTP session
    :param url: the URL to fetch from
    :return: a requests.Response object
    """
    request = requests.Request('GET', url)
    prepare = request.prepare()
    response = session.send(prepare)
    return response


def make_dirs_for_path(path: str):
    """
    An emulation of `mkdir -p`. If the designated directory (or any directory between the current directory and that
    directory) exists, no error is raised.

    :param path: the path to build directories to
    """
    os.makedirs(path, exist_ok=True)


def download_file(session: requests.Session, url: str, dest_file: str, on_error: Callable[[], None]):
    """
    Downloads the contents of a given URL and saves them into the designated file. If the status code of the HTTP
    request is anything other than 200, `on_error` will be called. (It is suggested that `on_error` raises an error.)

    :param session: the current open HTTP session
    :param url: the URL to download the contents of
    :param dest_file: the file to save those contents to
    :param on_error: a function to call if the status code does not equal 200
    """
    response = get_from_url(session, url)
    if response.status_code != 200:
        on_error()
    make_dirs_for_path(dirname(dest_file))
    with open(dest_file, 'w') as df:
        df.write(response.text)


def verify_downloaded_blob(blob_file: str, md5_file: str):
    """
    Computes the md5 sum for a file and reads the expected md5 sum from the given `.md5` file and compares the two.
    Raises an error if the sums do not match.

    :param blob_file: the file to compute the md5 sum for
    :param md5_file: the file to read an expected m5 sum from
    """
    actual_md5 = compute_md5_for_file(blob_file)
    expected_md5 = read_md5_from_md5_file(md5_file)
    if actual_md5.lower() != expected_md5.lower():
        raise MismatchedMd5Exception(actual_md5, expected_md5)


def raise_exception(exception: Exception):
    """
    Raises the given exception when called. Used for executing a `raise` statement in a lambda expression.

    :param exception: the exception to raise when called
    """
    raise exception


########
# Primary implementation
####


def download_blobs(blob_names_file: str, download_dir: str):
    """
    Reads names of blobs from the given file and attempts to download each of them, verifying that their actual md5 sums
    match the expected sums given by that file's corresponding `.md5` file. The downloaded contents will be stored in
    the designated `download_dir`, preserving path structure as expected (i.e., blob `/foo/bar/baz.jar` will be saved in
    `download_dir/foo/bar/baz.jar`).

    The `blob_names_file` should just be a text file with a blob name on each line and nothing more.

    :param blob_names_file: the file of blob names, separated by newlines
    :param download_dir: the directory to download the blobs into
    """
    # If no `blob_names_file` is given, read from stdin. Otherwise, open the file.
    if blob_names_file is None:
        f = sys.stdin
    else:
        f = open(blob_names_file)
    download_dir = abspath(download_dir)
    line_no = 0
    session = requests.Session()
    try:
        for raw_line in f:
            line_no += 1
            # Generate blob-related names.
            blob_name = raw_line.strip()
            blob_url = BASE_URL + blob_name
            blob_dest_file = join(download_dir, blob_name)
            # Generate md5-related names.
            md5_name = blob_name + '.md5'
            md5_url = BASE_URL + md5_name
            md5_dest_file = join(download_dir, blob_dest_file + '.md5')
            # Print progress output onto a single line.
            if line_no % 1000 == 0:
                print(f"\rProcessing line: {line_no}", end='')
            # Open an error context for the current blob.
            with error_context(blob_name):
                # Check whether the blob has previously been downloaded. If it has, check for a corresponding md5 file.
                # Download that md5 file if it is missing. If the blob has not been downloaded, download it and its md5.
                if isfile(blob_dest_file):
                    if not isfile(md5_dest_file):
                        download_file(session, md5_url, md5_dest_file,
                                      lambda: raise_exception(NoSuchMd5BlobException(md5_name)))
                else:
                    download_file(session, blob_url, blob_dest_file,
                                  lambda: raise_exception(NoSuchBlobException()))
                    download_file(session, md5_url, md5_dest_file,
                                  lambda: raise_exception(NoSuchMd5BlobException(md5_name)))
                # Verify md5 sums are equivalent.
                verify_downloaded_blob(blob_dest_file, md5_dest_file)
    except Exception:
        show(f"Processing interrupted on line {line_no}.")
        raise
    finally:
        f.close()
        print()


########
# main
####


if __name__ == '__main__':
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument('--blob-names-file', '-f', default=None)
    parser.add_argument('--download-dir', '-d', default='.')
    args = parser.parse_args()

    download_blobs(args.blob_names_file, args.download_dir)
