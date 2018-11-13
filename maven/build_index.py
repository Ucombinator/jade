#!/usr/bin/env python3

from os.path import isfile
from google.cloud import storage

MAVEN_BUCKET = 'maven-central'


def get_maven_bucket(auth_file: str) -> storage.Bucket:
    """
    Obtains a connection to the Maven bucket on Google Storage.

    :param auth_file: a `.json` file containing the Google Storage authentication
    :return: a Bucket object
    """
    client = storage.Client.from_service_account_json(auth_file)
    return client.get_bucket(MAVEN_BUCKET)


def index_blobs(auth_file: str, index_file: str, max_results: int=None):
    """
    Iterates through up to `max_results` blobs in the Maven bucket and writes their information to file.

    :param auth_file: a `.json` file containing the Google Storage authentication
    :param index_file: the file name to write the output to
    :param max_results: maximum number of blobs to index; give `None` to index all blobs
    """
    if isfile(index_file):
        raise RuntimeError(f"Index file already exists at: {index_file}.")
    bucket = get_maven_bucket(auth_file)
    blob_no = 0
    with open(index_file, 'w') as f:
        for blob in bucket.list_blobs(max_results=max_results):
            blob_no += 1
            f.write(f"{blob_no}\t{blob.name}\t{blob.size}\n")


if __name__ == '__main__':
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument('auth_file')
    parser.add_argument('index_file', nargs='?', default='index.tsv')
    parser.add_argument('--max-results', type=int, default=None)
    args = parser.parse_args()

    index_blobs(args.auth_file, args.index_file, args.max_results)
