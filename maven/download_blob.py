#!/usr/bin/env python3

from os import makedirs
from os.path import basename, dirname, isfile, join
from google.cloud import storage

MAVEN_BUCKET = 'maven-central'


def download_blob(name: str, dest_file: str, auth_file: str):
    client = storage.Client.from_service_account_json(auth_file)
    bucket = client.get_bucket(MAVEN_BUCKET)
    blob = bucket.get_blob(name)
    blob.download_to_filename(dest_file)


def get_dest_file(dest_file: str, dest_dir: str, name: str) -> str:
    if not dest_file:
        dest_file = name
    dest_path = join(dest_dir, dest_file)
    if isfile(dest_path):
        raise RuntimeError(f"File already exists with name: {dest_path}")
    makedirs(dirname(dest_path), exist_ok=True)
    return dest_path


if __name__ == '__main__':
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument('auth_file')
    parser.add_argument('blob_name')
    parser.add_argument('dest_file', nargs='?')
    parser.add_argument('-d', '--dest-dir', default='.')
    args = parser.parse_args()

    dest_name = get_dest_file(args.dest_file, args.dest_dir, args.blob_name)
    download_blob(args.blob_name, dest_name, args.auth_file)
