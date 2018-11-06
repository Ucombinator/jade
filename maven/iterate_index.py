import csv
from typing import Callable


INDEX_FILE = 'index.tsv'


def iterate_index(func: Callable[[str, str, str], None], index_file: str=INDEX_FILE):
    with open(index_file, newline='') as f:
        r = csv.reader(f, delimiter='\t')
        for blob_id, name, size in r:
            func(blob_id, name, size)
