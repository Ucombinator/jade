import csv
import pickle
import re
from typing import Dict, Optional, Set, Sequence


class TrieNode:
    def __init__(self, segment: Optional[str], parent: Optional['TrieNode']):
        self.segment = segment
        self.parent = parent
        self.children: Dict[str, TrieNode] = {}
        self.leaves: Dict[int, int] = {}
        self.min_size = float('inf')
        self.max_size = 0

    def __getitem__(self, key: str) -> 'TrieNode':
        return self.children[key]

    def __len__(self) -> int:
        return len(self.children)

    @property
    def is_leaf(self) -> bool:
        return bool(self.leaves)

    def count_leaves(self) -> int:
        return len(self.leaves) + sum(map(lambda n: n.count_leaves(), self.children.values()))

    def update_min_size(self, potential_new_min_size: int):
        self.min_size = min(self.min_size, potential_new_min_size)

    def update_max_size(self, potential_new_max_size: int):
        self.max_size = max(self.max_size, potential_new_max_size)

    def update_sizes(self, size: int):
        self.update_min_size(size)
        self.update_max_size(size)

    def add_child(self, child: 'TrieNode'):
        if child.segment in self.children:
            # Fail quietly when duplicate child is created. Otherwise, raise an error.
            if self.children[child.segment] == child:
                return
            else:
                print(f"Could not add {type(child).__name__} because {type(self.children[child.segment]).__name__} "
                      f"already exists at path: {child.segment}/{self.get_full_name()}")
                return
                # TODO: Fix this.
                # raise ValueError(f"Different child already exists for: {child.segment}/{self.get_full_name()}")
        self.children[child.segment] = child

    def add_leaf(self, blob_id: int, size: int):
        self.leaves[blob_id] = size

    def add_descendant_leaf(self, segments: Sequence[str], blob_id: int, size: int, reverse: bool=True):
        if reverse:
            segments = reversed(segments)
        node = self
        for segment in segments:
            if segment in node.children:
                node = node.children[segment]
                continue
            child = TrieNode(segment, node)
            node.add_child(child)
            node.update_sizes(size)
            node = child
        node.add_leaf(blob_id, size)

    def get_full_name(self) -> str:
        node = self
        parts = []
        while node is not None and node.segment is not None:  # The root node has a None segment and a None parent.
            parts.append(node.segment)
            node = node.parent
        return '/'.join(parts)

    def get_depth(self) -> int:
        depth = 0
        node = self
        while node is not None:
            depth += 1
            node = node.parent
        return depth

    def dump_to_file(self, filename: str):
        with open(filename, 'wb') as f:
            pickle.dump(self, f)

    @staticmethod
    def load_from_file(filename: str) -> 'TrieNode':
        with open(filename, 'rb') as f:
            node = pickle.load(f)
        assert isinstance(node, TrieNode)
        return node


class RootNode(TrieNode):
    def __init__(self):
        super().__init__(None, None)


def build_trie_from_index(index_file: str, exclusions: Set[str]=None, reverse: bool=True) -> TrieNode:
    if exclusions is None:
        exclusions = set()
    root = RootNode()
    line_no = 0
    try:
        with open(index_file) as f:
            reader = csv.reader(f, delimiter='\t')
            for blob_id, name, size in reader:
                line_no += 1
                segments = re.findall('\w+', name)
                if segments[-1] in exclusions:
                    # Skip certain extensions.
                    continue
                blob_id = int(blob_id)
                size = int(size)
                root.add_descendant_leaf(segments, blob_id, size, reverse=reverse)
    except ValueError as e:
        raise ValueError(f"Error on line {line_no}: {e.args[0]}")
    return root


def build_trie_from_file(filename: str, exclusions: Set[str]=None, reverse: bool=True) -> TrieNode:
    if exclusions is None:
        exclusions = set()
    root = RootNode()
    line_no = 0
    try:
        with open(filename) as f:
            for line in f:
                line_no += 1
                segments = re.findall('\w+', line.strip())
                if segments[-1] in exclusions:
                    continue
                root.add_descendant_leaf(segments, 0, 0, reverse=reverse)
    except ValueError as e:
        raise ValueError(f"Error on line {line_no}: {e.args[0]}")
    return root
