#!/usr/bin/env python3

########
# Imports
####

import contextlib
import os
import os.path
import shutil
import sys

from lxml import etree
from typing import ContextManager, Iterator, NamedTuple, Optional, Type, Union


########
# Constants
####


COPY_FILES = False
OUTPUT = sys.stdout
ERR_OUTPUT = sys.stderr


def _make_tag(tag: str) -> str:
    return '{*}' + tag


PARENT_TAG = _make_tag('parent')
MODEL_TAG = _make_tag('modelVersion')
GROUP_ID_TAG = _make_tag('groupId')
ARTIFACT_ID_TAG = _make_tag('artifactId')
VERSION_TAG = _make_tag('version')

SUPPORTED_MODEL_VERSIONS = {'4.0.0'}


########
# Types and associated convenience functions
####


# Provide short names and reduce number of warnings due to accessing protected members.
Element = etree._Element
ElementTree = etree._ElementTree


class PomTuple(NamedTuple):
    group_id: str
    artifact_id: str
    version: str


def tup_to_path(tup: PomTuple) -> str:
    grp = os.path.join(*tup.group_id.strip().split('.'))
    art = tup.artifact_id.strip()
    ver = tup.version.strip()
    filename = f"{art}-{ver}.pom"
    return os.path.join(grp, art, ver, filename)


def elems_to_tup(group_id_elem: Element, artifact_id_elem: Element, version_elem: Element) -> PomTuple:
    return PomTuple(group_id_elem.text, artifact_id_elem.text, version_elem.text)


########
# Custom printing functions
####


def show(*args, **kwargs):
    print(*args, file=OUTPUT, **kwargs)


def eshow(*args, **kwargs):
    print(*args, file=ERR_OUTPUT, **kwargs)


########
# Custom error handling
####


class PomOrganizationException(Exception):
    def __init__(self, message: str):
        self.message = message


class WrongModelVersionException(PomOrganizationException):
    def __init__(self, version: str):
        super().__init__(f"Can only handle model version 4.0.0, not {version}.")


class MissingTagException(PomOrganizationException):
    def __init__(self, tag: str):
        super().__init__(f"Expected to find {tag} tag, but none was found.")


class NoModelVersionTagException(MissingTagException):
    def __init__(self):
        super().__init__("<modelVersion>")


class NoParentTagException(MissingTagException):
    def __init__(self):
        super().__init__("<parent>")


class NoGroupIdTagException(MissingTagException):
    def __init__(self):
        super().__init__("<groupId>")


class NoArtifactIdTagException(MissingTagException):
    def __init__(self):
        super().__init__("<artifactId>")


class NoVersionTagException(MissingTagException):
    def __init__(self):
        super().__init__("<version>")


class VariableInTextException(PomOrganizationException):
    def __init__(self, tag: str):
        super().__init__(f"Found variable reference in tag: {tag}.")


class DestinationFileExists(PomOrganizationException):
    def __init__(self, dest: str):
        super().__init__(f"File already exists at destination: {dest}.")


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
    # noinspection PyBroadException
    try:
        yield
    except KeyboardInterrupt:
        # Allow a deliberate KeyboardInterrupt to terminate program execution.
        raise
    except PomOrganizationException as e:
        eshow(f"{name}\t{e.message}")
    except Exception as e:
        eshow(f"{name}\t{type(e).__name__}: {str(e)}")


########
# Helper functions
####


def scandir(dir_path: Optional[str]) -> ContextManager[Iterator[os.DirEntry]]:
    return os.scandir(dir_path)


def parse(path: str) -> ElementTree:
    return etree.parse(path)


def find(base: Union[ElementTree, Element], path: str) -> Optional[Element]:
    return base.find(path)


def relocate(src: str, dest: str):
    if os.path.isfile(dest):
        raise DestinationFileExists(dest)
    os.makedirs(os.path.dirname(dest), exist_ok=True)
    if COPY_FILES:
        shutil.copyfile(src, dest)
    else:
        shutil.move(src, dest)


def find_tag_text(xml: ElementTree, parent: Optional[Element], search_tag: str, tag: str,
                  missing_exception: Type[Exception]) -> Element:
    elem = find(xml, search_tag)
    check_parent = False
    if elem is not None:
        if '$' in elem.text:
            check_parent = True
        else:
            return elem
    if check_parent or elem is None:
        if parent is None:
            raise NoParentTagException()
        elem = find(parent, search_tag)
        if elem is None:
            raise missing_exception()
        if '$' in elem.text:
            raise VariableInTextException(tag)
        else:
            return elem
    raise missing_exception()


def find_group_id(xml: ElementTree, parent: Optional[Element]) -> Element:
    return find_tag_text(xml, parent, GROUP_ID_TAG, "<groupId>", NoGroupIdTagException)


def find_artifact_id(xml: ElementTree, parent: Optional[Element]) -> Element:
    return find_tag_text(xml, parent, ARTIFACT_ID_TAG, "<artifactId>", NoArtifactIdTagException)


def find_version(xml: ElementTree, parent: Optional[Element]) -> Element:
    return find_tag_text(xml, parent, VERSION_TAG, "<version>", NoVersionTagException)


def verify_model_version(xml: ElementTree):
    model = find(xml, MODEL_TAG)
    if model is None:
        # There should always be a <modelVersion> tag.
        raise NoModelVersionTagException()
    model_version = model.text.strip()
    if model_version not in SUPPORTED_MODEL_VERSIONS:
        # We only know how to parse version 4.0.0.
        raise WrongModelVersionException(model_version)


def extract_tup_from_pom(pom_path: str) -> PomTuple:
    xml = parse(pom_path)
    verify_model_version(xml)
    parent = find(xml, PARENT_TAG)
    group_id = find_group_id(xml, parent)
    artifact_id = find_artifact_id(xml, parent)
    version = find_version(xml, parent)
    return elems_to_tup(group_id, artifact_id, version)


########
# Primary implementation
####


def process_pom(pom_path: str, dest_dir: str) -> Optional[str]:
    with error_context(pom_path):
        tup = extract_tup_from_pom(pom_path)
        destination = os.path.join(dest_dir, tup_to_path(tup))
        relocate(pom_path, destination)
        return destination


def process_poms(pom_dir: str, dest_dir: str, show_progress: bool):
    pom_dir = os.path.abspath(pom_dir)
    dest_dir = os.path.abspath(dest_dir)
    progress_out = sys.stdout if show_progress else open(os.devnull, 'w')
    file_no = 0
    try:
        with scandir(pom_dir) as pom_it:
            for pom in pom_it:
                file_no += 1
                if file_no % 1000 == 0:
                    print(f"\rProcessing file: {file_no}", end='', file=progress_out)
                if not pom.name.endswith('.pom'):
                    continue
                pom_modified = process_pom(pom.path, dest_dir)
                if pom_modified is not None:
                    show(f"Moved {pom.path} to {pom_modified}.")
    except KeyboardInterrupt:
        show(f"Interrupted while processing file {file_no}.")


########
# main
####


if __name__ == '__main__':
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument('pom_dir', default='.')
    parser.add_argument('dest_dir', default='.')
    parser.add_argument('--copy-files', '-c', action='store_true')
    parser.add_argument('--log-file', '-l', default=None)
    parser.add_argument('--quiet', '-q', action='store_true')
    parser.add_argument('--show-progress', '-s', action='store_true')
    parsed_args = parser.parse_args()

    # Set copy/move semantics.
    COPY_FILES = parsed_args.copy_files
    if parsed_args.show_progress:
        if parsed_args.log_file is None:
            if not parsed_args.quiet:
                raise ValueError("Must specify log file (-l) or suppress regular output (-q) to support showing progress.")
        else:
            OUTPUT = open(parsed_args.log_file, 'w')
    if parsed_args.quiet:
        OUTPUT = open(os.devnull, 'w')
    # Begin processing.
    process_poms(parsed_args.pom_dir, parsed_args.dest_dir, parsed_args.show_progress)
