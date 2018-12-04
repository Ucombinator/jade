#!/usr/bin/env python3
"""
In the Maven repository, there are `maven-metadata.xml` files that inform us which versions of libraries are the most
recent. We want to extract all of the `.pom` files for the latest version of each library, since these contain the
dependency information that we need to build a full dependency graph.

The `maven-metadata.xml` files are usually structured as:

    <?xml ...>
    <metadata modelVersion="x.x.x">
        <groupId>xxx</groupId>
        <artifactId>xxx</artifactId>
        <versioning>
            <latest>x.x.x</latest>
            <release>x.x.x</release>
            <versions>
                <version>x.x.x</version>
                <version>x.x.x</version>
                ...
            </versions>
            <lastUpdated>YYYMMDDxxxxxx</lastUpdated>
        </versioning>
    </metadata>

or

    <?xml ...>
    <metadata modelVersion="x.x.x">
        <groupId>xxx</groupId>
        <artifactId>xxx</artifactId>
        <latest>x.x.x</latest>
        <versioning>
            <release>x.x.x</release>
            <versions>
                <version>x.x.x</version>
                <version>x.x.x</version>
                ...
            </versions>
            <lastUpdated>YYYMMDDxxxxxx</lastUpdated>
        </versioning>
    </metadata>

or

    <?xml ...>
    <metadata modelVersion="x.x.x">
        <groupId>xxx</groupId>
        <artifactId>xxx</artifactId>
        <version>x.x.x</version>
        <versioning>
            <release>x.x.x</release>
            <versions>
                <version>x.x.x</version>
                <version>x.x.x</version>
                ...
            </versions>
            <lastUpdated>YYYMMDDxxxxxx</lastUpdated>
        </versioning>
    </metadata>

However, there are also `maven-metadata.xml` files for each version. These generally have only three fields:

    <?xml ...>
    <metadata modelVersion="x.x.x">
        <groupId>xxx</groupId>
        <artifactId>xxx</artifactId>
        <version>x.x.x</version>
    </metadata>


The information we need is <groupId>, <artifactId>, and <latest> (or <version> if <latest> is not available), so these
are extracted via XML parsing with lxml and the path for the expected `.pom` file is constructed.
"""

from download_blobs import *

import sys

# noinspection PyUnresolvedReferences
from lxml import etree  # For some reason, `etree` can't be found by PyCharm.
from os.path import abspath, join

BLOB_BASE = 'repos/central/data/'


class XmlParseException(MavenIndexingException):
    pass


class NoGroupIdException(XmlParseException):
    def __init__(self):
        super().__init__("Could not find group ID.")


class NoArtifactIdException(XmlParseException):
    def __init__(self):
        super().__init__("Could not find artifact ID.")


class NoLatestVersionException(XmlParseException):
    def __init__(self):
        super().__init__("Could not find latest version.")


def process_metadata_xml(xml_file: str) -> str:
    """
    Opens an XML file and constructs the expected name of the `.pom` file that corresponds to the latest version of the
    library.

    Any valid XML file should have a <versioning> field at the top level. In such an XML file, we expect to find the
    <groupId> and <artifactId> fields also at the top level. Most often, the <latest> field appears inside the
    <versioning> field, but occasionally it is at the top level. If it is in neither place, we look at the top level for
    the <version> field, which is used by certain libraries in place of the <latest> field for some reason.

    :param xml_file: the path to the XML file to read
    :return: the name of the `.pom` file expected to exist for the latest version of the library
    """
    xml = etree.parse(xml_file)
    versioning = xml.find('versioning')
    if versioning is None:
        # The <versioning> tag is required to differentiate artifact-level maven-metadata.xml files from version-level.
        raise NoLatestVersionException()
    latest_version = versioning.find('latest')
    if latest_version is None:
        latest_version = xml.find('latest')
        if latest_version is None:
            latest_version = xml.find('version')
            if latest_version is None:
                raise NoLatestVersionException()
    latest_version = latest_version.text
    group_id = xml.find('groupId')
    if group_id is None:
        raise NoGroupIdException()
    group_id = group_id.text.replace('.', '/')
    artifact_id = xml.find('artifactId')
    if artifact_id is None:
        raise NoArtifactIdException()
    artifact_id = artifact_id.text
    # Construct the `.pom` file's name.
    pom_name = join(BLOB_BASE, group_id, artifact_id, latest_version, f'{artifact_id}-{latest_version}.pom')
    return pom_name


def extract_expected_poms(metadata_xml_names_file: Optional[str], download_dir: str):
    """
    Builds a list of expected `.pom` files based on a given list of `maven-metadata.xml` files in the Maven repository.

    :param metadata_xml_names_file: the input file to read `maven-metadata.xml` file names from
    :param download_dir: the top-level directory to read the `maven-metadata.xml` files
    """
    if metadata_xml_names_file is None:
        f = sys.stdin
    else:
        f = open(metadata_xml_names_file)
    download_dir = abspath(download_dir)
    line_no = 0
    try:
        for raw_line in f:
            line_no += 1
            xml_name = join(download_dir, raw_line.strip())
            with error_context(xml_name):
                expected_pom = process_metadata_xml(xml_name)
                print(expected_pom)
    except Exception:
        show(f"Processing interrupted on line {line_no}.")
        raise
    finally:
        f.close()


if __name__ == '__main__':
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument('--xml-names-file', '-f', default=None)
    parser.add_argument('--download-dir', '-d', default='.')
    args = parser.parse_args()

    extract_expected_poms(args.xml_names_file, args.download_dir)
