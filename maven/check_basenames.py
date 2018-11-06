#!/usr/bin/env python3


def check_basenames(all_files_file: str, basenames_file: str):
    af = open(all_files_file)
    bf = open(basenames_file)

    a_line = ''
    b_line = ''

    def adv_a():
        nonlocal a_line
        a_line = af.readline()

    def adv_b():
        nonlocal b_line
        b_line = bf.readline()

    adv_a()
    adv_b()

    while True:
        if a_line == '':
            # We've reached the end of the all-names file. Print the remainder of the basenames.
            while b_line != '':
                print(b_line.rstrip())
                adv_b()
            # And quit.
            break
        elif b_line == '':
            # We've reached the end of the basenames file, so we're done.
            break
        elif a_line == b_line:
            # The lines match.
            adv_a()
            adv_b()
        elif a_line < b_line:
            # We haven't found the match yet.
            adv_a()
        elif a_line > b_line:
            # We passed the match --- meaning it doesn't exist.
            print(b_line.rstrip())
            adv_b()
        else:
            raise RuntimeError


if __name__ == '__main__':
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument('all_files_file')
    parser.add_argument('basenames_file')
    args = parser.parse_args()

    check_basenames(args.all_files_file, args.basenames_file)
