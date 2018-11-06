// Reads stdin and prints to stdout the sorted and unique-ed lines
// Compile with 'gcc -Ofast main.c' or 'clang -Ofast main.c'

// Test input generation:
// perl -e '$i = 1000*1000; while ($i < 2*1000*1000) { print("1234566789-1234566789-1234566789-1234566789-1234566789-1234566789-1234566789-1234566789-1234566789-1234566789-1234566789-1234566789-1234566789-1234566789-$i\n"); $i++}' >test-input.txt

#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

////////////////////////////////////////////////////////////////////////////////
// Configuration

//const size_t min_pool_free_size = 256;
//const size_t pool_size = 4*1024; // total pool size

const size_t min_pool_free_size = 1024;
const size_t pool_size = 128*1024; // total pool size

////////////////////////////////////////////////////////////////////////////////
// Links

struct link {
  struct link *next;
  char * string;
};

struct link * links = NULL;

////////////////////////////////////////////////////////////////////////////////
// Pools

void *pool;
void *alt_pool;
size_t pool_alloc_size; // number of bytes allocated in pool

////////////////////////////////////////////////////////////////////////////////
// Functions

// TODO: put many zeros at the end so we can do strcmp by words
// TODO: also align pool_alloc_size

#define DEBUG_LINE printf("%s: %d\n", __FILE__, __LINE__)

void print_links(struct link *current_link) {
  while (current_link != NULL) {
    fputs(current_link->string, stdout);
    current_link = current_link->next;
  }
}

void print_links_debug() {
  struct link *current_link = links;
  printf("\n\nbegin print_links\n");
  while (current_link != NULL) {
    printf("link: %s", current_link->string);
    current_link = current_link->next;
  }
  printf("\nend print_links\n\n");
}

// Find the first link in alt_pool and remove it and all links after it.
// Returns a pointer to the first link in the alt_pool.
struct link* remove_alt_pool_links() {
  struct link *current_link = links;
  struct link **ptr_to_current_link = &links;

  // Find the first link in the alt_pool
  if (pool < alt_pool) {
    while (current_link != NULL && (void*)current_link < alt_pool) {
      ptr_to_current_link = &current_link->next;
      current_link = current_link->next;
    }
  } else {
    while (current_link != NULL && (void*)current_link >= pool) {
      ptr_to_current_link = &current_link->next;
      current_link = current_link->next;
    }
  }

  // Note that current_link is now the first link in the alt_pool

  // Remove the pointer to the current_link
  *ptr_to_current_link = NULL;

  return current_link;
}

// Uses mutation to reverse a list of links.
// Returns a pointer to the head of the reversed list.
struct link* reverse_list(struct link *current_link) {
  struct link *previous_link = NULL;

  while (current_link != NULL) {
    struct link *tmp = current_link->next;
    current_link->next = previous_link;
    previous_link = current_link;
    current_link = tmp;
  }

  return previous_link;
}

void flush() {
  // Print the links that are in the alt_pool
  struct link *current_link = remove_alt_pool_links();
  struct link *reversed_links = reverse_list(current_link);
  print_links(reversed_links);

  // At this point everything in alt_pool should be dead so we can now swap the pools
  void *tmp = pool;
  pool = alt_pool;
  alt_pool = tmp;
  pool_alloc_size = 0;
}

int str_cmp(char *str1, char *str2, size_t n) {
  size_t i = 0;
  while (*(long long*)(str1 + i) == *(long long*)(str2 + i)) {
    i += sizeof(long long);
  }
  if (*(long long*)(str1 + i) < *(long long*)(str2 + i)) {
    return -1;
  } else {
    return 1;
  }
}

int main(int argc, char** argv) {
  assert(min_pool_free_size > sizeof(struct link) + 1); // +1 for null byte read by fgets

  pool = malloc(pool_size);
  alt_pool = malloc(pool_size);

  while (true) {
    // Flush if we need more free space
    if (pool_alloc_size + min_pool_free_size > pool_size) { flush(); }

    void *pool_base = pool + pool_alloc_size;
    struct link *new_link = pool_base;
    char *new_string = pool_base + sizeof(struct link);
    size_t n = pool + pool_size - (void*)new_string;

    //char *result = fgets(new_string, pool + pool_size - (void*)lsnew_string, stdin);
    ssize_t result = getline(&new_string, &n, stdin);

    *(long long*)(new_string + result) = 0;
    *(long long*)(new_string + result + sizeof(long long)) = 0;

    // Exit if at EOF or there was an error
    if (result ==-1) {
      if (feof(stdin)) {
        break;
      } else {
        fprintf(stderr, "Error when reading STDIN\n");
        exit(1);
      }
    }

    struct link *current_link = links;
    struct link **ptr_to_current_link = &links;

    const size_t length = result; //strlen(new_string);

    // Figure out where to insert the new link
    while (true) {
      const int cmp = current_link == NULL ? 1 : strncmp(new_string, current_link->string, length);
      if (cmp > 0) { // Insert the line before current_link (the common case)
        // Mark the link and string as allocated
        pool_alloc_size += sizeof(struct link);
        pool_alloc_size += strlen(new_string) + 1 ; // +1 is for the null byte
        pool_alloc_size += 32;
        pool_alloc_size &= 0xffFFffFFffFFffE0;

        // Fill in the link data
        new_link->next = current_link;
        new_link->string = new_string;
        *ptr_to_current_link = new_link;

        // Stop searching
        break;
      } else if (cmp < 0) { // Keep searching
        ptr_to_current_link = &current_link->next;
        current_link = current_link->next;
      } else { // We already have the line, so ignore it
        // Stop searching
        break;
      }
    }
  }

  flush(); // Print the alt_pool
  flush(); // Print what was the pool but is now the alt_pool

  assert(links == NULL);

  return 0;
}
