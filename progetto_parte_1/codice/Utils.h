/* This module contains some generic utility functions. */

#ifndef UTILS_HEADER
#define UTILS_HEADER

typedef struct YYLTYPE YYLTYPE;

/***** DYNAMIC ARRAY *****/

/* Dynamic array implementation. Manages a resizable array of elements of a specified
   size. The array grows as needed when new elements are added. */
typedef struct
{
    void *data;       // Pointer to the array's data
    size_t size;      // Number of elements currently stored
    size_t capacity;  // Allocated capacity
    size_t elem_size; // Size of each element
} DynamicArray;

DynamicArray *da_init(size_t elem_size);
void da_free(DynamicArray *arr);
void da_push(DynamicArray *arr, void *elem);
void *da_get(DynamicArray *arr, size_t index);
size_t da_size(DynamicArray *arr);

/***** FILE OPERATIONS *****/

/* Open a file (`filename1`) relatively to another file path (`filename2`). */
char *relative_path(char *filename1, char *filename2, YYLTYPE *loc);

#endif /* UTILS_HEADER */
