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

/* Initialize the dynamic array */
DynamicArray *da_init(size_t elem_size);
/* Free the memory used by the array */
void da_free(DynamicArray *arr);
/* Add an element to the end */
void da_push(DynamicArray *arr, void *elem);
/* Get a pointer to an element at index */
void *da_get(DynamicArray *arr, size_t index);
/* Get the array size */
size_t da_size(DynamicArray *arr);

/***** FILE OPERATIONS *****/

/* Open a file (`filename1`) relatively to another file path (`filename2`). */
char *relative_path(char *filename1, char *filename2, YYLTYPE *loc);

#endif /* UTILS_HEADER */
