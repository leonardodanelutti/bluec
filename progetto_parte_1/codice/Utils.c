#define _GNU_SOURCE // For realpath

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <libgen.h> // For dirname
#include <errno.h>  // For errno
#include "Utils.h"
#include "Logs.h"

/***** DYNAMIC ARRAY *****/

// Initialize the dynamic array
DynamicArray *da_init(size_t elem_size)
{
    DynamicArray *arr = malloc(sizeof(DynamicArray));
    size_t capacity = 5; // Initial capacity
    arr->data = malloc(elem_size * capacity);
    if (arr->data == NULL)
    {
        printf(ERROR "Memory allocation failed for dynamic array data.\n");
        exit(EXIT_FAILURE);
    }
    arr->size = 0;
    arr->capacity = capacity;
    arr->elem_size = elem_size;
    return arr;
}

// Free the memory used by the array
void da_free(DynamicArray *arr)
{
    free(arr->data);
    arr->data = NULL;
    arr->size = 0;
    arr->capacity = 0;
}

// Resize the array when needed
void da_resize(DynamicArray *arr, size_t new_capacity)
{
    arr->data = realloc(arr->data, arr->elem_size * new_capacity);
    if (arr->data == NULL)
    {
        printf(ERROR "Memory allocation failed in reallocating in dynamic array.\n");
        exit(EXIT_FAILURE);
    }
    arr->capacity = new_capacity;
}

// Add an element to the end
void da_push(DynamicArray *arr, void *elem)
{
    if (arr->size == arr->capacity)
    {
        da_resize(arr, arr->capacity * 2);
    }
    void *dest = (char *)arr->data + (arr->size * arr->elem_size);
    memcpy(dest, elem, arr->elem_size);
    arr->size++;
}

// Get a pointer to an element at index
void *da_get(DynamicArray *arr, size_t index)
{
    if (index >= arr->size)
        return NULL;
    return (char *)arr->data + (index * arr->elem_size);
}

size_t da_size(DynamicArray *arr)
{
    return arr->size;
}

/***** FILE OPERATIONS *****/

char *relative_path(char *filename1, char *filename2, YYLTYPE *loc)
{
    // Check if `filename1` is a UNIX absolute path
    if (filename1[0] == '/')
    {
        return filename1;
    }

    // If `filename1` is not absolute, construct the full path relative to `filename2`
    char *full_path = malloc(strlen(filename2) + strlen(filename1) + 2);
    if (!full_path)
    {
        printf(ERROR "out of memory when allocating full_path!\n");
        exit(1);
    }

    // Use a copy of filename2 for dirname, as it may modify the string
    char *filename2_copy = strdup(filename2);
    sprintf(full_path, "%s/%s", dirname(filename2_copy), filename1);
    free(filename2_copy);

    // Now resolve to absolute path
    char *abs_path = realpath(full_path, NULL);
    free(full_path);

    if (!abs_path)
    {
        ERROR_F(loc, "%s imported from %s: %s\n", filename1, filename2, strerror(errno));
        exit(1);
    }
    return abs_path;
}
