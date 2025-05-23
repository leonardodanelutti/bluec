#ifndef UTILS_HEADER
#define UTILS_HEADER

#include "Absyn.h"
#include "Logs.h"

/***** ABSTRACT SYNTAX TREE UTILS *****/

/* Merge two XMLS structures */
XMLS mergeXMLS(XMLS xmls1, XMLS xmls2);


/***** DYNAMIC ARRAY *****/

typedef struct {
    void* data;         // Pointer to the array's data
    size_t size;        // Number of elements currently stored
    size_t capacity;    // Allocated capacity
    size_t elem_size;   // Size of each element
} DynamicArray;

DynamicArray* da_init(size_t elem_size);
void da_free(DynamicArray* arr);
void da_push(DynamicArray* arr, void* elem);
void* da_get(DynamicArray* arr, size_t index);
size_t da_size(DynamicArray* arr);

#endif
