#include "Absyn.h"
#include "DataStructure.h"
#include "Logs.h"

#ifndef UTILS_HEADER
#define UTILS_HEADER

/***** ABSTRACT SYNTAX TREE UTILS *****/

/* Merge two XMLS structures */
XMLS mergeXMLS(XMLS xmls1, XMLS xmls2);

/***** CUSTOM DATA STRUCTURE UTILS *****/

/* Recursivly resolve pointers in the `Sections` custom data structure. */
void resolveSections(Sections sections_list);

/* Resolve the pointed value of a field.
   Runs recursively until a non-`Ref_unresolved` value is reached. */
void resolveField(Fields field, Sections start_sections_list);

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
