#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "Utils.h"

XMLS mergeXMLS(XMLS xmls1, XMLS xmls2)
{
    XMLS r = (XMLS)malloc(sizeof(*r));
    r = xmls1;
    while (xmls1->kind != is_XMLSEmpty)
    {
        xmls1 = xmls1->u.xmlsection_.xmls_;
    }
    *xmls1 = *xmls2;
    return r;
}

// Initialize the dynamic array
DynamicArray* da_init(size_t elem_size) {
    DynamicArray* arr = malloc(sizeof(DynamicArray));
    size_t capacity = 5; // Initial capacity
    arr->data = malloc(elem_size * capacity);
    arr->size = 0;
    arr->capacity = capacity;
    arr->elem_size = elem_size;
    return arr;
}

// Free the memory used by the array
void da_free(DynamicArray* arr) {
    free(arr->data);
    arr->data = NULL;
    arr->size = 0;
    arr->capacity = 0;
}

// Resize the array when needed
void da_resize(DynamicArray* arr, size_t new_capacity) {
    arr->data = realloc(arr->data, arr->elem_size * new_capacity);
    arr->capacity = new_capacity;
}

// Add an element to the end
void da_push(DynamicArray* arr, void* elem) {
    if (arr->size == arr->capacity) {
        da_resize(arr, arr->capacity * 2);
    }
    void* dest = (char*)arr->data + (arr->size * arr->elem_size);
    memcpy(dest, elem, arr->elem_size);
    arr->size++;
}

// Get a pointer to an element at index
void* da_get(DynamicArray* arr, size_t index) {
    if (index >= arr->size) return NULL;
    return (char*)arr->data + (index * arr->elem_size);
}

size_t da_size(DynamicArray* arr) {
    return arr->size;
}

