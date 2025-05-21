#include <stdio.h>
#include <stdlib.h>

#include "DataStructure.h"

Sections make_SectionNew(Ident ident) {
    Sections new_section = (Sections)malloc(sizeof(struct Sections_));
    if (new_section == NULL) {
        fprintf(stderr, "Memory allocation failed for new section\n");
        exit(EXIT_FAILURE);
    }
    new_section->ident = ident;
    new_section->fields = NULL;
    new_section->next = NULL;
    return new_section;
}

Fields make_FieldNew(Ident ident, Value val) {
    Fields new_field = (Fields)malloc(sizeof(struct Fields_));
    if (new_field == NULL) {
        fprintf(stderr, "Memory allocation failed for new field\n");
        exit(EXIT_FAILURE);
    }
    new_field->ident = ident;
    new_field->val = val;
    new_field->next = NULL;
    new_field->section = NULL;
    return new_field;
}

