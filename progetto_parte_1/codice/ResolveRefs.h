#ifndef RESOLVE_CONFLICTS_H
#define RESOLVE_CONFLICTS_H

#include "DataStructure.h"

/***** CUSTOM DATA STRUCTURE UTILS *****/

/* Recursivly resolve pointers in the `Sections` custom data structure. */
void resolveSections(Sections sections_list);

/* Resolve the pointed value of a field.
   Runs recursively until a non-`Ref_unresolved` value is reached. */
void resolveField(Fields field, Sections start_sections_list);

#endif