#include "Absyn.h"
#include "DataStructure.h"
#include "Colours.h"

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

#endif
