/* This module implements reference resolution for the custom data structures. If a
   variable cannot be resolved, an error is reported and the program is terminated.
   Moreover, some warning checks are implemented to ensure that the user is aware of the
   possible issues. */

#ifndef RESOLVE_CONFLICTS_H
#define RESOLVE_CONFLICTS_H

#include <stdbool.h>
#include "DataStructure.h"

/* Recursivly resolve pointers in the `Sections` custom data structure. */
void resolveSections(Sections sections_list);

/* Resolve the pointed value of a field.
   Runs recursively until a non-`Ref_unresolved` value is reached.
   If the value `trust_refs` is set to `false`, the function does not trust already
   resolved references and will run recursively until a non-`Ref` or non-`Ref_unresolved`
   value is reached. */
void resolveField(Fields field, Sections start_sections_list, bool trust_refs);

#endif