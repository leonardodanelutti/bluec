#include <stdio.h>
#include <stdlib.h>

#include "ResolveConflicts.h"

/* Helper function to check whether inherited sections exist. */
void check_inheritExistance(DynamicArray *inherits, Sections start_sections_list)
{
    for (size_t i = 0; i < inherits->size; i++)
    {
        Inherited_ inherit = *(Inherited_ *)da_get(inherits, i);
        int found = 0;
        for (Sections section = start_sections_list; section != NULL; section = section->next)
        {
            if (strcmp(section->ident, inherit.section_ident) == 0)
            {
                found = 1;
                break;
            }
        }
        if (!found)
        {
            fprintf(stderr, WARNING "%s:%d:%d: section %s does not exist.\n",
                    inherit.symbol_loc.file_path, inherit.symbol_loc.top_loc.first_line,
                    inherit.symbol_loc.top_loc.first_column + 1, inherit.section_ident);
        }
    }
}

void resolveSections_recursive(Sections current_section, Sections start_sections_list)
{
    /* Base case: we reached the end of the sections list */
    if (current_section == NULL)
        return;

    /* Check if inherits are of existing sections */
    check_inheritExistance(current_section->inherits, start_sections_list);

    /* Iterate through the fields in the current section to search for dangling
       references */
    for (Fields field = current_section->fields; field != NULL; field = field->next)
    {
        if (field->val.kind == Ref_unresolved)
        {
            /* Resolve the reference */
            resolveField(field, start_sections_list);
        }
    }

    /* Recursively call the function for the next section */
    resolveSections_recursive(current_section->next, start_sections_list);
}

void resolveSections(Sections sections_list)
{
    resolveSections_recursive(sections_list, sections_list);
}

void resolveField(Fields field, Sections start_sections_list)
{
    // Mark the field as visited
    field->kind = Visited;

    /* Base case: we reached the end of the sections list */
    if (start_sections_list == NULL)
    {
        // Resolved reference
        field->kind = Not_visited;
        return;
    }

    /* Check if the field is a reference */
    if (field->val.kind == Ref_unresolved)
    {
        /* TODO: remove this debugging print
           printf("Resolving %s that points to $%s.%s\n", field->ident, field->val.u.unresolved_ref.section_ident, field->val.u.unresolved_ref.field_ident);
           */
        /* Find the section that contains the field */
        for (Sections section = start_sections_list; section != NULL; section = section->next)
        {
            if (strcmp(section->ident, field->val.u.unresolved_ref.section_ident) == 0)
            {
                /* Find the field in the section */
                for (Fields ref_field = section->fields; ref_field != NULL; ref_field = ref_field->next)
                {
                    if (strcmp(ref_field->ident, field->val.u.unresolved_ref.field_ident) == 0)
                    {
                        if (ref_field->kind == Visited)
                        {
                            /* Recursive reference detected */
                            fprintf(stderr, ERROR "recursive reference from $%s.%s to $%s.%s.\n",
                                    field->section->ident, field->ident,
                                    ref_field->section->ident, ref_field->ident);
                            exit(EXIT_FAILURE);
                        }
                        /* Update the field value with the resolved reference */
                        field->val.kind = Ref;
                        field->val.u.ref = ref_field;
                        /* Update the `pointed_by` field in the pointed `Field` */
                        if (ref_field->pointed_by.vals_size == 0)
                        {
                            /* Initialize the pointed_by field if it is empty */
                            ref_field->pointed_by.vals_size = 5;
                            ref_field->pointed_by.vals = malloc(sizeof(*ref_field->pointed_by.vals) * ref_field->pointed_by.vals_size);
                            if (ref_field->pointed_by.vals == NULL)
                            {
                                fprintf(stderr, ERROR "Memory allocation failed for pointed_by field.\n");
                                exit(EXIT_FAILURE);
                            }
                        }
                        else if (ref_field->pointed_by.vals_count >= ref_field->pointed_by.vals_size)
                        {
                            /* Resize the pointed_by field if it is full */
                            ref_field->pointed_by.vals_size += 5;
                            ref_field->pointed_by.vals = realloc(ref_field->pointed_by.vals, sizeof(*ref_field->pointed_by.vals) * ref_field->pointed_by.vals_size);
                            if (ref_field->pointed_by.vals == NULL)
                            {
                                fprintf(stderr, ERROR "Memory reallocation failed for pointed_by field.\n");
                                exit(EXIT_FAILURE);
                            }
                        }
                        /* Add the field to the pointed_by list */
                        ref_field->pointed_by.vals[ref_field->pointed_by.vals_count] = &field->val;
                        ref_field->pointed_by.vals_count++;

                        /* Recursively resolve the reference */
                        resolveField(ref_field, start_sections_list);

                        // Resolved reference
                        field->kind = Not_visited;
                        return;
                    }
                }
                fprintf(stderr, ERROR "%s:%d:%d: reference to non-existing field %s in section %s.\n",
                        field->val.symbol_loc.file_path, field->val.symbol_loc.top_loc.first_line,
                        field->val.symbol_loc.top_loc.first_column + 1, field->val.u.unresolved_ref.field_ident,
                        field->val.u.unresolved_ref.section_ident);
                exit(EXIT_FAILURE);
            }
        }
        /* The reference could not be resolved */
        /* TODO: add file name in the error */
        fprintf(stderr, ERROR "%s:%d:%d: reference to non-existing section %s.\n",
                field->val.symbol_loc.file_path, field->val.symbol_loc.top_loc.first_line,
                field->val.symbol_loc.top_loc.first_column + 1, field->val.u.unresolved_ref.section_ident);
        exit(EXIT_FAILURE);
    }

    /* Not a reference */
    field->kind = Not_visited;
    return;
}

// Dg
// TODO: Inherit warning if unused? Forse no
