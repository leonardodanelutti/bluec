#include <stdio.h>
#include <stdlib.h>

#include "ResolveRefs.h"

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

    int used_inherits[current_section->inherits->size];
    memset(used_inherits, 0, sizeof(used_inherits));

    /* Iterate through the fields in the current section to search for dangling
       references */
    for (Fields field = current_section->fields; field != NULL; field = field->next)
    {
        if (field->val.kind == Ref_unresolved)
        {
            /* Resolve the reference */
            resolveField(field, start_sections_list, 1);
        }

        if (field->val.kind == Ref)
        {
            /* Check if an inherit was used */
            if (field->val.u.ref->section != current_section)
            {
                int repeated_inherit = 0;
                for (size_t i = 0; i < current_section->inherits->size; i++)
                {
                    Inherited_ inherit = *(Inherited_ *)da_get(current_section->inherits, i);
                    if (strcmp(field->val.u.ref->section->ident, inherit.section_ident) == 0)
                    {
                        used_inherits[i] = 1;
                        if (repeated_inherit)
                        {
                            fprintf(stderr, WARNING "%s:%d:%d: inherited section `%s` multiple times. Consider removing this duplicate.\n",
                                    inherit.symbol_loc.file_path, inherit.symbol_loc.top_loc.first_line,
                                    inherit.symbol_loc.top_loc.first_column + 1, inherit.section_ident);
                        }
                        repeated_inherit = 1;
                    }
                }
            }
        }
    }

    /* Check for unused inherits */
    for (size_t i = 0; i < current_section->inherits->size; i++)
    {
        if (!used_inherits[i])
        {
            Inherited_ inherit = *(Inherited_ *)da_get(current_section->inherits, i);
            fprintf(stderr, WARNING "%s:%d:%d: inherited section `%s` is not used. Consider removing it.\n",
                    inherit.symbol_loc.file_path, inherit.symbol_loc.top_loc.first_line,
                    inherit.symbol_loc.top_loc.first_column + 1, inherit.section_ident);
        }
    }

    /* Recursively call the function for the next section */
    resolveSections_recursive(current_section->next, start_sections_list);
}

void resolveSections(Sections sections_list)
{
    resolveSections_recursive(sections_list, sections_list);
}

void resolveField(Fields field, Sections start_sections_list, bool trust_refs)
{
    // printf("Resolving field %s in section %s\n", field->ident, field->section->ident);
    //  Mark the field as visited
    field->kind = Visited;

    // Store the ident of the pointed field
    FieldIdent_ pointed;
    switch (field->val.kind)
    {
    case Ref_unresolved:
        pointed.field_ident = field->val.u.unresolved_ref.field_ident;
        pointed.section_ident = field->val.u.unresolved_ref.section_ident;
        break;
    case Ref:
        pointed.field_ident = field->val.u.ref->ident;
        pointed.section_ident = field->val.u.ref->section->ident;
        break;
    default:
        pointed = (FieldIdent_){NULL, NULL};
    }
    // Check if the field is a reference
    if (field->val.kind == Ref_unresolved || (field->val.kind == Ref && !trust_refs))
    {
        /* TODO: remove this debugging print
        printf("Resolving %s that points to $%s.%s\n", field->ident, pointed.section_ident, pointed.field_ident); */

        /* Find the section that contains the field */
        for (Sections section = start_sections_list; section != NULL; section = section->next)
        {
            if (strcmp(section->ident, pointed.section_ident) == 0)
            {
                /* Find the field in the section */
                for (Fields ref_field = section->fields; ref_field != NULL; ref_field = ref_field->next)
                {
                    if (strcmp(ref_field->ident, pointed.field_ident) == 0)
                    {
                        if (ref_field->kind == Visited)
                        {
                            /* Recursive reference detected */
                            fprintf(stderr, ERROR "%s:%d:%d: recursive reference from $%s.%s to $%s.%s.\n",
                                    field->val.symbol_loc.file_path, field->val.symbol_loc.top_loc.first_line, field->val.symbol_loc.top_loc.first_column + 1,
                                    field->section->ident, field->ident,
                                    ref_field->section->ident, ref_field->ident);
                            exit(EXIT_FAILURE);
                        }
                        /* Update the field value with the resolved reference */
                        field->val.kind = Ref;
                        field->val.u.ref = ref_field;
                        /* Update the `pointed_by` field in the pointed `Field` */
                        da_push(ref_field->pointed_by, &field);

                        /* Recursively resolve the reference */
                        resolveField(ref_field, start_sections_list, trust_refs);

                        // Resolved reference
                        field->kind = Not_visited;
                        return;
                    }
                }
                fprintf(stderr, ERROR "%s:%d:%d: reference to non-existing field %s in section %s.\n",
                        field->val.symbol_loc.file_path, field->val.symbol_loc.top_loc.first_line,
                        field->val.symbol_loc.top_loc.first_column + 1, pointed.field_ident,
                        pointed.section_ident);
                exit(EXIT_FAILURE);
            }
        }
        /* The reference could not be resolved */
        fprintf(stderr, ERROR "%s:%d:%d: reference to non-existing section %s.\n",
                field->val.symbol_loc.file_path, field->val.symbol_loc.top_loc.first_line,
                field->val.symbol_loc.top_loc.first_column + 1, pointed.section_ident);
        exit(EXIT_FAILURE);
    }

    /* Not a reference */
    field->kind = Not_visited;
    return;
}
