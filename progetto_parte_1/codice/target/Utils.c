#include <stdio.h>
#include <stdlib.h>
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

void resolveSections_recursive(Sections current_section, Sections start_sections_list)
{
    /* Base case: we reached the end of the sections list */
    if (current_section == NULL)
        return;

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
    printf("Resolving %s that points to $%s.%s\n", field->ident, field->val.u.unresolved_ref.section_ident, field->val.u.unresolved_ref.field_ident);
    /* Base case: we reached the end of the sections list */
    if (start_sections_list == NULL)
        return;

    /* Check if the field is a reference */
    if (field->val.kind == Ref_unresolved)
    {
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
                            fprintf(stderr, ANSI_COLOR_RED "ERROR:" ANSI_COLOR_RESET " recursive reference from $%s.%s to $%s.%s.\n",
                                    field->section->ident, field->ident,
                                    ref_field->section->ident, ref_field->ident);
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
                                fprintf(stderr, ANSI_COLOR_RED "ERROR:" ANSI_COLOR_RED " Memory allocation failed for pointed_by field.\n");
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
                                fprintf(stderr, ANSI_COLOR_RED "ERROR:" ANSI_COLOR_RED " Memory reallocation failed for pointed_by field.\n");
                                exit(EXIT_FAILURE);
                            }
                        }
                        /* Add the field to the pointed_by list */
                        ref_field->pointed_by.vals[ref_field->pointed_by.vals_count] = &field->val;
                        ref_field->pointed_by.vals_count++;

                        /* Mark the field as visited */
                        ref_field->kind = Visited;
                        /* Recursively resolve the reference */
                        resolveField(ref_field, start_sections_list);

                        /* Resolved reference */
                        ref_field->kind = Not_visited;
                        return;
                    }
                }
            }
        }
        /* The reference could not be resolved */
        /* TODO: add file name in the error */
        fprintf(stderr, ANSI_COLOR_RED "ERROR:" ANSI_COLOR_RESET " unresolved reference to section %s and field %s.\n",
                field->val.u.unresolved_ref.section_ident, field->val.u.unresolved_ref.field_ident);
        exit(EXIT_FAILURE);
    }

    /* Not a reference */
    return;
}

/* TODO:
   - Puntatore bidirezionale per eliminazione
   - Inherit warning if section undefined */
