#include <stdio.h>
#include <stdlib.h>

#include "ResolveConflicts.h"
#include "DataStructure.h"

void check_Section(Sections sections_list, Ident section_ident);
Fields convert_SBody_to_Fields(SBody sbody, Sections current_section);
Val_ convert_Value_to_Val_(Value value, Ident section_ident);
void check_Inherit(DynamicArray *inherit_section_list, Val_ val, Ident current_section);
int overwrite_Field(Fields fields_list, Ident field_ident, Val_ val);

Sections make_SectionNew(Ident ident)
{
    Sections new_section = (Sections)malloc(sizeof(struct Sections_));
    if (new_section == NULL)
    {
        fprintf(stderr, "Memory allocation failed for new section\n");
        exit(EXIT_FAILURE);
    }
    new_section->ident = ident;
    new_section->fields = NULL;
    new_section->next = NULL;
    new_section->inherits = da_init(sizeof(Ident));
    return new_section;
}

Fields make_FieldNew(Ident ident, Val_ val)
{
    Fields new_field = (Fields)malloc(sizeof(struct Fields_));
    if (new_field == NULL)
    {
        fprintf(stderr, "Memory allocation failed for new field\n");
        exit(EXIT_FAILURE);
    }
    new_field->ident = ident;
    new_field->val = val;
    new_field->kind = Not_visited;
    new_field->next = NULL;
    new_field->section = NULL;
    ValList_ pointed_by = {
        .vals = NULL,
        .vals_size = 0,
        .vals_count = 0};
    new_field->pointed_by = pointed_by;
    return new_field;
}

void push_Section(Sections sections_list, Ident section_ident)
{
    if (sections_list == NULL)
    {
        sections_list = make_SectionNew(section_ident);
        return;
    }

    Sections current = sections_list;
    while (current->next != NULL)
    {
        if (strcmp(current->ident, section_ident) == 0)
        {
            fprintf(stderr, "Duplicate section with identifier: %s\n", section_ident);
            exit(EXIT_FAILURE);
        }
        current = current->next;
    }
    Sections new_section = make_SectionNew(section_ident);
    current->next = new_section;
}

void push_FieldToSection(Sections sections_list, FieldIdent_ field_ident, Val_ val)
{
    Sections current = sections_list;
    // Find the corresponding section
    while (current != NULL)
    {
        if (strcmp(current->ident, field_ident.section_ident) == 0)
        {
            // TODO: backpointer
            Fields new_field = make_FieldNew(field_ident.field_ident, val);
            new_field->section = current;
            // resolve the field identifier
            resolveField(new_field, sections_list);
            // Check if the field already exists in the section, if it does, overwrite it
            // otherwise, add it to the section
            Fields current_field = current->fields;
            Fields prev_field = NULL;
            while (current_field != NULL)
            {
                if (strcmp(current_field->ident, field_ident.field_ident) == 0)
                {
                    current_field->val = val;
                    /* TODO: warning with line */
                    printf("Warning field overwritten");
                    return; // Field found and overwritten
                }
                prev_field = current_field;
                current_field = current_field->next;
            }
            prev_field->next = new_field;
            return; // Field added
        }
        current = current->next;
    }
    fprintf(stderr, "Section %s not found \n", field_ident.section_ident);
}

void delete_Section(Sections sections_list, Ident section_ident)
{
    Sections current = sections_list;
    Sections prev_section = NULL;
    // Find the section to delete
    while (current != NULL)
    {
        if (strcmp(current->ident, section_ident) == 0)
        {
            // Manage the removal of all the fields in the section
            for (Fields field = current->fields; field != NULL;)
            {
                Fields next_field = field->next;
                // TODO: devo per forza creare un nuovo FieldIdent?
                delete_FieldInSection(sections_list, new_FieldIdent(field->ident, section_ident));
                field = next_field;
            }
            // Remove the section from the list
            if (prev_section == NULL)
            {
                sections_list = current->next; // Delete the first section
            }
            else
            {
                prev_section->next = current->next; // Delete the section in the middle or end
            }
            free(current);
            return; // Section deleted
        }
        prev_section = current;
        current = current->next;
    }
    fprintf(stderr, "Section %s not found\n", section_ident);
}

void delete_FieldInSection(Sections sections_list, FieldIdent_ field_ident)
{
    Sections current = sections_list;
    // Find the corresponding section
    while (current != NULL)
    {
        if (strcmp(current->ident, field_ident.section_ident) == 0)
        {
            Fields current_field = current->fields;
            Fields prev_field = NULL;
            // Find the field to delete
            while (current_field != NULL)
            {
                if (strcmp(current_field->ident, field_ident.field_ident) == 0)
                {
                    // Check the values that are currently pointed to this field
                    for (long unsigned int i = 0; i < current_field->pointed_by.vals_count; i++)
                    {
                        // Get the i-th pointer to the field
                        Val pointer = current_field->pointed_by.vals[i];
                        // Copy the field value to the pointer value
                        pointer->kind = current_field->val.kind;
                        pointer->u = current_field->val.u;
                    }
                    // Remove the field from the list
                    if (prev_field == NULL)
                    {
                        current->fields = current_field->next; // Delete the first field
                    }
                    else
                    {
                        prev_field->next = current_field->next; // Delete the field in the middle or end
                    }
                    free(current_field);
                    return; // Field deleted
                }
                prev_field = current_field;
                current_field = current_field->next;
            }
        }
        current = current->next;
    }
    fprintf(stderr, ERROR "Field %s not found in section %s\n", field_ident.field_ident, field_ident.section_ident);
}

/* Convert XMLS to a linked list of sections */

Sections convert_XMLS_to_Sections(XMLS xml)
{
    if (xml == NULL || xml->kind == is_XMLSEmpty)
    {
        return NULL;
    }

    /* Create the head of the sections list */
    Sections sections_list = make_SectionNew(xml->u.xmlsection_.section_->u.filesection_.ident_);
    sections_list->fields = convert_SBody_to_Fields(xml->u.xmlsection_.section_->u.filesection_.sbody_,
                                                    sections_list);
    Sections last_section = sections_list;
    /* Move to the next field */
    xml = xml->u.xmlsection_.xmls_;

    /* Iterate through the XMLS list and create sections */
    while (xml->kind == is_XMLSection)
    {
        Ident sec_ident = xml->u.xmlsection_.section_->u.filesection_.ident_;
        SBody sbody = xml->u.xmlsection_.section_->u.filesection_.sbody_;

        check_Section(sections_list, sec_ident);
        Sections new_section = make_SectionNew(sec_ident);
        new_section->fields = convert_SBody_to_Fields(sbody, new_section);
        last_section->next = new_section;
        last_section = new_section;
        /* Move to the next XMLS element */
        xml = xml->u.xmlsection_.xmls_;
    }

    return sections_list;
}

/* Check if a section is already present in a section list */
void check_Section(Sections sections_list, Ident section_ident)
{
    Sections current = sections_list;
    while (current != NULL)
    {
        if (strcmp(current->ident, section_ident) == 0)
        {
            /* TODO: add file and line number? */
            fprintf(stderr, "Duplicate section with identifier: %s\n", section_ident);
            exit(EXIT_FAILURE);
        }
        current = current->next;
    }
}

Fields convert_SBody_to_Fields(SBody sbody, Sections current_section)
{
    if (sbody == NULL || sbody->kind == is_SBodyEmpty)
    {
        return NULL;
    }

    Ident section_ident = current_section->ident;

    /* Construct a list of all identifiers in the inherits */

    DynamicArray *inherit_section_list = current_section->inherits;

    while (sbody->kind == is_SBodywInherit)
    {
        da_push(inherit_section_list, sbody->u.sbodywinherit_.inherit_->u.inheritsection_.ident_);
        sbody = sbody->u.sbodywinherit_.sbody_;
    }
    
    if (sbody->kind == is_SBodyEmpty)
    {
        return NULL;
    }

    /* Create the head of the fields list */

    Ident field_ident = sbody->u.sbodywoinherit_.field_->u.sectionfield_.ident_;
    Val_ value = convert_Value_to_Val_(sbody->u.sbodywoinherit_.field_->u.sectionfield_.value_, section_ident);
    check_Inherit(inherit_section_list, value, section_ident);
    Fields fields_list = make_FieldNew(field_ident, value);
    fields_list->section = current_section;
    Fields last_field = fields_list;
    /* Move to the next field */
    SBodyF sbodyf = sbody->u.sbodywoinherit_.sbodyf_;

    while (sbodyf->kind == is_SBodyField)
    {
        Ident field_ident = sbodyf->u.sbodyfield_.field_->u.sectionfield_.ident_;
        Value value = sbodyf->u.sbodyfield_.field_->u.sectionfield_.value_;
        Val_ val = convert_Value_to_Val_(value, section_ident);

        if (!overwrite_Field(fields_list, field_ident, val))
        {
            check_Inherit(inherit_section_list, val, section_ident);
            Fields new_field = make_FieldNew(field_ident, val);
            new_field->section = current_section;
            last_field->next = new_field;
            last_field = new_field;
        }

        /* Move to the next field */
        sbodyf = sbodyf->u.sbodyfield_.sbodyf_;
    }

    return fields_list;
}

Val_ convert_Value_to_Val_(Value value, Ident section_ident)
{
    switch (value->kind)
    {
    case is_ValueInteger:
        return new_Int(value->u.valueinteger_.integer_);
    case is_Value_true:
        return new_Bool(1);
    case is_Value_false:
        return new_Bool(0);
    case is_ValueString:
        return new_Str(value->u.valuestring_.string_);
    case is_Value1:
        return new_Ref(new_FieldIdent(value->u.value1_.ident_, section_ident));
    case is_Value2:
        return new_Ref(new_FieldIdent(value->u.value2_.ident_2, value->u.value2_.ident_1));
    default:
        fprintf(stderr, "Unknown value kind\n");
        exit(EXIT_FAILURE);
    }
}

Val_ new_Int(int int_)
{
    Val_ val = {
        .kind = Int,
        .u.int_ = int_};
    return val;
}

Val_ new_Str(String str)
{
    Val_ val = {
        .kind = Str,
        .u.str = str};
    return val;
}

Val_ new_Bool(int bool_)
{
    Val_ val = {
        .kind = Bool,
        .u.bool_ = bool_};
    return val;
}

Val_ new_Ref(FieldIdent_ ref)
{
    Val_ val = {
        .kind = Ref_unresolved,
        .u.unresolved_ref = ref};
    return val;
}

FieldIdent_ new_FieldIdent(Ident field_ident, Ident section_ident)
{
    FieldIdent_ field_ident_ = {
        .field_ident = field_ident,
        .section_ident = section_ident};
    return field_ident_;
}

/* Check if non local variables are inherited */
void check_Inherit(DynamicArray *inherit_section_list, Val_ val, Ident current_section)
{
    if (val.kind == Ref_unresolved && strcmp(val.u.unresolved_ref.section_ident, current_section) != 0)
    {
        for (size_t i = 0; i < da_size(inherit_section_list); i++)
        {
            // TODO: Check if this is correct
            Ident inherit_section = *(Ident *)da_get(inherit_section_list, i);
            printf("%s", inherit_section);
            if (strcmp(inherit_section, val.u.unresolved_ref.section_ident) == 0)
            {
                return; // Inherit section found
            }
        }
        fprintf(stderr, "Non-local variable %s in section %s without inherit\n", val.u.unresolved_ref.field_ident, current_section);
        exit(EXIT_FAILURE);
    }
}

/* Overwrite a field in a list of field if identifier is matched */
int overwrite_Field(Fields fields_list, Ident field_ident, Val_ val)
{
    Fields current = fields_list;
    while (current != NULL)
    {
        if (strcmp(current->ident, field_ident) == 0)
        {
            current->val = val;
            /* TODO: warning with line */
            printf("Field %s overwritten with new value in section %s\n", field_ident, current->section->ident);
            return 1; // Field found and overwritten
        }
        current = current->next;
    }
    return 0; // Field not found
}
