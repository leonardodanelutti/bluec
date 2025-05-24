#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ResolveRefs.h"
#include "DataStructure.h"

/***********************   Private Funcitons   ************************/

/* Create a new Inherited */
Inherited_ new_Inherit(Ident section_ident, SymbolLoc_ symbol_loc);

/* Convert a Sbody (from the AST) to Fields */
Fields convert_SBody_to_Fields(SBody sbody, Sections current_section);
/* Convert a Value (from the AST) to Val_ */
Val_ convert_Value_to_Val_(Value value, Ident section_ident);
/* Convert an Inherit (from the AST) to Inherited_ */
Inherited_ convert_Inherit_to_Inherited_(Inherit val);

/* Check if a section is already present in a section list */
void check_Section(Sections sections_list, Ident section_ident);
/* Check if non local variables are inherited. If inherit is found 
   return 1, if not return 0 */
int check_Inherit(Sections section, Val_ val);
/* Overwrite a field in a list of field if identifier is matched.
   Returns 1 is value is overwritten, 0 otherwise */
int overwrite_Field(Fields fields_list, Ident field_ident, Val_ val);


/**************************   Constructors   **************************/

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
    new_section->inherits = da_init(sizeof(Inherited_));
    SymbolLoc_ default_ = DEFAULT_SYMBOL_LOC_;
    new_section->symbol_loc = default_;
    return new_section;
}

Fields new_FieldNew(Ident ident, Val_ val)
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
    SymbolLoc_ default_ = DEFAULT_SYMBOL_LOC_;
    new_field->symbol_loc = default_;
    return new_field;
}

Val_ new_Int(int int_)
{
    Val_ val = {
        .kind = Int,
        .u.int_ = int_,
        .symbol_loc = DEFAULT_SYMBOL_LOC_};
    return val;
}

Val_ new_Str(String str)
{
    Val_ val = {
        .kind = Str,
        .u.str = str,
        .symbol_loc = DEFAULT_SYMBOL_LOC_};
    return val;
}

Val_ new_Bool(int bool_)
{
    Val_ val = {
        .kind = Bool,
        .u.bool_ = bool_,
        .symbol_loc = DEFAULT_SYMBOL_LOC_};
    return val;
}

Val_ new_Ref(FieldIdent_ ref)
{
    Val_ val = {
        .kind = Ref_unresolved,
        .u.unresolved_ref = ref,
        .symbol_loc = DEFAULT_SYMBOL_LOC_};
    return val;
}

FieldIdent_ new_FieldIdent(Ident field_ident, Ident section_ident)
{
    FieldIdent_ field_ident_ = {
        .field_ident = field_ident,
        .section_ident = section_ident};
    return field_ident_;
}

Inherited_ new_Inherit(Ident section_ident, SymbolLoc_ symbol_loc)
{
    Inherited_ inherit = {
        .section_ident = strdup(section_ident),
        .symbol_loc = symbol_loc};
    return inherit;
}

/*********************   Data Structure Methods   *********************/

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
            fprintf(stderr, WARNING "Section %s is already defined, section will not be pushed\n", section_ident);
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
            Fields new_field = new_FieldNew(field_ident.field_ident, val);
            new_field->section = current;
            // Add inherit if not present
            if (check_Inherit(current, val))
            {
                SymbolLoc_ default_ = DEFAULT_SYMBOL_LOC_;
                Inherited_ new_inherit = new_Inherit(val.u.unresolved_ref.section_ident, default_);
                da_push(current->inherits, &new_inherit);
            }
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
                    printf(WARNING "Field %s was overwritten", field_ident.field_ident);
                    return; // Field found and overwritten
                }
                prev_field = current_field;
                current_field = current_field->next;
            }
            if (prev_field == NULL)
            {
                current->fields = new_field; // Add as the first field
            }
            else
            {
                prev_field->next = new_field; // Add to the end of the list
            }
            return; // Field added
        }
        current = current->next;
    }
    fprintf(stderr, WARNING "Section %s not found, field will not be pushed\n", field_ident.section_ident);
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
            da_free(current->inherits);
            free(current->ident);
            free(current);
            return; // Section deleted
        }
        prev_section = current;
        current = current->next;
    }
    fprintf(stderr, WARNING "Section %s not found\n", section_ident);
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
                    free(current_field->ident);
                    free(current_field->pointed_by.vals);
                    free_Val(current_field->val);
                    free(current_field);
                    return; // Field deleted
                }
                prev_field = current_field;
                current_field = current_field->next;
            }
        }
        current = current->next;
    }
    fprintf(stderr, WARNING "Field %s not found in section %s\n", field_ident.field_ident, field_ident.section_ident);
}

int get_value(Sections sections_list, FieldIdent_ field_ident, Val return_val) {
    Sections current = sections_list;
    // Find the corresponding section
    while (current != NULL)
    {
        if (strcmp(current->ident, field_ident.section_ident) == 0)
        {
            Fields current_field = current->fields;
            // Find the field in the section
            while (current_field != NULL)
            {
                if (strcmp(current_field->ident, field_ident.field_ident) == 0)
                {
                    Val_ current_val = current_field->val;
                    
                    // Resolve references if the value is a reference
                    while (current_val.kind == Ref) {
                        current_val = current_val.u.ref->val;
                    }

                    *return_val = current_val;
                    return 0; // Value found
                }
                current_field = current_field->next;
            }
            fprintf(stderr, WARNING "Field %s not found in section %s\n", field_ident.field_ident, field_ident.section_ident);
            return -2; // Field not found
        }
        current = current->next;
    }
    fprintf(stderr, WARNING "Section %s not found \n", field_ident.section_ident);
    return -1; // Section not found
}

/***************************   Conversions   **************************/

Sections convert_XMLS_to_Sections(XMLS xml)
{
    if (xml == NULL || xml->kind == is_XMLSEmpty)
    {
        return NULL;
    }

    /* Create the head of the sections list */
    Sections sections_list = make_SectionNew(strdup(xml->u.xmlsection_.section_->u.filesection_.ident_));
    sections_list->symbol_loc = xml->u.xmlsection_.section_->symbol_loc;
    sections_list->fields = convert_SBody_to_Fields(xml->u.xmlsection_.section_->u.filesection_.sbody_,
                                                    sections_list);
    Sections last_section = sections_list;
    /* Move to the next field */
    xml = xml->u.xmlsection_.xmls_;

    /* Iterate through the XMLS list and create sections */
    while (xml->kind == is_XMLSection)
    {
        Ident sec_ident = xml->u.xmlsection_.section_->u.filesection_.ident_;
        SymbolLoc_ symbol_loc = xml->u.xmlsection_.section_->symbol_loc;
        SBody sbody = xml->u.xmlsection_.section_->u.filesection_.sbody_;

        check_Section(sections_list, sec_ident);
        Sections new_section = make_SectionNew(strdup(sec_ident));
        new_section->symbol_loc = symbol_loc;
        new_section->fields = convert_SBody_to_Fields(sbody, new_section);
        last_section->next = new_section;
        last_section = new_section;
        /* Move to the next XMLS element */
        xml = xml->u.xmlsection_.xmls_;
    }

    return sections_list;
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
        Inherited_ new_inherit = convert_Inherit_to_Inherited_(sbody->u.sbodywinherit_.inherit_);
        da_push(inherit_section_list, &new_inherit);
        sbody = sbody->u.sbodywinherit_.sbody_;
    }

    if (sbody->kind == is_SBodyEmpty)
    {
        return NULL;
    }

    /* Create the head of the fields list */

    Ident field_ident = sbody->u.sbodywoinherit_.field_->u.sectionfield_.ident_;
    Val_ val = convert_Value_to_Val_(sbody->u.sbodywoinherit_.field_->u.sectionfield_.value_, section_ident);
    SymbolLoc_ symbol_loc = sbody->u.sbodywoinherit_.field_->symbol_loc;

    if (check_Inherit(current_section, val))
    {
        fprintf(stderr, ERROR "The variable %s refers to section %s, but section %s is not inherited.\n",
                field_ident, val.u.unresolved_ref.section_ident, val.u.unresolved_ref.section_ident);
        exit(EXIT_FAILURE);
    }
    Fields fields_list = new_FieldNew(strdup(field_ident), val);
    fields_list->section = current_section;
    fields_list->symbol_loc = symbol_loc;

    Fields last_field = fields_list;
    /* Move to the next field */
    SBodyF sbodyf = sbody->u.sbodywoinherit_.sbodyf_;

    while (sbodyf->kind == is_SBodyField)
    {
        Ident field_ident = sbodyf->u.sbodyfield_.field_->u.sectionfield_.ident_;
        Val_ val = convert_Value_to_Val_(sbodyf->u.sbodyfield_.field_->u.sectionfield_.value_, section_ident);
        SymbolLoc_ symbol_loc = sbodyf->u.sbodyfield_.field_->symbol_loc;

        if (!overwrite_Field(fields_list, field_ident, val))
        {
            if (check_Inherit(current_section, val))
            {
                fprintf(stderr, ERROR "The variable %s refers to section %s, but section %s is not inherited.\n",
                        field_ident, val.u.unresolved_ref.section_ident, val.u.unresolved_ref.section_ident);
                exit(EXIT_FAILURE);
            }
            Fields new_field = new_FieldNew(strdup(field_ident), val);
            new_field->section = current_section;
            new_field->symbol_loc = symbol_loc;

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
    Val_ new;
    switch (value->kind)
    {
    case is_ValueInteger:
        new = new_Int(value->u.valueinteger_.integer_);
        new.symbol_loc = value->symbol_loc;
        return new;
    case is_Value_true:
        new = new_Bool(1);
        new.symbol_loc = value->symbol_loc;
        return new;
    case is_Value_false:
        new = new_Bool(0);
        new.symbol_loc = value->symbol_loc;
        return new;
    case is_ValueString:
        new = new_Str(strdup(value->u.valuestring_.string_));
        new.symbol_loc = value->symbol_loc;
        return new;
    case is_Value1:
        new = new_Ref(new_FieldIdent(strdup(value->u.value1_.ident_), strdup(section_ident)));
        new.symbol_loc = value->symbol_loc;
        return new;
    case is_Value2:
        new = new_Ref(new_FieldIdent(strdup(value->u.value2_.ident_2), strdup(value->u.value2_.ident_1)));
        new.symbol_loc = value->symbol_loc;
        return new;
    default:
        fprintf(stderr, ERROR "Unknown value kind variable in section %s\n", section_ident);
        exit(EXIT_FAILURE);
    }
}

Inherited_ convert_Inherit_to_Inherited_(Inherit val)
{
    Inherited_ inherit = {
        .section_ident = strdup(val->u.inheritsection_.ident_),
        .symbol_loc = val->symbol_loc};
    return inherit;
}

/*****************************   Checks   *****************************/

void check_Section(Sections sections_list, Ident section_ident)
{
    Sections current = sections_list;
    while (current != NULL)
    {
        if (strcmp(current->ident, section_ident) == 0)
        {
            /* TODO: add file and line number? */
            fprintf(stderr, ERROR "Section %s has multiple definitions\n", section_ident);
            exit(EXIT_FAILURE);
        }
        current = current->next;
    }
}

int check_Inherit(Sections section, Val_ val)
{
    DynamicArray *inherit_section_list = section->inherits;
    Ident current_section = section->ident;

    if (val.kind == Ref_unresolved && strcmp(val.u.unresolved_ref.section_ident, current_section) != 0)
    {
        for (size_t i = 0; i < da_size(inherit_section_list); i++)
        {
            Inherited_ inherit_section = *(Inherited_ *)da_get(inherit_section_list, i);
            if (strcmp(inherit_section.section_ident, val.u.unresolved_ref.section_ident) == 0)
            {
                return 0; // Inherit section found
            }
        }
        return 1; // Inherit section not found
    }
    return 0;
}

int overwrite_Field(Fields fields_list, Ident field_ident, Val_ val)
{
    Fields current = fields_list;
    while (current != NULL)
    {
        if (strcmp(current->ident, field_ident) == 0)
        {
            current->val = val;
            /* TODO: warning with line */
            printf(WARNING "Field %s was overwritten with new value in section %s\n", field_ident, current->section->ident);
            return 1; // Field found and overwritten
        }
        current = current->next;
    }
    return 0; // Field not found
}

/*********************   Recursive Destructors   **********************/

void free_Sections(Sections sections_list)
{
    Sections current = sections_list;
    while (current != NULL)
    {
        Sections next_section = current->next;

        free_Fields(current->fields);
        da_free(current->inherits);
        free(current->ident);
        free(current);

        current = next_section;
    }
    sections_list = NULL;
}

void free_Fields(Fields fields_list)
{
    Fields current = fields_list;
    while (current != NULL)
    {
        Fields next_field = current->next;

        free(current->ident);
        free(current->pointed_by.vals);
        free_Val(current->val);
        free(current);

        current = next_field;
    }
    fields_list = NULL;
}

void free_Val(Val_ val)
{
    switch (val.kind)
    {
    case Str:
        free(val.u.str);
        break;
    case Ref_unresolved:
        free(val.u.unresolved_ref.field_ident);
        free(val.u.unresolved_ref.section_ident);
        break;
    default:
        // No dynamic memory to free for other kinds
        break;
    }
}
