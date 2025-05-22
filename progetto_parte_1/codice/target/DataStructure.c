#include <stdio.h>
#include <stdlib.h>

#include "DataStructure.h"

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
    new_field->next = NULL;
    new_field->section = NULL;
    return new_field;
}


/* Convert XMLS to a linked list of sections */

void check_Section(Sections sections_list, Ident section_ident);
Fields convert_SBody_to_Fields(SBody sbody, Ident section_ident);
Val_ convert_Value_to_Val_(Value value, Ident section_ident);
void check_Inherit(Ident *inherit_section_list, Val_ val, Ident current_section);
int overwrite_Field(Fields fields_list, Ident field_ident, Val_ val);


Sections convert_XMLS_to_Sections(XMLS xml) {
    if (xml == NULL || xml->kind == is_XMLSEmpty) {
        return NULL;
    }

    /* Create the head of the sections list */
    Sections sections_list = make_SectionNew(xml->u.xmlsection_.section_->u.filesection_.ident_);
    sections_list->fields = convert_SBody_to_Fields(xml->u.xmlsection_.section_->u.filesection_.sbody_,
                                                    xml->u.xmlsection_.section_->u.filesection_.ident_);
    Sections last_section = sections_list;
    /* Move to the next field */
    xml = xml->u.xmlsection_.xmls_;

    /* Iterate through the XMLS list and create sections */
    while (xml->kind == is_XMLSection) {
        Ident sec_ident = xml->u.xmlsection_.section_->u.filesection_.ident_;
        SBody sbody = xml->u.xmlsection_.section_->u.filesection_.sbody_;

        check_Section(sections_list, sec_ident);
        Sections new_section = make_SectionNew(sec_ident);
        new_section->fields = convert_SBody_to_Fields(sbody, sec_ident);
        last_section->next = new_section;
        last_section = new_section;
        /* Move to the next XMLS element */
        xml = xml->u.xmlsection_.xmls_;
    }

    return sections_list;
}

/* Check if a section is already present in a section list */
void check_Section(Sections sections_list, Ident section_ident) {
    Sections current = sections_list;
    while (current != NULL) {
        if (current->ident == section_ident) {
            /* TODO: add file and line number? */
            fprintf(stderr, "Duplicate section with identifier: %s\n", section_ident);
            exit(EXIT_FAILURE);
        }
        current = current->next;
    }
}


Fields convert_SBody_to_Fields(SBody sbody, Ident section_ident) {
    if (sbody == NULL || sbody->kind == is_SBodyEmpty) {
        return NULL;
    }

    /* Construct a list of all identifiers in the inherits */

    Ident *inherit_section_list = malloc(sizeof(Ident) * 10);
    long unsigned int inherit_list_count = 0;
    long unsigned int inherit_list_size = 10;

    while (sbody->kind == is_SBodywInherit) {
        if (inherit_list_count >= inherit_list_size) {
            inherit_list_size += 5;
            inherit_section_list = realloc(inherit_section_list, sizeof(Ident) * inherit_list_size);
            if (inherit_section_list == NULL) {
                fprintf(stderr, "Memory allocation failed for inherit section list\n");
                exit(EXIT_FAILURE);
            }
        }
        inherit_section_list[inherit_list_count++] = sbody->u.sbodywinherit_.inherit_->u.inheritsection_.ident_;
        sbody = sbody->u.sbodywinherit_.sbody_;
    }

    /*  */
    if (sbody->kind == is_SBodyEmpty) {
        return NULL;
    }

    /* Create the head of the fields list */

    Ident field_ident = sbody->u.sbodywoinherit_.field_->u.sectionfield_.ident_;
    Val_ value = convert_Value_to_Val_(sbody->u.sbodywoinherit_.field_->u.sectionfield_.value_, section_ident);
    check_Inherit(inherit_section_list, value, section_ident);
    Fields fields_list = make_FieldNew(field_ident, value);
    Fields last_field = fields_list;
    /* Move to the next field */
    SBodyF sbodyf = sbody->u.sbodywoinherit_.sbodyf_;

    while (sbodyf->kind == is_SBodyField) {
        Ident field_ident = sbodyf->u.sbodyfield_.field_->u.sectionfield_.ident_;
        Value value = sbodyf->u.sbodyfield_.field_->u.sectionfield_.value_;
        Val_ val = convert_Value_to_Val_(value, section_ident);

        if (!overwrite_Field(fields_list, field_ident, val)) {
            check_Inherit(inherit_section_list, val, section_ident);
            Fields new_field = make_FieldNew(field_ident, val);
            last_field->next = new_field;
            last_field = new_field;
        }
        
        /* Move to the next field */
        sbodyf = sbodyf->u.sbodyfield_.sbodyf_;
    }

    return fields_list;
}

Val_ convert_Value_to_Val_(Value value, Ident section_ident) {
    switch (value->kind) {
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

Val_ new_Int(int int_) {
    Val_ val = {
        .kind = Int,
        .u.int_ = int_
    };
    return val;
}

Val_ new_Str(String str) {
    Val_ val = {
        .kind = Str,
        .u.str = str
    };
    return val;
}

Val_ new_Bool(int bool_) {
    Val_ val = {
        .kind = Bool,
        .u.bool_ = bool_
    };
    return val;
}

Val_ new_Ref(FieldIdent_ ref) {
    Val_ val = {
        .kind = Ref_unresolved,
        .u.unresolved_ref = ref
    };
    return val;
}

FieldIdent_ new_FieldIdent(Ident field_ident, Ident section_ident) {
    FieldIdent_ field_ident_ = {
        .field_ident = field_ident,
        .section_ident = section_ident
    };
    return field_ident_;
}

/* Check if non local variables are inherited */
void check_Inherit(Ident *inherit_section_list, Val_ val, Ident current_section) {
    
}

/* Overwrite a field in a list of field if identifier is matched */
int overwrite_Field(Fields fields_list, Ident field_ident, Val_ val) {
  return 0;
}
