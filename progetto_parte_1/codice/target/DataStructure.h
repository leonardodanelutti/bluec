#include "Absyn.h"

struct Sections_;
typedef struct Sections_ *Sections;

struct Fields_;
typedef struct Fields_ *Fields;

struct Val_;
typedef struct Val_ *Val;

typedef struct FieldIdent_
{
    Ident field_ident;
    Ident section_ident;
} FieldIdent_;

struct Sections_
{
    Ident ident;
    Fields fields;
    Sections next;
    /* Sections front; */
    /* TODO: Inherit list to check non-existing section inherited, i.e.:
       "
       <section name=a>
           <inherit>b</inherit> # where b is not defined
       </section>
       "
       should throw a warning or an error reporting inherit usage of a non existing
       section. */
};

/* Create a new section */
Sections make_Section(Ident ident);

/* Push a new section to a linked list of sections,
   checks if inserting a duplicate section */
void push_Section(Sections sections_list, Ident section_ident);

/* Append a section (or sections) on to the end of a linked list of sections */
void append_Section_internal(Sections left_sections_list, Sections right_sections_list);

/* Push a new field on a given section in a valid linked list of sections,
   checks are performed */
void push_FieldToSection(Sections sections_list, FieldIdent_ field_ident, Value val);

/* Delete a section from a valid linked list of Sections */
void delete_Section(Sections sections_list, Ident section_ident);

/* Delete a field from a given section in a valid linked list of sections */
void delete_FieldInSection(Sections sections_list, FieldIdent_ field_ident);

/* TODO: modify */

struct Fields_
{
    Ident ident;
    Value val;
    Fields next;
    /* Fields front; */
    Sections section;
};

/* Create a new field */
Fields make_FieldNew(Ident ident, Value val);

/* Push a new field to the end of a linked list of Fields,
   overwrites duplicate fields */
void push_Field_internal(Fields current, Ident field_ident, Value val);

/* Push a new field to the end of a valid linked list of Fields
   checks are performed */
void push_Field(Fields current, Ident field_ident, Value val);

/* Delete a field from the linked list of Fields */
void delete_Field(Fields current, Ident field_ident);

/* TODO: add modify */

struct Val_
{
    enum
    {
        Int,
        Str,
        Bool,
        Ref,
        Ref_unresolved
    } kind;
    union
    {
        int int_;
        String str;
        int bool_;
        Fields ref;
        FieldIdent_ unresolved_ref;
    } u;
};
