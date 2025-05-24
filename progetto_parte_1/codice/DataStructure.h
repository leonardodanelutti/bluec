#ifndef DATASTRUCTURE_HEADER
#define DATASTRUCTURE_HEADER

#include "Absyn.h"
#include "Utils.h"

/*******************   Data Structure Definitions   *******************/

/* Reference of a Sections_ struct */
struct Sections_;
typedef struct Sections_ *Sections;

/* Global variable pointing at the first section */
extern Sections sections;

/* Reference of a Fields_ struct */
struct Fields_;
typedef struct Fields_ *Fields;

/* An identifier for a Field inside a Section */
typedef struct FieldIdent_
{
    Ident field_ident;
    Ident section_ident;
} FieldIdent_;

/* A value of a Field */
typedef struct Val_
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
    SymbolLoc_ symbol_loc;
} Val_;

/* Reference of a Val_ struct */
typedef Val_ *Val;

/* Struct definiton of an Inherit */
typedef struct Inherited_
{
    Ident section_ident;
    SymbolLoc_ symbol_loc;
} Inherited_;

/* A linked list of Sections */
struct Sections_
{
    Ident ident;
    Fields fields;
    SymbolLoc_ symbol_loc;
    Sections next;
    // Inherited_
    DynamicArray *inherits;
};

/* A linked list of Fields */
struct Fields_
{
    enum
    {
        Visited,
        Not_visited
    } kind;
    Ident ident;
    Val_ val;
    SymbolLoc_ symbol_loc;
    Fields next;
    Sections section;
    // Fields
    DynamicArray *pointed_by;
};

/**************************   Constructors   **************************/

/* Create a new Section */
Sections new_Section(Ident ident);

/* Create a new Field */
Fields new_FieldNew(Ident ident, Val_ val);

/* Create a new Val */
Val_ new_Int(int int_);
Val_ new_Str(String str);
Val_ new_Bool(int bool_);
Val_ new_Ref(FieldIdent_ ref);

/* Create a new Field identifier */
FieldIdent_ new_FieldIdent(Ident field_ident, Ident section_ident);

/*********************   Data Structure Methods   *********************/

/* Push a new section to a linked list of sections,
   checks if inserting a duplicate section */
void push_Section(Sections sections_list, Ident section_ident);

/* Push a new field on a given section in a valid linked list of sections,
   checks are performed */
void push_FieldToSection(Sections sections_list, FieldIdent_ field_ident, Val_ val);

/* Delete a section from a valid linked list of Sections */
void delete_Section(Sections sections_list, Ident section_ident);

/* Delete a field from a given section in a valid linked list of sections */
void delete_FieldInSection(Sections sections_list, FieldIdent_ field_ident);

/* Return a value associeted to a Field, the value returned is never a Ref */
int get_value(Sections sections_list, FieldIdent_ field_ident, Val return_val);

/***************************   Conversions   **************************/

/* Converts the abstract syntax tree to the datastruct defined by us */
Sections convert_XMLS_to_Sections(XMLS xml);

/*********************   Recursive Destructors   **********************/

void free_Sections(Sections sections_list);
void free_Fields(Fields fields_list);
void free_Val(Val_ val);

#endif
