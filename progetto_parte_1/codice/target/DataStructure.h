#include "Absyn.h"

struct Sections_;
typedef struct Sections_ *Sections;

struct Fields_;
typedef struct Fields_ *Fields;

struct Val_;
typedef struct Val_ *Val;

struct Sections_
{
    Ident ident;
    Fields fields;
    Sections next;
    /* Sections front; */
};

struct Fields_
{
    Ident ident;
    Value val;
    Fields next;
    /* Fields front; */
    Sections section;
};

struct Val_
{
    enum
    {
        Int,
        Str,
        Bool,
        Ref,
        Ref_null
    } kind;
    union
    {
        int int_;
        String str;
        int bool_;
        Fields section;
    } u;
};
