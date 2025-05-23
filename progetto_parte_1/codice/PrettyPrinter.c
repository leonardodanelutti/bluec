#include "PrettyPrinter.h"


struct List_;
typedef struct List_ *List;

struct List_ {
    Ident ident;
    List next;
};

// Funzione principale di stampa delle sezioni
void printSections(Sections section) {
    while (section != NULL) {
        printf("<section name=%s>", section->ident);

        Fields field = section->fields;
        printInherits(*(section->inherits));   // stampa inherits 
        printFields(field);                    // stampa fields

        printf("</section>\n");
        printf("file-lines: %s.%d-%d\n", section->symbol_loc.file_path, section->symbol_loc.top_loc.first_line, section->symbol_loc.bottom_loc.last_line);
        section = section->next;
    }
}


// Stampa tutti i field di una sezione
void printFields(Fields field) {
    while (field != NULL) {
        printf("  <field name=%s>", field->ident);
        printVal(field->val, field->section->ident);
        printf("</field>");
        printf("lines: %d-%d\n", field->symbol_loc.top_loc, field->symbol_loc.bottom_loc);
        field = field->next;
    }
}

// Stampa il valore del field
void printVal(Val_ val, Ident section_ident) {
    switch (val.kind) {
        case Int:
            printf(" %d ", val.u.int_);
            printf("lines: %d-%d", val.symbol_loc.top_loc, val.symbol_loc.bottom_loc);
            break;
        case Str:
            printf(" \"%s\" ", val.u.str);
            printf("lines: %d-%d", val.symbol_loc.top_loc, val.symbol_loc.bottom_loc);

            break;
        case Bool:
            printf(val.u.bool_ ? " true " : " false ");
            printf("lines: %d-%d", val.symbol_loc.top_loc, val.symbol_loc.bottom_loc);

            break;
        case Ref: {
            Ident sectionInherit = val.u.ref->section->ident;
            if (sectionInherit == section_ident) {
                printf(" $%s ", val.u.ref->ident);
                printf("lines: %d-%d", val.symbol_loc.top_loc, val.symbol_loc.bottom_loc);

            } else {
                printf(" $%s.%s ", sectionInherit, val.u.ref->ident);
                printf("lines: %d-%d", val.symbol_loc.top_loc, val.symbol_loc.bottom_loc);
            }
            break;
        }
        case Ref_unresolved:
            printf(" &%s.%s ", val.u.unresolved_ref.section_ident, val.u.unresolved_ref.field_ident);
            printf("lines: %d-%d\n", val.symbol_loc.top_loc, val.symbol_loc.bottom_loc);
            break;
        default:
            fprintf(stderr, "Unknown value type\n");
            exit(EXIT_FAILURE);
    }
}


// Stampa tutti gli inherit di una sezione
void printInherits(DynamicArray inherits) {
    for (size_t i = 0; i < inherits.size; i++) {
        Inherited_ inherit = *(Inherited_ *)da_get(&inherits, i);
        printf("  <inherit> %s </inherit>\n", inherit.section_ident);
        printf("lines: %d-%d\n", inherit.symbol_loc.top_loc, inherit.symbol_loc.bottom_loc);
    }
}

//funzione per stampare i commenti
void printComments(Comment_ comment) {
    printf("Comment: %s\n", comment.comment);
    printf("File: %s\n", comment.file_path);
    printf("Line: %d\n", comment.line);
}