#include "PrettyPrinter.h"


struct List_;
typedef struct List_ *List;

struct List_ {
    Ident ident;
    List next;
};

int not_in_list(Ident section_ident, List list);
void free_list(List list);
List add_to_list(Ident ident, List list);

// Funzione principale di stampa delle sezioni
void printSections(Sections section) {
    while (section != NULL) {
        printf("<section name=%s>\n", section->ident);

        Fields field = section->fields;
        printInherits(field, section->ident);  // stampa inherits unici
        printFields(field);                    // stampa fields

        printf("</section>\n");
        section = section->next;
    }
}


// Stampa tutti i field di una sezione
void printFields(Fields field) {
    while (field != NULL) {
        printf("  <field name=%s>", field->ident);
        printVal(field->val, field->section->ident);
        printf("</field>\n");
        field = field->next;
    }
}

// Stampa il valore del field
void printVal(Val_ val, Ident section_ident) {
    switch (val.kind) {
        case Int:
            printf(" %d ", val.u.int_);
            break;
        case Str:
            printf(" \"%s\" ", val.u.str);
            break;
        case Bool:
            printf(val.u.bool_ ? " true " : " false ");
            break;
        case Ref: {
            Ident sectionInherit = val.u.ref->section->ident;
            if (sectionInherit == section_ident) {
                printf(" $%s ", val.u.ref->ident);
            } else {
                printf(" $%s.%s ", sectionInherit, val.u.ref->ident);
            }
            break;
        }
        case Ref_unresolved:
            printf(" &%s.%s ", val.u.unresolved_ref.section_ident, val.u.unresolved_ref.field_ident);
            break;
        default:
            fprintf(stderr, "Unknown value type\n");
            exit(EXIT_FAILURE);
    }
}


// Stampa tutti gli inherit unici da una lista di field
void printInherits(Fields field, Ident section_ident) {
    List list = NULL;

    while (field != NULL) {
        if (field->val.kind == Ref) {
            Ident sectionInherit = field->val.u.ref->section->ident;
            if (strcmp(sectionInherit, section_ident) != 0 &&
                not_in_list(sectionInherit, list)) {

                list = add_to_list(sectionInherit, list);
                printf("  <inherit>%s</inherit>\n", sectionInherit);
            }
        }
        field = field->next;
    }

    // Libera la lista al termine
    free_list(list);
}

// Ritorna 1 se l'ident non è già nella lista, 0 altrimenti
int not_in_list(Ident section_ident, List list) {
    while (list != NULL) {
        if (strcmp(list->ident, section_ident) == 0) {
            return 0;
        }
        list = list->next;
    }
    return 1;
}

// Aggiunge un nuovo ident all'inizio della lista
List add_to_list(Ident ident, List list) {
    List new_node = (List)malloc(sizeof(struct List_));
    if (!new_node) {
        fprintf(stderr, "Memory allocation failed for list node\n");
        exit(EXIT_FAILURE);
    }
    new_node->ident = ident;
    new_node->next = list;
    return new_node;
}

// Libera tutta la memoria della lista
void free_list(List list) {
    while (list != NULL) {
        List tmp = list;
        list = list->next;
        free(tmp);
    }
}








/*

void printInherits(Fields field, Ident section_ident) {
    //lista di ident
    List list = NULL;

    List ident_node;
    while (field != NULL) {
        if (field->val.kind == Ref) {
            Ident sectionInherit = field->val.u.ref->section->ident;
            if (not_in_list(sectionInherit, list) && (sectionInherit != section_ident)) {
                ident_node = (List)malloc(sizeof(struct List_));
                if (ident_node == NULL) {
                    fprintf(stderr, "Memory allocation failed for new section\n");
                    exit(EXIT_FAILURE);
                }
                ident_node->ident = sectionInherit;
                ident_node->next = list;
                list = ident_node;        
                printf("  <inherit> %s </inherit>\n", sectionInherit);
            };
        }
        field = field->next;
    }
}
void printInherits(Val_ val, Ident section_ident) {
    //lista di ident
    List list = NULL;

    List ident_node;
    if (val.kind == Ref) {
    Ident sectionInherit = val.u.ref->section->ident;
    if (not_in_list(sectionInherit, list)) {
        ident_node = (List)malloc(sizeof(struct List_));
        if (ident_node == NULL) {
            fprintf(stderr, "Memory allocation failed for new section\n");
            exit(EXIT_FAILURE);
        }
        ident_node->ident = sectionInherit;
        ident_node->next = list;
        list = ident_node;        
        printf(" <inherit>%s</inherit>\n", sectionInherit);
    };
  }
}

int not_in_list(Ident section_ident, List list) {
    while (list != NULL) {
        if (strcmp(list->ident, section_ident) == 0) {
            return 0; // found
        }
        list = list->next;
    }
    return 1; // not found
}

*/