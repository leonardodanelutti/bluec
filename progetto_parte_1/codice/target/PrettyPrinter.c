#include "PrettyPrinter.h"


struct List_;
typedef struct List_ *List;

struct List_ {
    Ident ident;
    List next;
};

int not_in_list(Ident section_ident, List list);

Ident sectionInherit;

List list;
List tmp;

//
void printSections(Sections section) {
  while (section != NULL) {
    printf("<section name=%s>\n", section->ident);
    Fields field = section->fields;
    printInherits2(field, section->ident);
    printFields(field);
    printf("</section>\n");
    section = section->next;
  }
}

void printFields(Fields field) {
  while (field != NULL) {
    printf("  <field name=%s>", field->ident);
    printInherits(field->val, field->section->ident);
    printVal(field->val, field->section->ident);
    printf(" </field>\n");
    field = field->next;
  }
}

void printVal(Val_ val, Ident section_ident) {
  switch (val.kind) {
    case Int:
      printf(" %d ", val.u.int_);
      break;
    case Str:
      printf(" \"%s\" ", val.u.str);
      break;
    case Bool:
      if (val.u.bool_) {
        printf(" true ");
      } else {
        printf(" false ");
      }
      break;
    case Ref:
    sectionInherit = val.u.ref->section->ident;
      if (sectionInherit == section_ident) {
        printf(" $%s ", val.u.ref->ident);
      } else {
        printf(" $%s.%s ", val.u.ref->section->ident, val.u.ref->ident);
      }
      break;
    case Ref_unresolved:
      printf(" &%s.%s ", val.u.unresolved_ref.section_ident, val.u.unresolved_ref.field_ident);
      break;
    default:
      fprintf(stderr, "Unknown value type\n");
      exit(EXIT_FAILURE);
  }
}

void printInherits2(Fields field, Ident section_ident) {
    //lista di ident
    List list = NULL;

    List ident_node;
    while (field != NULL) {
        if (field->val.kind == Ref) {
            Ident sectionInherit = field->val.u.ref->section->ident;
            if (not_in_list(sectionInherit, list)) {
                ident_node = (List)malloc(sizeof(struct List_));
                if (ident_node == NULL) {
                    fprintf(stderr, "Memory allocation failed for new section\n");
                    exit(EXIT_FAILURE);
                }
                ident_node->ident = sectionInherit;
                ident_node->next = list;
                list = ident_node;        
                printf("  <inherit>%s</inherit>\n", sectionInherit);
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