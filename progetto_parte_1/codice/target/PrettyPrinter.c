#include "PrettyPrinter.h"

//
void printSections(Sections section) {
  while (section != NULL) {
    printf("<section name=%s>\n", section->ident);
    Fields field = section->fields;
    printFields(field);
    printf("</section>\n");
    section = section->next;
  }
}

void printFields(Fields field) {
  while (field != NULL) {
    printf("  <field name=%s>\n", field->ident);
    printVal(field->val, field->section->ident);
    printf("  </field>\n");
    field = field->next;
  }
}

void printVal(Val_ val, Ident section_ident) {
  switch (val.kind) {
    case Int:
      printf(" %d ", val.u.int_);
      break;
    case Str:
      printf(" %s ", val.u.str);
      break;
    case Bool:
      if (val.u.bool_) {
        printf(" true ");
      } else {
        printf(" false ");
      }
      break;
    case Ref:
      if (val.u.ref->section->ident == section_ident) {
        printf(" $%s ", val.u.ref->ident);
      } else {
        //add inherit
        printf(" $%s.%s ", val.u.ref->section->ident, val.u.ref->ident);
      }
      break;
/* case Ref_unresolved: */
      /* printf(" $ ", val.u.unresolved_ref.field_ident); */
      /* break; */
    default:
      fprintf(stderr, "Unknown value type\n");
      exit(EXIT_FAILURE);
  }
}