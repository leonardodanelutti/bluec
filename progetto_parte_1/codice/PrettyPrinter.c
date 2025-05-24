#include "PrettyPrinter.h"

size_t i = 0;


// This function retrieves the current comment based on the index `i`.
Comment_ get_current_comment(size_t i) {
    Comment_ *current_comment = (Comment_ *)da_get(comments, i);
    if (current_comment == NULL) {
        fprintf(stderr, ERROR "No comments found\n");
        exit(EXIT_FAILURE);
    }
    return *current_comment;
}

// This function prints the sections of the XML structure
void printSections(Sections section) {
    while (section != NULL) {
        if (comments != NULL) {
            while (i < comments->size) {
                Comment_ *current_comment = (Comment_ *)da_get(comments, i);
                if (section->symbol_loc.file_path == current_comment->file_path &&
                    (unsigned int) section->symbol_loc.top_loc.last_line >= current_comment->line) {
                    printf("%s\n", current_comment->comment);
                    i++;
                } else {
                    break;
                }
            }
        }

        printf("<section name=%s>\n", section->ident);

        printInherits(*(section->inherits));   // stampa inherits 
        printFields(section->fields);          // stampa fields

        if (comments != NULL) {
            while (i < comments->size) {
                Comment_ *current_comment = (Comment_ *)da_get(comments, i);
                if (section->symbol_loc.file_path == current_comment->file_path &&
                    (unsigned int) section->symbol_loc.bottom_loc.last_line >= current_comment->line) {
                    printf("%s\n", current_comment->comment);
                    i++;
                } else {
                    break;
                }
            }
        }

        printf("</section>\n");
        section = section->next;
    }
    i = 0;
}

// This function prints the inherits of a section
void printInherits(DynamicArray inherits) {
    for (size_t j = 0; j < inherits.size; j++) {
        Inherited_ inherit = *(Inherited_ *)da_get(&inherits, j);
        if (comments != NULL) {
            while (i < comments->size) {
                Comment_ *current_comment = (Comment_ *)da_get(comments, i);
                if (inherit.symbol_loc.file_path == current_comment->file_path &&
                    (unsigned int) inherit.symbol_loc.top_loc.last_line >= current_comment->line) {
                    printf("  %s\n", current_comment->comment);
                    i++;
                } else {
                    break;
                }
            }
        }
        printf("  <inherit>");

        printf("%s", inherit.section_ident);

        if (comments != NULL) {
            while (i < comments->size) {
                Comment_ *current_comment = (Comment_ *)da_get(comments, i);
                if (inherit.symbol_loc.file_path == current_comment->file_path &&
                    (unsigned int) inherit.symbol_loc.bottom_loc.last_line >= current_comment->line) {
                    printf("\n  %s\n", current_comment->comment);
                    i++;
                } else {
                    break;
                }
            }
        }
         printf(" </inherit>\n");
    }
}

// This function prints the fields of a section
void printFields(Fields field) {
    while (field != NULL) {
        if (comments != NULL) {
            while (i < comments->size) {
                Comment_ *current_comment = (Comment_ *)da_get(comments, i);
                if (field->symbol_loc.file_path == current_comment->file_path &&
                    (unsigned int) field->symbol_loc.top_loc.last_line >= current_comment->line) {
                    printf("  %s\n", current_comment->comment);
                    i++;
                } else {
                    break;
                }
            }
        }
        printf("  <field name=%s>", field->ident);
        printVal(field->val, field->section->ident);
        if (comments != NULL) {
            while (i < comments->size) {
                Comment_ *current_comment = (Comment_ *)da_get(comments, i);
                if (field->symbol_loc.file_path == current_comment->file_path &&
                    (unsigned int) field->symbol_loc.bottom_loc.last_line >= current_comment->line) {
                    printf("\n   %s\n  ", current_comment->comment);
                    i++;
                } else {
                    break;
                }
            }
        }
        printf("</field>\n");
        field = field->next;
    }
}

// This function prints the value of a field.
void printVal(Val_ val, Ident section_ident) {
    if (comments != NULL){
        while (i < comments->size) {
            Comment_ *current_comment = (Comment_ *)da_get(comments, i);
            if (val.symbol_loc.file_path == current_comment->file_path &&
                (unsigned int) val.symbol_loc.top_loc.first_line >= current_comment->line) {
                printf("  %s\n", current_comment->comment);
                i++;
            } else {
                break;
            }
        }
    }

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