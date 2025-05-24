#include <stdio.h>
#include <stdlib.h>
#include "DataStructure.h"
#include "Parser.h"
#include "ResolveRefs.h"
#include "PrettyPrinter.h"

Sections structure_FromFile(char *filename)
{
    FILE *input;
    XMLS parse_tree;
    if (filename)
    {
        input = fopen(filename, "r");
        if (!input)
        {
            fprintf(stderr, ERROR "Cannot open file %s\n", filename);
            exit(1);
        }
    }
    else
        input = stdin;
    /* The default entry point is used. For other options see Parser.h */
    parse_tree = pXML(input, filename);
    /* Close file if opened */
    if (filename)
        fclose(input);
    /* Create custom data structure */
    Sections sections = convert_XMLS_to_Sections(parse_tree);
    free_XMLS(parse_tree);
    /* Resolve references */
    resolveSections(sections);
    return sections;
}

int main(int argc, char **argv)
{
    // Init the data structure from a file
    Sections section_list = structure_FromFile("../tests/testConsegna.txt");
    printf("Structure at start:\n\n");
    printSections(section_list);

    // Check the 'get_value' function
    Val_ val = new_Bool(0);
    if (get_value(section_list, new_FieldIdent("miao", "nomesez3"), &val) == 0)
    {
        if (val.kind == Int && val.u.int_ == 3)
        {
            printf("Successfully retrieved value\n");
        }
        else
        {
            printf(ERROR "Unexpected value retrived.\n");
        }
    } else {
        // Section or field not found
    }

    // Check the 'push_Section' function

    push_Section(section_list, "new_section");

    // Check the 'push_FieldToSection' function

    push_FieldToSection(section_list, new_FieldIdent("new_field1", "new_section"), new_Ref(new_FieldIdent("var3", "nomesez1")));
    // Modify the value of an existing field
    push_FieldToSection(section_list, new_FieldIdent("var1", "nomesez1"), new_Ref(new_FieldIdent("new_field1", "new_section")));

    // Check the 'delete_FieldInSection' function

    // Check the 'delete_Section' function

    printf("\nAfter:\n");
    printSections(section_list);
}