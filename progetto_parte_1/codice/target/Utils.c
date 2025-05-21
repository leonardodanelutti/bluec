#include <stdio.h>
#include <stdlib.h>
#include "Utils.h"

XMLS mergeXMLS(XMLS xmls1, XMLS xmls2)
{
    XMLS r = (XMLS)malloc(sizeof(*r));
    r = xmls1;
    while (xmls1->kind != is_XMLSEmpty)
    {
        xmls1 = xmls1->u.xmlsection_.xmls_;
    }
    *xmls1 = *xmls2;
    return r;
}