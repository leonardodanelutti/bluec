#ifndef COMMENTS_HEADER
#define COMMENTS_HEADER

#include "Bison.h"
#include "Absyn.h"

typedef struct SymbolLoc_ SymbolLoc_;

/* `SymbolLoc_` is used to store the location of a symbol (terminal or non-terminal) in a
   file. It is used as a connection with the lexer to retrieve the potential comments
   associated with a symbol. */
struct SymbolLoc_
{
    // The location of the symbol in the file
    YYLTYPE loc;
    // The file where the symbol is located; NULL if read from stdin
    String file_path;
};

#endif /* COMMENTS_H */