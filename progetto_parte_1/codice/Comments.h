#ifndef COMMENTS_HEADER
#define COMMENTS_HEADER

#include "Absyn.h"

typedef struct SymbolLoc_ SymbolLoc_;


#if ! defined YYLTYPE && ! defined YYLTYPE_IS_DECLARED
typedef struct YYLTYPE YYLTYPE;
struct YYLTYPE
{
  int first_line;
  int first_column;
  int last_line;
  int last_column;
};
# define YYLTYPE_IS_DECLARED 1
# define YYLTYPE_IS_TRIVIAL 1
#endif

typedef char *String;

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