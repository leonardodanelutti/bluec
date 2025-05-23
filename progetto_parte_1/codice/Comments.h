#ifndef COMMENTS_HEADER
#define COMMENTS_HEADER

#include <stddef.h>
#include "Utils.h"

#if !defined YYLTYPE && !defined YYLTYPE_IS_DECLARED
typedef struct YYLTYPE YYLTYPE;
struct YYLTYPE
{
    int first_line;
    int first_column;
    int last_line;
    int last_column;
};
#define YYLTYPE_IS_DECLARED 1
#define YYLTYPE_IS_TRIVIAL 1
#endif

typedef char *String;

/* Global variable that stores an array of the comments */
extern DynamicArray *comments;

typedef struct SymbolLoc_ SymbolLoc_;
typedef struct Comment_ Comment_;

/* `SymbolLoc_` is used to store the location of a symbol (terminal or non-terminal) in a
   file. It is used as a connection with the lexer to retrieve the potential comments
   associated with a symbol. */
struct SymbolLoc_
{
    // The location of the top symbol in the file
    YYLTYPE top_loc;
    // The location of the symbol in the file
    YYLTYPE bottom_loc;
    // The file where the symbol is located; NULL if read from stdin
    String file_path;
};

#define DEFAULT_SYMBOL_LOC_ { {0, 0, 0, 0}, {0, 0, 0, 0}, NULL }

/* A data structure that holds positional information about a comment. */
struct Comment_
{
    // The location of the comment in the file
    unsigned int line;
    // The file where the comment is located; NULL if read from stdin
    String file_path;
    // The comment
    String comment;
};

/* Pushes a new comment to the global variable `comments`. */
void push_Comment(unsigned int line, String file_path, String comment);

/* Pushes all comments in a string to the global variable `comments`. */
void detect_Comments(unsigned int first_line, String file_path, String str);

#endif /* COMMENTS_H */