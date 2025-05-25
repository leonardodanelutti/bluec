/* This module implements logging utilities to standardize error and warning messages
   across the project: it provides macros for formatted error and warning messages. */

#ifndef LOGS_HEADER
#define LOGS_HEADER

#include "Parser.h"

/* Colors and font styles */

#define ANSI_COLOR_RED "\x1b[31m"
#define ANSI_COLOR_GREEN "\x1b[32m"
#define ANSI_COLOR_YELLOW "\x1b[33m"
#define ANSI_COLOR_BLUE "\x1b[34m"
#define ANSI_COLOR_MAGENTA "\x1b[35m"
#define ANSI_COLOR_CYAN "\x1b[36m"
#define ANSI_COLOR_RESET "\x1b[0m"

#define ANSI_BOLD "\e[1m"
#define ANSI_BOLD_RESET "\e[m"

/* Error and warning messages */

#define ERROR ANSI_COLOR_RED ANSI_BOLD "ERROR: " ANSI_BOLD_RESET ANSI_COLOR_RESET
#define WARNING ANSI_COLOR_YELLOW ANSI_BOLD "WARNING: " ANSI_BOLD_RESET ANSI_COLOR_RESET

/* Macros that print location information. */

#define ERROR_F(loc, fmt, ...)                                                                                          \
    do                                                                                                                  \
    {                                                                                                                   \
        const void *_loc_tmp = (loc);                                                                                   \
        const char *_file_path = (*(ParsedXML_ *)da_get(parsed_files, current_file)).file_path;                         \
        if (_loc_tmp != NULL)                                                                                           \
            fprintf(stderr, ERROR "%s:%d:%d: " fmt "\n",                                                                \
                    _file_path ? _file_path : "<stdin>",                                                                \
                    ((const YYLTYPE *)_loc_tmp)->first_line, ((const YYLTYPE *)_loc_tmp)->first_column, ##__VA_ARGS__); \
        else                                                                                                            \
            fprintf(stderr, ERROR "%s: " fmt "\n",                                                                      \
                    _file_path ? _file_path : "<stdin>", ##__VA_ARGS__);                                                \
    } while (0)

#define WARNING_F(loc, fmt, ...)                                                                                        \
    do                                                                                                                  \
    {                                                                                                                   \
        const void *_loc_tmp = (loc);                                                                                   \
        const char *_file_path = (*(ParsedXML_ *)da_get(parsed_files, current_file)).file_path;                         \
        if (_loc_tmp != NULL)                                                                                           \
            fprintf(stderr, WARNING "%s:%d:%d: " fmt "\n",                                                              \
                    _file_path ? _file_path : "<stdin>",                                                                \
                    ((const YYLTYPE *)_loc_tmp)->first_line, ((const YYLTYPE *)_loc_tmp)->first_column, ##__VA_ARGS__); \
        else                                                                                                            \
            fprintf(stderr, WARNING "%s: " fmt "\n",                                                                    \
                    _file_path ? _file_path : "<stdin>", ##__VA_ARGS__);                                                \
    } while (0)

#endif /* LOGS_HEADER */