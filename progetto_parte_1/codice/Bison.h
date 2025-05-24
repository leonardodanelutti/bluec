/* A Bison parser, made by GNU Bison 3.7.5.  */

/* Bison interface for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015, 2018-2021 Free Software Foundation,
   Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* DO NOT RELY ON FEATURES THAT ARE NOT DOCUMENTED in the manual,
   especially those whose name start with YY_ or yy_.  They are
   private implementation details that can be changed or removed.  */

#ifndef YY_GRAMMAR_BISON_H_INCLUDED
# define YY_GRAMMAR_BISON_H_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 1
#endif
#if YYDEBUG
extern int grammar_debug;
#endif

/* Token kinds.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    YYEMPTY = -2,
    YYEOF = 0,                     /* "end of file"  */
    YYerror = 256,                 /* error  */
    YYUNDEF = 257,                 /* "invalid token"  */
    _ERROR_ = 258,                 /* _ERROR_  */
    _DOLLAR = 259,                 /* _DOLLAR  */
    _DOT = 260,                    /* _DOT  */
    _LT = 261,                     /* _LT  */
    _SYMB_3 = 262,                 /* _SYMB_3  */
    _EQ = 263,                     /* _EQ  */
    _GT = 264,                     /* _GT  */
    _KW_false = 265,               /* _KW_false  */
    _KW_field = 266,               /* _KW_field  */
    _KW_inherit = 267,             /* _KW_inherit  */
    _KW_name = 268,                /* _KW_name  */
    _KW_section = 269,             /* _KW_section  */
    _KW_true = 270,                /* _KW_true  */
    _STRING_ = 271,                /* _STRING_  */
    _INTEGER_ = 272,               /* _INTEGER_  */
    _IDENT_ = 273                  /* _IDENT_  */
  };
  typedef enum yytokentype yytoken_kind_t;
#endif

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
union YYSTYPE
{
#line 55 "grammar.y"

  int    _int;
  char   _char;
  double _double;
  char*  _string;
  XML xml_;
  XMLS xmls_;
  Import import_;
  Section section_;
  SBody sbody_;
  SBodyF sbodyf_;
  Inherit inherit_;
  Field field_;
  Value value_;

#line 98 "Bison.h"

};
typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif

/* Location type.  */
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



int grammar_parse (yyscan_t scanner, YYSTYPE *result);

#endif /* !YY_GRAMMAR_BISON_H_INCLUDED  */
