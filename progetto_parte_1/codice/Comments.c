#include <stdlib.h>
#include <string.h>
#include "Comments.h"

DynamicArray *comments = NULL;

/* Helper function that creates a new `Comment_`. */
Comment_ make_Comment(unsigned int line, String file_path, String comment)
{
    Comment_ new_comment = {
        .line = line,
        .file_path = file_path,
        .comment = comment};
    return new_comment;
}

void push_Comment(unsigned int line, String file_path, String comment)
{
    if (comments == NULL)
    {
        comments = da_init(sizeof(Comment_));
    }
    Comment_ new_comment = make_Comment(line, file_path, comment);
    da_push(comments, &new_comment);
}

void detect_Comments(unsigned int first_line, String file_path, String str)
{
    int is_in_comment = 0;
    size_t comment_start = 0;
    for (size_t i = 0; str[i] != '\0'; i++) {
        switch (str[i]) {
            case '\n':
                if (is_in_comment) {
                    is_in_comment = 0;
                    size_t len = i - comment_start;
                    char *dest = malloc(len + 1);
                    strncpy(dest, str + comment_start, len);
                    push_Comment(first_line, file_path, dest);
                }
                first_line++;
                break;
            case '#':
                if (!is_in_comment) {
                    comment_start = i;
                    is_in_comment = 1;
                }
                break;
        }
    }
}