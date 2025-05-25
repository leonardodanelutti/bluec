#include <stdlib.h>
#include <string.h>
#include "Comments.h"

DynamicArray *comments = NULL;

/* Free the memory used by the global variable `comments`. */
void free_Comments(void)
{
    if (comments != NULL)
    {
        da_free(comments);
        comments = NULL;
    }
}

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
    // if we are currently in a comment
    int is_in_comment = 0;
    // the start of a comment
    size_t comment_start = 0;
    for (size_t i = 0; str[i] != '\0'; i++)
    {
        switch (str[i])
        {
        case '\n':
            // if we were in a comment and we reach a newline, we push the comment and reset the state
            if (is_in_comment)
            {
                is_in_comment = 0;
                size_t len = i - comment_start;
                char *dest = malloc(len + 1);
                strncpy(dest, str + comment_start, len);
                push_Comment(first_line, file_path, dest);
            }
            first_line++;
            break;
        case '#':
            // if we are not currently in a comment and we find a '#', we start a new comment
            if (!is_in_comment)
            {
                comment_start = i;
                is_in_comment = 1;
            }
            break;
        }
    }
}