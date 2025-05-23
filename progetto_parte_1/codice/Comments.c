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