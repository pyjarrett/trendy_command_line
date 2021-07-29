with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;

package Trendy_Command_Line is
    -- Command lines are a long list of elements to be considered singularly,
    -- in turn, to generate a space of configuration values.
    --
    -- https://www.gnu.org/software/libc/manual/html_node/Argument-Syntax.html
    --
    -- IEEE Std 1003.1-2017
    -- 12. Utility Conventions
    -- https://pubs.opengroup.org/onlinepubs/9699919799/
    --
    -- The whole list of incoming strings is called "arguments".
    -- Arguments which begin with "-' are options.

    package ASU renames Ada.Strings.Unbounded;

    package String_Vectors is new Ada.Containers.Vectors(Index_Type   => Positive,
                                                         Element_Type => ASU.Unbounded_String,
                                                         "="          => ASU."=");

    -- An option was found which might not exist.
    Unknown_Option : exception;

    -- The user provided some weird token that wasn't recognized.
    Unknown_Token : exception;

end Trendy_Command_Line;
