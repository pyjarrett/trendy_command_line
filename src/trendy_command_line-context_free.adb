with Ada.Characters.Handling;

package body Trendy_Command_Line.Context_Free is


    -- Valid option characters after the first hyphen.
    -- TODO: This might need to be reduced to only letters, using an additional
    -- check only 3rd -> last also allowing a hyphen.
    function Is_Valid_Option_Character (C : Character) return Boolean is
    begin
        return Ada.Characters.Handling.Is_Graphic (C)
            and then not Ada.Characters.Handling.Is_Space (C);
    end Is_Valid_Option_Character;

    ---------------------------------------------------------------------------
    -- Context-free checks
    ---------------------------------------------------------------------------

    function Is_Long_Option (Str : String) return Boolean is
    begin
        return
            Str'Length > 2 and then Str (Str'First) = '-' and then Str (Str'First + 1) = '-'
            and then Str (Str'First + 2) /= '-'
            and then (for all C in (Str'First + 2) .. Str'Last => Is_Valid_Option_Character (Str (C)));
    end Is_Long_Option;

    function Is_Short_Option (Str : String) return Boolean is
    begin
        return
            Str'Length = 2 and then Str (Str'First) = '-'
            and then Is_Valid_Option_Character (Str (Str'First + 1))
            and then Str (Str'First + 1) /= '-';
    end Is_Short_Option;

    -- It's not possible to disambiguate between a short option with an argument
    -- and a short option group of multiple options without any arguments.
    --
    -- https://www.gnu.org/software/libc/manual/html_node/Argument-Syntax.html
    --     "An option and its argument may or may not appear as separate tokens.
    --     (In other words, the whitespace separating them is optional.) Thus,
    --     -o foo and -ofoo are equivalent. "
    function Is_Short_Option_Or_Group (Str : String) return Boolean is
    begin
        return Str'Length >= 2 and then Str (Str'First) = '-'
            and then (for all C in (Str'First + 1) .. Str'Last =>
                                 Is_Valid_Option_Character(Str(C)) and then Str(C) /= '-');
    end Is_Short_Option_Or_Group;

    -- Whether somethign is a command or an operand depends on the parser.
    function Is_Command_Or_Operand (Str : String) return Boolean is
    begin
        return Str(Str'First) /= '-';
    end Is_Command_Or_Operand;

end Trendy_Command_Line.Context_Free;
