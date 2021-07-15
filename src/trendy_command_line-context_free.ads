package Trendy_Command_Line.Context_Free is

    function Is_Long_Option (Str : String) return Boolean;
    function Is_Short_Option (Str : String) return Boolean;
    function Is_Short_Option_Or_Group (Str : String) return Boolean;
    function Is_Command_Or_Operand (Str : String) return Boolean;
    function Is_Option_Terminator (Str : String) return Boolean;

    type Parser_Token_Kind is (Command_Or_Operand,
                               Short_Option_Or_Group,
                               Long_Option,
                               Option_Terminator);

    function General_Token_Kind (Str : String) return Parser_Token_Kind;

end Trendy_Command_Line.Context_Free;
