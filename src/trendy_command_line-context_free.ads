package Trendy_Command_Line.Context_Free is

    function Is_Long_Option (Str : String) return Boolean;
    function Is_Short_Option (Str : String) return Boolean;
    function Is_Short_Option_Or_Group (Str : String) return Boolean;
    function Is_Command_Or_Operand (Str : String) return Boolean;

end Trendy_Command_Line.Context_Free;
