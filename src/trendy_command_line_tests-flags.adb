with Trendy_Command_Line; use Trendy_Command_Line;
with Trendy_Command_Line.Options; use Trendy_Command_Line.Options;
with Trendy_Command_Line.Parsers;

with Ada.Exceptions;

package body Trendy_Command_Line_Tests.Flags is
    function "+"(Str : String) return ASU.Unbounded_String renames ASU.To_Unbounded_String;

    type Sample_Options is (Verbose, Skip_Errors);
    type Sample_Operands is (None);
    pragma Unreferenced (None);
    package Sample_Parsers is new Trendy_Command_Line.Parsers(Sample_Options, Sample_Operands);
    use Sample_Parsers;

    ---------------------------------------------------------------------------
    -- Testing
    ---------------------------------------------------------------------------

    procedure Test_Boolean_Option_Defaults (T : in out Trendy_Test.Operation'Class) is
        P : Parser;
        Args : Parsed_Arguments;
        Empty : String_Vectors.Vector;
    begin
        T.Register;
        P.Add_Option(Verbose, "-v", "--verbose", "Print more information when running", True_When_Set);
        P.Add_Option(Skip_Errors, "-e", "--skip-errors", "Skip errors when running", False_When_Set);
        P.No_Operands;

        Args := P.Parse (Empty);
        T.Assert (not Get_Boolean(Args, Verbose));
        T.Assert (Get_Boolean(Args, Skip_Errors));
    end Test_Boolean_Option_Defaults;

    procedure Test_Boolean_Option_Toggles (T : in out Trendy_Test.Operation'Class) is
        P       : Parser;
        Args    : Parsed_Arguments;
        Toggles : String_Vectors.Vector;
    begin
        T.Register;

        P.Add_Option(Verbose, "-v", "--verbose", "Print more information when running", True_When_Set);
        P.Add_Option(Skip_Errors, "-e", "--skip-errors", "Print more information when running", False_When_Set);
        P.No_Operands;

        Toggles.Append(+"--verbose");
        Toggles.Append(+"--skip-errors");
        Args := P.Parse (Toggles);
        T.Assert (Get_Boolean(Args, Verbose));
        T.Assert (not Get_Boolean(Args, Skip_Errors));
    end Test_Boolean_Option_Toggles;

    procedure Test_Boolean_Option_Too_Many_Occurrences (T : in out Trendy_Test.Operation'Class) is
        P       : Parser;
        Args    : Parsed_Arguments;
        Toggles : String_Vectors.Vector;
    begin
        T.Register;

        P.Add_Option(Verbose, "-v", "--verbose", "Print more information when running", True_When_Set);
        P.Add_Option(Skip_Errors, "-e", "--skip-errors", "Print more information when running", False_When_Set);
        P.No_Operands;

        Toggles.Append(+"--verbose");
        Toggles.Append(+"--skip-errors");
        Toggles.Append(+"--skip-errors");
        Args := P.Parse (Toggles);
        pragma Unreferenced (Args);
        T.Fail ("No exception raised.");
    exception
        when Trendy_Test.Test_Registered => null;
        when Trendy_Test.Test_Disabled => null;
        when Too_Many_Occurrences => null;
        when Error : others =>
            T.Fail ("Unexpected exception: " & Ada.Exceptions.Exception_Name (Error)
                    & " " & Ada.Exceptions.Exception_Information (Error));
    end Test_Boolean_Option_Too_Many_Occurrences;

    procedure Test_Boolean_Option_Short_Options (T : in out Trendy_Test.Operation'Class) is
        P       : Parser;
        Args    : Parsed_Arguments;
        Toggles : String_Vectors.Vector;
    begin
        T.Register;

        P.Add_Option(Verbose, "-v", "--verbose", "Print more information when running", True_When_Set);
        P.Add_Option(Skip_Errors, "-e", "--skip-errors", "Print more information when running", False_When_Set);
        P.No_Operands;

        Toggles.Append(+"-v");
        Toggles.Append(+"-e");
        Args := P.Parse (Toggles);
        T.Assert (Get_Boolean(Args, Verbose));
        T.Assert (not Get_Boolean(Args, Skip_Errors));
    end Test_Boolean_Option_Short_Options;

    procedure Test_Boolean_Option_Short_Option_Group (T : in out Trendy_Test.Operation'Class) is
        P       : Parser;
        Args    : Parsed_Arguments;
        Toggles : String_Vectors.Vector;
    begin
        T.Register;

        P.Add_Option(Verbose, "-v", "--verbose", "Print more information when running", True_When_Set);
        P.Add_Option(Skip_Errors, "-e", "--skip-errors", "Print more information when running", False_When_Set);
        P.No_Operands;

        Toggles.Append(+"-ve");
        Args := P.Parse (Toggles);
        T.Assert (Get_Boolean(Args, Verbose));
        T.Assert (not Get_Boolean(Args, Skip_Errors));
    end Test_Boolean_Option_Short_Option_Group;

    ---------------------------------------------------------------------------
    -- Registry
    ---------------------------------------------------------------------------

    function All_Tests return Trendy_Test.Test_Group is
    begin
        return
            (Test_Boolean_Option_Defaults'Access,
             Test_Boolean_Option_Toggles'Access,
             Test_Boolean_Option_Too_Many_Occurrences'Access,
             Test_Boolean_Option_Short_Options'Access,
             Test_Boolean_Option_Short_Option_Group'Access
            );
    end All_Tests;

end Trendy_Command_Line_Tests.Flags;
