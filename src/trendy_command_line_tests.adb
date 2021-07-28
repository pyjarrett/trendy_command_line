with Trendy_Command_Line.Context_Free; use Trendy_Command_Line.Context_Free;
with Trendy_Command_Line.Options; use Trendy_Command_Line.Options;
with Trendy_Command_Line.Parsers;
use Trendy_Command_Line;

with Ada.Exceptions;

package body Trendy_Command_Line_Tests is

    function "+"(Str : String) return ASU.Unbounded_String renames ASU.To_Unbounded_String;

    type Sample_Options is (Verbose, Skip_Errors);
    package Sample_Parsers is new Trendy_Command_Line.Parsers(Sample_Options);
    use Sample_Parsers;

    ---------------------------------------------------------------------------
    -- Testing
    ---------------------------------------------------------------------------

    procedure Test_Is_Short_Option (T : in out Trendy_Test.Operation'Class) is
    begin
        T.Register;
        T.Assert (Is_Short_Option ("-v"));
        T.Assert (not Is_Short_Option ("-"));
        T.Assert (not Is_Short_Option ("--"));
        T.Assert (not Is_Short_Option ("- "));
    end Test_Is_Short_Option;

    procedure Test_Is_Short_Option_Or_Group (T : in out Trendy_Test.Operation'Class) is
    begin
        T.Register;
        T.Assert (Is_Short_Option_Or_Group("-v"));
        T.Assert (Is_Short_Option_Or_Group("-ofoo"));
        T.Assert (Is_Short_Option_Or_Group("-avzx"));

        T.Assert (not Is_Short_Option_Or_Group("- "));
        T.Assert (not Is_Short_Option_Or_Group("--notshort"));
        T.Assert (not Is_Short_Option_Or_Group("-a-b"));
    end Test_Is_Short_Option_Or_Group;

    procedure Test_Is_Long_Option (T : in out Trendy_Test.Operation'Class) is
    begin
        T.Register;
        T.Assert (Is_Long_Option ("--verbose"));
        T.Assert (Is_Long_Option ("--long_option"));
        T.Assert (Is_Long_Option ("--ignore-dot-files"));
        T.Assert (not Is_Long_Option ("-s"));
        T.Assert (not Is_Long_Option ("--"));
        T.Assert (not Is_Long_Option ("--has and space"));
    end Test_Is_Long_Option;

    procedure Test_Is_Option_Terminator (T : in out Trendy_Test.Operation'Class) is
    begin
        T.Register;
        T.Assert (Is_Option_Terminator("--"));
        T.Assert (not Is_Option_Terminator("-"));
        T.Assert (not Is_Option_Terminator("--some-option"));
        T.Assert (not Is_Option_Terminator("---"));
    end Test_Is_Option_Terminator;

    procedure Test_Boolean_Option_Defaults (T : in out Trendy_Test.Operation'Class) is
        P : Parser;
        Args : Parsed_Arguments;
        Empty : String_Vectors.Vector;
    begin
        T.Register;
        P.Add_Option(Verbose, "-v", "--verbose", "Print more information when running", True_When_Set);
        P.Add_Option(Skip_Errors, "-e", "--skip-errors", "Skip errors when running", False_When_Set);

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

        Toggles.Append(+"-ve");
        Args := P.Parse (Toggles);
        T.Assert (Get_Boolean(Args, Verbose));
        T.Assert (not Get_Boolean(Args, Skip_Errors));
    end Test_Boolean_Option_Short_Option_Group;

    procedure Test_Option_With_Argument (T : in out Trendy_Test.Operation'Class) is
        type Option_Names is (Output_File);

        package Parsers is new Trendy_Command_Line.Parsers(Option_Name => Option_Names);
        P       : Parsers.Parser;
        Args    : Parsers.Parsed_Arguments;
        Input   : String_Vectors.Vector;
    begin
        T.Register;

        P.Add_Option(Output_File, "-o", "", "Output file location", Trendy_Command_Line.Options.Store_String);

        Input.Append(+"-o");
        Input.Append(+"sample.out");
        Args := P.Parse (Input);
        T.Assert (Parsers.Get_String (Args, Output_File) = "sample.out");
    end Test_Option_With_Argument;

    ---------------------------------------------------------------------------
    -- Registry
    ---------------------------------------------------------------------------

    function All_Tests return Trendy_Test.Test_Group is
    begin
        return
            (Test_Is_Short_Option'Access,
             Test_Is_Long_Option'Access,
             Test_Is_Option_Terminator'Access,
             Test_Is_Short_Option_Or_Group'Access,
             Test_Boolean_Option_Defaults'Access,
             Test_Boolean_Option_Toggles'Access,
             Test_Boolean_Option_Too_Many_Occurrences'Access,
             Test_Boolean_Option_Short_Options'Access,
             Test_Boolean_Option_Short_Option_Group'Access,
             Test_Option_With_Argument'Access
            );
    end All_Tests;

end Trendy_Command_Line_Tests;
