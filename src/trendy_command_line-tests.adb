with Trendy_Command_Line.Context_Free; use Trendy_Command_Line.Context_Free;

package body Trendy_Command_Line.Tests is

    --  function "+"(Str : String) return ASU.Unbounded_String renames ASU.To_Unbounded_String;

    ---------------------------------------------------------------------------
    -- Testing
    ---------------------------------------------------------------------------

    procedure Test_Is_Short_Option (T : in out Trendy_Test.Test'Class) is
    begin
        T.Register ("Is_Short_Option");
        T.Require (Is_Short_Option ("-v"));
        T.Require (not Is_Short_Option ("-"));
        T.Require (not Is_Short_Option ("--"));
        T.Require (not Is_Short_Option ("- "));
    end Test_Is_Short_Option;

    procedure Test_Is_Short_Option_Or_Group (T : in out Trendy_Test.Test'Class) is
    begin
        T.Register ("Is_Short_Option_Or_Group");
        T.Require (Is_Short_Option_Or_Group("-v"));
        T.Require (Is_Short_Option_Or_Group("-ofoo"));
        T.Require (Is_Short_Option_Or_Group("-avzx"));

        T.Require (not Is_Short_Option_Or_Group("- "));
        T.Require (not Is_Short_Option_Or_Group("--notshort"));
        T.Require (not Is_Short_Option_Or_Group("-a-b"));
    end Test_Is_Short_Option_Or_Group;

    procedure Test_Is_Long_Option (T : in out Trendy_Test.Test'Class) is
    begin
        T.Register ("Is_Long_Option");

        T.Require (Is_Long_Option ("--verbose"));
        T.Require (Is_Long_Option ("--long_option"));
        T.Require (Is_Long_Option ("--ignore-dot-files"));
        T.Require (not Is_Long_Option ("-s"));
        T.Require (not Is_Long_Option ("--"));
        T.Require (not Is_Long_Option ("--has and space"));
    end Test_Is_Long_Option;

    procedure Test_Boolean_Option (T : in out Trendy_Test.Test'Class) is
        P : Parser;
        --  Args : Parsed_Arguments;

        --  Empty : String_Vectors.Vector;
        --  Verbose : String_Vectors.Vector;
        --  VerboseAndSkipErrors : String_Vectors.Vector;
    begin
        T.Register ("Boolean Option");
        P.Add_Option("verbose", "-v", "--verbose", "Print more information when running", True_When_Set);
        P.Add_Option("skip_errors", "-e", "--skip-errors", "Print more information when running", False_When_Set);

        --  Args := P.Parse(Empty);
        --  T.Require(not Boolean_Value_Of(Args, "verbose"));
        --  T.Require(Boolean_Value_Of(Args, "skip_errors"));

        --  Verbose.Append(ASU.To_Unbounded_String("--verbose"));
        --  Args := P.Parse(Verbose);
        --  T.Require(Boolean_Value_Of(Args, "verbose"));
        --  T.Require(Boolean_Value_Of(Args, "skip_errors"));

        --  VerboseAndSkipErrors.Append(ASU.To_Unbounded_String("--verbose"));
        --  VerboseAndSkipErrors.Append(ASU.To_Unbounded_String("--skip-errors"));
        --  Args := P.Parse(VerboseAndSkipErrors);
        --  T.Require(Boolean_Value_Of(Args, "verbose"));
        --  T.Require(not Boolean_Value_Of(Args, "skip_errors"));
    end Test_Boolean_Option;

    procedure Test_Boolean_Option_Defaults (T : in out Trendy_Test.Test'Class) is
        P : Parser;
        Args : Parsed_Arguments;
        Empty : String_Vectors.Vector;
    begin
        T.Register ("Boolean Option Defaults");
        P.Add_Option("verbose", "-v", "--verbose", "Print more information when running", True_When_Set);
        P.Add_Option("skip_errors", "-e", "--skip-errors", "Print more information when running", False_When_Set);

        Args := P.Parse (Empty);
        T.Require (not Boolean_Value_Of(Args, "verbose"));
        T.Require (Boolean_Value_Of(Args, "skip_errors"));
    end Test_Boolean_Option_Defaults;

    --  procedure Test_Boolean_Option_Toggles (T : in out Trendy_Test.Test'Class) is
    --      P : Parser;
    --      Args : Parsed_Arguments;
    --      Toggles : String_Vectors.Vector;
    --  begin
    --      T.Register (Name => "Boolean Option Toggles",
    --                 Disabled => True);
    --
    --      P.Add_Option("verbose", "-v", "--verbose", "Print more information when running", True_When_Set);
    --      P.Add_Option("skip_errors", "-e", "--skip-errors", "Print more information when running", False_When_Set);
    --
    --      Toggles.Append(+"--verbose");
    --      Toggles.Append(+"--skip-errors");
    --      Args := P.Parse (Toggles);
    --      T.Require (Boolean_Value_Of(Args, "verbose"));
    --      T.Require (not Boolean_Value_Of(Args, "skip_errors"));
    --  end Test_Boolean_Option_Toggles;

    ---------------------------------------------------------------------------
    -- Registry
    ---------------------------------------------------------------------------

    function All_Tests return Trendy_Test.Test_Group is
    begin
        return
            (Test_Is_Short_Option'Access,
             Test_Is_Long_Option'Access,
             Test_Boolean_Option'Access,
             Test_Is_Short_Option_Or_Group'Access,
             Test_Boolean_Option_Defaults'Access
             --  Test_Boolean_Option_Defaults_Boolean_Option_Toggles'Access
            );
    end All_Tests;

end Trendy_Command_Line.Tests;
