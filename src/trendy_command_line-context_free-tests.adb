package body Trendy_Command_Line.Context_Free.Tests is

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

    ---------------------------------------------------------------------------
    -- Registry
    ---------------------------------------------------------------------------

    function All_Tests return Trendy_Test.Test_Group is
    begin
        return
            (Test_Is_Short_Option'Access,
             Test_Is_Long_Option'Access,
             Test_Is_Option_Terminator'Access,
             Test_Is_Short_Option_Or_Group'Access
            );
    end All_Tests;


end Trendy_Command_Line.Context_Free.Tests;
