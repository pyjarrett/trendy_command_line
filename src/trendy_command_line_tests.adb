with Trendy_Command_Line.Options; use Trendy_Command_Line.Options;
with Trendy_Command_Line.Parsers;
use Trendy_Command_Line;

package body Trendy_Command_Line_Tests is

    function "+"(Str : String) return ASU.Unbounded_String renames ASU.To_Unbounded_String;

    procedure Test_Short_Option_With_Argument (T : in out Trendy_Test.Operation'Class) is
        type Option_Name is (Output_File);
        type Operand_Name is (None);
        pragma Unreferenced (None);

        package Parsers is new Trendy_Command_Line.Parsers(Option_Name => Option_Name, Operand_Name => Operand_Name);
        P       : Parsers.Parser;
        Args    : Parsers.Parsed_Arguments;
        Input   : String_Vectors.Vector;
    begin
        T.Register;

        P.Add_Option(Output_File, "-o", "", "Output file location", Trendy_Command_Line.Options.Store_String);
        P.No_Operands;

        Input.Append(+"-osample.out");
        Args := P.Parse (Input);
        T.Assert (Parsers.Get_String (Args, Output_File) = "sample.out");
    end Test_Short_Option_With_Argument;

    procedure Test_Long_Option_With_Argument (T : in out Trendy_Test.Operation'Class) is
        type Option_Name is (Output_File);
        type Operand_Name is (None);
        pragma Unreferenced (None);

        package Parsers is new Trendy_Command_Line.Parsers(Option_Name => Option_Name, Operand_Name => Operand_Name);
        P       : Parsers.Parser;
        Args    : Parsers.Parsed_Arguments;
        Input   : String_Vectors.Vector;
    begin
        T.Register(Parallelize => False);

        P.Add_Option(Output_File, "", "--output", "Output file location", Trendy_Command_Line.Options.Store_String);
        P.No_Operands;

        Input.Append(+"--output");
        Input.Append(+"sample.out");
        Args := P.Parse (Input);
        T.Assert (Parsers.Get_String (Args, Output_File) = "sample.out");
    end Test_Long_Option_With_Argument;

    procedure Test_One_Operand (T : in out Trendy_Test.Operation'Class) is
        type Option_Name is (None);
        type Operand_Name is (Path);
        pragma Unreferenced (None);

        package Parsers is new Trendy_Command_Line.Parsers(Option_Name => Option_Name, Operand_Name => Operand_Name);
        P       : Parsers.Parser;
        Args    : Parsers.Parsed_Arguments;
        Input   : String_Vectors.Vector;
    begin
        T.Register(Parallelize => False);

        P.Add_Operand(Path, Parsers.One, "A path to examine");
        P.No_Options;

        Input.Append(+"sample.out");
        Args := P.Parse (Input);
        T.Assert (Parsers.Get_String (Args, Path) = "sample.out");
    end Test_One_Operand;

    ---------------------------------------------------------------------------
    -- Registry
    ---------------------------------------------------------------------------

    function All_Tests return Trendy_Test.Test_Group is
    begin
        return
            (Test_Short_Option_With_Argument'Access,
             Test_Long_Option_With_Argument'Access,
             Test_One_Operand'Access
            );
    end All_Tests;

end Trendy_Command_Line_Tests;
