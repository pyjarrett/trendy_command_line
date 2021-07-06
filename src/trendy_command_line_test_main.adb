with Shared_Pointers_Tests;
with Trendy_Command_Line.Tests;
with Trendy_Test;

with Ada.Text_IO;

procedure Trendy_Command_Line_Test_Main is

    package AIO renames Ada.Text_IO;

begin

    Trendy_Test.Register (Trendy_Command_Line.Tests.All_Tests);
    Trendy_Test.Register (Shared_Pointers_Tests.All_Tests);

    case Trendy_Test.Run is
        when Trendy_Test.Passed =>
            AIO.Put_Line ("Passed");
        when Trendy_Test.Failed =>
            AIO.Put_Line ("Failed");
    end case;
end Trendy_Command_Line_Test_Main;
