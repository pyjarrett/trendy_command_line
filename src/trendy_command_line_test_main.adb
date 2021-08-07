with Shared_Pointers_Tests;
with Trendy_Command_Line_Tests.Flags;
with Trendy_Command_Line.Context_Free.Tests;
with Trendy_Test;
with Trendy_Test.Reports;

procedure Trendy_Command_Line_Test_Main is
begin
    Trendy_Test.Register (Trendy_Command_Line_Tests.All_Tests);
    Trendy_Test.Register (Trendy_Command_Line_Tests.Flags.All_Tests);
    Trendy_Test.Register (Trendy_Command_Line.Context_Free.Tests.All_Tests);
    Trendy_Test.Register (Shared_Pointers_Tests.All_Tests);

    Trendy_Test.Reports.Print_Basic_Report(Trendy_Test.Run);
end Trendy_Command_Line_Test_Main;
