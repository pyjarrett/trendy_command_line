with Ada.Finalization;
with Trendy_Test;

package Shared_Pointers_Tests is

    type Oracle_Reporter is record
        Initializes : Natural := 0;
        Adjusts : Natural := 0;
        Finalizes : Natural := 0;
    end record;

    type Oracle_Reporter_Access is access all Oracle_Reporter;

    type Oracle (Reporter : Oracle_Reporter_Access) is new Ada.Finalization.Controlled with null record;
    type Oracle_Access is access Oracle;

    overriding
    procedure Initialize (Self : in out Oracle);

    overriding
    procedure Adjust (Self : in out Oracle);

    overriding
    procedure Finalize (Self : in out Oracle);

    type Oracle_Parameters(Reporter : Oracle_Reporter_Access) is null record;

    function All_Tests return Trendy_Test.Test_Group;

end Shared_Pointers_Tests;
