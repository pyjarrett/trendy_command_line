with Shared_Pointers;
with Ada.Unchecked_Deallocation;

package body Shared_Pointers_Tests is

    overriding
    procedure Initialize (Self : in out Oracle) is
    begin
        Self.Reporter.Initializes := Self.Reporter.Initializes + 1;
    end Initialize;

    overriding
    procedure Adjust (Self : in out Oracle) is
    begin
        Self.Reporter.Adjusts := Self.Reporter.Adjusts + 1;
    end Adjust;

    overriding
    procedure Finalize (Self : in out Oracle) is
    begin
        Self.Reporter.Finalizes := Self.Reporter.Finalizes + 1;
    end Finalize;

    function Allocate (Params : in Oracle_Parameters) return Oracle_Access is
    begin
        return new Oracle (Params.Reporter);
    end Allocate;

    procedure Free is new Ada.Unchecked_Deallocation (Oracle, Oracle_Access);

    package Oracle_Pointers is new Shared_Pointers(T          => Oracle,
                                                   T_Access   => Oracle_Access,
                                                   Parameters => Oracle_Parameters,
                                                   Allocate   => Allocate,
                                                   Free       => Free);

    procedure Test_Oracle (T : in out Trendy_Test.Test'Class) is
        Reporter : Oracle_Reporter_Access := new Oracle_Reporter;
        procedure Free is
            new Ada.Unchecked_Deallocation (Oracle_Reporter, Oracle_Reporter_Access);
    begin
        T.Register ("Test_Oracle");

        declare
            Ptr : Oracle_Pointers.Single_Shared_Pointer := Oracle_Pointers.Make ((Reporter => Reporter));
        begin
            T.Require (Ptr.Ref_Count = 1);
            Ptr.Reset;
            T.Require (Reporter.Initializes = 1);
            T.Require (Reporter.Adjusts = 0);
            T.Require (Reporter.Finalizes = 1);

            T.Require (Oracle_Pointers.Ref_Count(Ptr) = 0);
        end;

        T.Require (Reporter.Initializes = 1);
        T.Require (Reporter.Adjusts = 0);
        T.Require (Reporter.Finalizes = 1);

        Free (Reporter);
    end Test_Oracle;

    procedure Test_Empty (T : in out Trendy_Test.Test'Class) is
    begin
        null;
    end Test_Empty;


    function All_Tests return Trendy_Test.Test_Group is
    begin
        return (Test_Oracle'Access,
                Test_Empty'Access);
    end All_Tests;

end Shared_Pointers_Tests;
