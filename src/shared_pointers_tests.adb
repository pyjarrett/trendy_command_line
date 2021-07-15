with Shared_Pointers;
with Ada.Unchecked_Deallocation;
with Trendy_Test.Assertions; use Trendy_Test.Assertions;

package body Shared_Pointers_Tests is
    procedure Free is
        new Ada.Unchecked_Deallocation (Oracle_Reporter, Oracle_Reporter_Access);

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

    --------------------------------------------------------------------------
    use Trendy_Test.Assertions.Integer_Assertions;

    procedure Require_EQ is new Trendy_Test.Require_EQ (T     => Oracle_Pointers.Single_Shared_Pointer,
                                                        Image => Oracle_Pointers.Image);

    --------------------------------------------------------------------------

    procedure Test_Single_Oracle (T : in out Trendy_Test.Operation'Class) is
        Reporter : Oracle_Reporter_Access := new Oracle_Reporter;
    begin
        T.Register;

        declare
            Ptr : Oracle_Pointers.Single_Shared_Pointer := Oracle_Pointers.Make ((Reporter => Reporter));
        begin
            Require_EQ (T, Ptr.Ref_Count, 1);
            Ptr.Reset;
            Require_EQ (T, Reporter.Initializes, 1);
            Require_EQ (T, Reporter.Adjusts, 0);
            Require_EQ (T, Reporter.Finalizes, 1);

            Require_EQ (T, Oracle_Pointers.Ref_Count(Ptr), 0);
        end;

        Require_EQ (T, Reporter.Initializes, 1);
        Require_EQ (T, Reporter.Adjusts, 0);
        Require_EQ (T, Reporter.Finalizes, 1);

        Free (Reporter);
    end Test_Single_Oracle;


    procedure Test_Multiple_Oracles (T : in out Trendy_Test.Operation'Class) is
        Reporter : Oracle_Reporter_Access := new Oracle_Reporter;
    begin
        T.Register;

        declare
            Ptr : Oracle_Pointers.Single_Shared_Pointer := Oracle_Pointers.Make ((Reporter => Reporter));
            Ptr2 : Oracle_Pointers.Single_Shared_Pointer := Ptr;
            Ptr3 : Oracle_Pointers.Single_Shared_Pointer;
        begin
            Require_EQ (T, Ptr.Ref_Count, 2);
            Require_EQ (T, Ptr2.Ref_Count, 2);
            Require_EQ (T, Ptr3.Ref_Count, 0);

            Require_EQ (T, Ptr, Ptr2);

            Ptr.Reset;
            Require_EQ (T, Reporter.Initializes, 1);
            Require_EQ (T, Reporter.Adjusts, 0);
            Require_EQ (T, Reporter.Finalizes, 0);

            Ptr2.Reset;
            Require_EQ (T, Reporter.Initializes, 1);
            Require_EQ (T, Reporter.Adjusts, 0);
            Require_EQ (T, Reporter.Finalizes, 1);
            Require_EQ (T, Oracle_Pointers.Ref_Count(Ptr), 0);
            Require_EQ (T, Oracle_Pointers.Ref_Count(Ptr2), 0);
            -- Previous object was destroyed.

            -- Make a new object, we should see and initialize/finalize aftwards.
            Ptr3 := Oracle_Pointers.Make((Reporter => Reporter));
            Require_EQ (T, Ptr3.Ref_Count, 1);
            Ptr3.Reset;
            Require_EQ (T, Ptr3.Ref_Count, 0);

            Require_EQ (T, Ptr2, Ptr3);
            Require_EQ (T, Ptr2, Ptr2);
        end;

        Require_EQ (T, Reporter.Initializes, 2);
        Require_EQ (T, Reporter.Adjusts, 0);
        Require_EQ (T, Reporter.Finalizes, 2);

        Free (Reporter);
    end Test_Multiple_Oracles;


    function All_Tests return Trendy_Test.Test_Group is
    begin
        return (Test_Single_Oracle'Access, Test_Multiple_Oracles'Access);
    end All_Tests;

end Shared_Pointers_Tests;
