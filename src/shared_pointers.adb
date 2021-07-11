with System;
with System.Storage_Elements;
with Ada.Unchecked_Conversion;

package body Shared_Pointers is
    type Address_Location is new System.Storage_Elements.Integer_Address;

    type Hex_Chars_Array is array (Address_Location range 0 .. 15) of Character;
    Hex_Chars : constant Hex_Chars_Array := ('0', '1', '2', '3', '4', '5', '6',
                                            '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F');

    function As_Hex (Loc : Address_Location) return String is
        Full_Length   : constant Positive := Address_Location'Size / 8;
        Current       : Address_Location := Loc;
        Next_Char     : Address_Location'Base;
    begin
        return Str : String (1 .. Full_Length) do
            for X in 0 .. Full_Length - 1 loop
                Next_Char := Current mod 16;
                Current := Current / 16;
                Str (Full_Length - X) := Hex_Chars (Next_Char);
            end loop;
        end return;
    end As_Hex;


    function Is_Valid (Self : in Single_Shared_Pointer) return Boolean is (Self.Target /= null);

    function Ref_Count (Self : Single_Shared_Pointer) return Natural is
    begin
        return (if Self.Target = null then 0 else Self.Target.Count);
    end Ref_Count;

    procedure Reset (Self : in out Single_Shared_Pointer) is
    begin
        if Self.Target /= null then
            Self.Target.Count := Self.Target.Count - 1;

            if Self.Target.Count = 0 then
                Free (Self.Target.Element);
                Free (Self.Target);
            end if;
            Self.Target := null;
        end if;
    end Reset;

    pragma Assert (System.Address'Size = 64);
    pragma Assert (Address_Location'Size = 64);

    function Convert is new Ada.Unchecked_Conversion(System.Address, Address_Location);

    function Image (Self : in Single_Shared_Pointer) return String is
        Control_Block_Address : constant Address_Location := Convert(Self.Target'Address);
        Element_Address : constant Address_Location
            := (if Self.Target = null then 0 else Convert(Self.Target.Element'Address));
        Refs : constant Natural := (if Self.Target = null then 0 else Self.Target.Count);
    begin
        return Control_Block_Address'Image & " " & As_Hex(Control_Block_Address)
            & " " & Element_Address'Image & " " & " Refs: " & Refs'Image;
    end Image;

    function Get (Self : in Single_Shared_Pointer) return Reference_Type is
    begin
        return Reference_Type' (Element => Self.Target.Element);
    end Get;

    function Make (Params : Parameters) return Single_Shared_Pointer is
    begin
        return Pointer : Single_Shared_Pointer do
            Pointer.Target := new Control_Block' (Element => Allocate (Params),
                                                  Count   => 1);
        end return;
    end Make;

    function Make (Acc : not null T_Access) return Single_Shared_Pointer is
    begin
        return Pointer : Single_Shared_Pointer do
            Pointer.Target := new Control_Block' (Element => Acc,
                                                  Count   => 1);
        end return;
    end Make;

    overriding
    procedure Adjust (Self : in out Single_Shared_Pointer) is
    begin
        if Self.Target /= null then
            Self.Target.Count := Self.Target.Count + 1;
        end if;
    end Adjust;

    overriding
    procedure Finalize (Self : in out Single_Shared_Pointer) is
    begin
        Self.Reset;
    end Finalize;

    function Is_Valid (B : Control_Block) return Boolean is
    begin
        return B.Element /= null;
    end Is_Valid;

end Shared_Pointers;
