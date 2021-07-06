package body Shared_Pointers is

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
        end if;
    end Reset;

    function Make (Params : Parameters) return Single_Shared_Pointer is
    begin
        return Pointer : Single_Shared_Pointer do
            Pointer.Target := new Control_Block' (Element => Allocate (Params),
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
        Reset(Self);
    end Finalize;

end Shared_Pointers;
