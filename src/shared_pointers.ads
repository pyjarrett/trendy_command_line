with Ada.Finalization;
with Ada.Unchecked_Deallocation;

generic
    type T (<>) is abstract tagged limited private;
    type T_Access is access T;
    type Parameters (<>) is limited private;
    with function Allocate (Params : in Parameters) return T_Access;
    with procedure Free (Acc : in out T_Access);

package Shared_Pointers is
    -- THIS IS NOT THREAD/TASK SAFE.
    -- A thread-UNsafe reference counted pointer.  THIS IS NOT THREAD/TASK SAFE.
    --
    -- A very simple solution to a simple problem.  "I don't want to have to remember to free a block
    -- of memory, I just want it done when it needs to be done."
    --
    -- THIS IS NOT THREAD/TASK SAFE.
    -- Did I mention that THIS IS NOT THREAD/TASK SAFE?

    type Single_Shared_Pointer is tagged private;

    function Make (Params : in Parameters) return Single_Shared_Pointer;

    function Ref_Count (Self : in Single_Shared_Pointer) return Natural;

    procedure Reset (Self : in out Single_Shared_Pointer);

private

    -- Information about the target to free and how many outstanding references
    -- there are regarding it.
    --
    -- When count goes to zero, the access element should be deleted.
    -- Note that the control block is iteself dynamically allocated since there's
    -- no central place where it can be stack allocated.
    type Control_Block is record
        Element : T_Access;
        Count : Natural := 1;
    end record;

    type Control_Block_Access is access Control_Block;

    procedure Free is
        new Ada.Unchecked_Deallocation (Control_Block, Control_Block_Access);

    type Single_Shared_Pointer is new Ada.Finalization.Controlled with record
        Target : Control_Block_Access := null;
    end record;

    overriding
    procedure Adjust (Self : in out Single_Shared_Pointer);

    overriding
    procedure Finalize (Self : in out Single_Shared_Pointer);

end Shared_Pointers;
