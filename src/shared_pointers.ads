with Ada.Finalization;
with Ada.Unchecked_Deallocation;

generic
    type T (<>) is limited private;
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
    --
    -- TODO: Add implicit dereference.

    type Single_Shared_Pointer is tagged private;

    type Reference_Type (Element : not null access T) is null record
        with Implicit_Dereference => Element;

    function Is_Valid (Self : in Single_Shared_Pointer) return Boolean;
    -- Verifies that the pointer points to something.

    function Make (Params : in Parameters) return Single_Shared_Pointer
        with Post => Make'Result.Is_Valid;

    function Make (Acc : not null T_Access) return Single_Shared_Pointer
        with Post => Make'Result.Is_Valid;

    function Ref_Count (Self : in Single_Shared_Pointer) return Natural;

    procedure Reset (Self : in out Single_Shared_Pointer)
        with Post => not Self.Is_Valid;

    function Image (Self : in Single_Shared_Pointer) return String;

    function Get (Self : in Single_Shared_Pointer) return Reference_Type
        with Pre => Self.Is_Valid;

    type Control_Block is private;
    -- Declared here to allow a type invariant.

    overriding
    function "="(Left, Right : Single_Shared_Pointer) return Boolean;

private

    -- Information about the target to free and how many outstanding references
    -- there are regarding it.
    --
    -- When count goes to zero, the access element should be deleted.
    -- Note that the control block is iteself dynamically allocated since there's
    -- no central place where it can be stack allocated.
    --
    -- It'd be better to allocate the control block and the element at the same
    -- time in one call, but this is acceptable for right now.
    type Control_Block is record
        Element : T_Access := null;
        Count : Natural := 1;
    end record
        with Type_Invariant => Is_Valid (Control_Block);

    function Is_Valid (B : Control_Block) return Boolean;
    -- A valid control block contains allocated memory of the target type.

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
