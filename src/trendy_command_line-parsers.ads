with Trendy_Command_Line.Options; use Trendy_Command_Line.Options;
with Trendy_Command_Line.Context_Free;

generic
    -- Rather than identify options with strings, use a list of unique values
    -- for a type.  Hopefully this will reduce errors.
    type Option_Name is (<>);

    type Operand_Name is (<>);

package Trendy_Command_Line.Parsers is

    ---------------------------------------------------------------------------
    -- Parser
    ---------------------------------------------------------------------------

    -- A parser is a set of all of the commands, options and operands that can
    -- be extracted from a command line.  Clients don't care how it works, so
    -- long as it simplifies the creation of a CLI tool.
    --
    -- This is tagged to allow "dot-notation" to simplify its usage.
    type Parser is tagged limited private;

    -- An opaque handle provided to clients to get the values of an argument
    -- parse.
    --
    -- These really shouldn't be copied around since that might be expensive,
    -- though it isn't necessarily incorrect to do so.
    type Parsed_Arguments is private;

    -- Parses the current command line.
    function Parse (P : aliased in Parser) return Parsed_Arguments;

    -- Called to parse arguments using a given parser out of an array of command line arguments.
    function Parse (P : aliased in Parser; Args : in String_Vectors.Vector) return Parsed_Arguments;

    ---------------------------------------------------------------------------
    -- Parsed Arguments (Parsing Results)
    ---------------------------------------------------------------------------
    function Get_Boolean(P : Parsed_Arguments; Name : Option_Name) return Boolean;

    function Get_String (P : Parsed_Arguments; Name : Option_Name) return String;

    function Get_String (P : Parsed_Arguments; Name : Operand_Name) return String;

    ---------------------------------------------------------------------------
    -- Options
    ---------------------------------------------------------------------------

    function Has_Format_For_Option (P : Parser; Name : Option_Name) return Boolean;

    procedure Add_Option (P            : in out Parser;
                          Name         : Option_Name;
                          Short_Option : String := "";
                          Long_Option  : String := "";
                          Help         : String;
                          Action       : Option_Action := True_When_Set
                         )
        with
            Pre => not Has_Format_For_Option (P, Name)
                   and then (Context_Free.Is_Long_Option (Long_Option)
                                or else Context_Free.Is_Short_Option(Short_Option)),
            Post => Has_Format_For_Option (P, Name);

    -- The type backing an option.  An option informs the program about future
    -- program state, so state must be stored to do this.
    type Option_Kind is (Boolean_Option, Integer_Option, String_Option, Operands_Option);

    function Kind (P : Parser; Name : Option_Name) return Option_Kind
        with
            Pre => Has_Format_For_Option (P, Name);

    -- Flags have zero arguments.
    function Is_Flag (P : Parser; Name : Option_Name) return Boolean
        with Pre => Has_Format_For_Option (P, Name);

    Wrong_Option_Type    : exception;
    Unimplemented        : exception;
    Too_Many_Occurrences : exception;
    No_Value             : exception;
    Too_Many_Arguments   : exception;
    Unfulfilled_Option   : exception;
    Unfulfilled_Operand  : exception;
    Unused_Argument      : exception;

    procedure Default (P    : in out Parser;
                       Name : Option_Name;
                       Str  : String)
        with Pre => Has_Format_For_Option (P, Name) and then Kind (P, Name) = String_Option;

    procedure No_Options (P : in out Parser);
    procedure No_Operands (P : in out Parser);

    ---------------------------------------------------------------------------
    -- Operands
    ---------------------------------------------------------------------------
    type Operand_Arity is (One, One_Or_More, Zero_Or_More);

    procedure Add_Operand (P     : in out Parser;
                           Name  : Operand_Name;
                           Arity : Operand_Arity := One;
                           Help  : String);

private

    type Argument_Status is (Unused, Added, Ignored);

    -- A description of an option to be provided to the command line.  Either
    -- short or long option must be provided.
    type Option_Format is record
        Status       : Argument_Status := Unused;
        Short_Option : ASU.Unbounded_String;
        Long_Option  : ASU.Unbounded_String;
        Help         : ASU.Unbounded_String;
        Action       : Option_Action;
    end record;

    -- Every action of an option describes a specific type of state to store underneath.
    Action_To_Kind : constant array (Option_Action) of Option_Kind :=
                       (True_When_Set  => Boolean_Option,
                        False_When_Set => Boolean_Option,
                        Store_Int      => Integer_Option,
                        Store_String   => String_Option,
                        Store_Operands => Operands_Option);

    -- Backing values stored for options.
    --
    -- This just uses a naive implementation instead of doing a variant with a
    -- compressed footprint.
    type Option_Value is record
        -- Repeating options is usually an error, though not necessarily so.
        -- Tracking the number of occurrences provides the program with the
        -- means to do so something about it.
        Occurrences   : Natural := 0;
        Kind          : Option_Kind;
        Boolean_Value : Boolean;
        Operands      : String_Vectors.Vector;
    end record;

    type Option_Formats is array (Option_Name) of Option_Format;
    type Option_Values is array (Option_Name) of Option_Value;

    type Operand_Format is record
        Status : Argument_Status := Unused;
        Arity : Operand_Arity;
        Help  : ASU.Unbounded_String;
    end record;

    type Operand_Format_Array is array (Operand_Name) of Operand_Format;
    type Operand_Values is array (Operand_Name) of String_Vectors.Vector;

    type Parser is tagged limited record
        Defaults        : Option_Values;
        Formats         : Option_Formats;
        Operand_Formats : Operand_Format_Array;
    end record;

    type Parsed_Arguments is record
        Values   : Option_Values;
        Operands : Operand_Values;
    end record;

    type Parse_State (Current_Parser : access constant Parser; Result : access Parsed_Arguments) is record
        -- All arguments after the option terminator ("--") are considered operands.
        Option_Terminator_Reached           : Boolean := False;

        Has_More_Operands                   : Boolean := True;
        Current_Operand                     : Operand_Name := Operand_Name'First;
        Has_Last_Option                     : Boolean := False;

        -- Only valid if Has_Last_Option is true.
        Arguments_Processed_For_Last_Option : Natural := 0;
        Last_Option                         : Option_Name;
        Last_Option_Arguments               : String_Vectors.Vector;

        -- The list of arguments being parsed.  When the parsing is complete without,
        -- errors, this list will be empty.
        Unprocessed_Arguments               : String_Vectors.Vector;
    end record;

    -- Does the last option have enough options to be considered "complete"?
    function Is_Last_Option_Fulfilled (P : in Parser; State : in Parse_State) return Boolean
        with Pre => State.Has_Last_Option;

    procedure Start_Option (P : in out Parse_State; Name : Option_Name);
        -- with Pre => Min_Num_Arguments (Name) > 0

    procedure Clear_Option (P : in Parser; State : in out Parse_State)
        with Pre => not State.Has_Last_Option or else Is_Last_Option_Fulfilled (P, State);

    procedure Process_Operand (P : in out Parse_State; Operand : in ASU.Unbounded_String);

end Trendy_Command_Line.Parsers;
