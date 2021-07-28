with Trendy_Command_Line.Options; use Trendy_Command_Line.Options;

generic
    -- Rather than identify options with strings, use a list of unique values
    -- for a type.  Hopefully this will reduce errors.
    type Option_Name is (<>);

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
    function Get_Boolean(P : in Parsed_Arguments; Name : Option_Name) return Boolean;

    function Get_String (P : Parsed_Arguments; Name : Option_Name) return String;

    ---------------------------------------------------------------------------
    -- Options
    ---------------------------------------------------------------------------
    procedure Add_Option (P            : in out Parser;
                          Name         : Option_Name;
                          Short_Option : String := "";
                          Long_Option  : String := "";
                          Help         : String;
                          Action       : Option_Action := True_When_Set
                         );

    Wrong_Option_Type    : exception;
    Unimplemented        : exception;
    Too_Many_Occurrences : exception;
    No_Value             : exception;

    ---------------------------------------------------------------------------
    -- Operands
    ---------------------------------------------------------------------------
    --  procedure Add_Operand (

private

    -- A description of an option to be provided to the command line.  Either
    -- short or long option must be provided.
    --
    -- TODO: A validator of operands might be useful.
    -- Validator : access function (Str : String) return Boolean;
    type Option_Format is record
        Short_Option : ASU.Unbounded_String;
        Long_Option  : ASU.Unbounded_String;
        Help         : ASU.Unbounded_String;
        Action       : Option_Action;
    end record;

    -- The type backing an option.  An option informs the program about future
    -- program state, so state must be stored to do this.
    type Option_Kind is (Boolean_Option, Integer_Option, String_Option, Operands_Option);

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
        --
        -- TODO: Add description of the accumulation method for options for which
        --       multiple occurrences if permitted.
        Occurrences   : Natural := 0;
        Kind          : Option_Kind;
        Boolean_Value : Boolean;
        Operands      : String_Vectors.Vector;
    end record;

    type Option_Formats is array (Option_Name) of Option_Format;
    type Option_Values is array (Option_Name) of Option_Value;

    -- TODO: Need to add sub-parsers.  Ignoring this for now to get parsers stood up.
    type Parser is tagged limited record
        Formats  : Option_Formats;
        Defaults : Option_Values;
    end record;

    type Parsed_Arguments is record
        Values : Option_Values;

        -- TODO: Add storage for operands.
    end record;

    type Parse_State (Current_Parser : access constant Parser; Result : access Parsed_Arguments) is record
        -- All arguments after the option terminator ("--") are considered operands.
        Option_Terminator_Reached           : Boolean := False;

        Has_Last_Option                     : Boolean := False;

        -- Only valid if Has_Last_Option is true.
        Arguments_Processed_For_Last_Option : Natural := 0;
        Last_Option                         : Option_Name;
        Last_Option_Arguments               : String_Vectors.Vector;

        -- The list of arguments being parsed.  When the parsing is complete without,
        -- errors, this list will be empty.
        Unprocessed_Arguments               : String_Vectors.Vector;
    end record;

    procedure Start_Option (P : in out Parse_State; Name : Option_Name);
        -- with Pre => Min_Num_Arguments (Name) > 0

    procedure Clear_Option (P : in out Parse_State);

    procedure Process_Operand (P : in out Parse_State; Operand : in ASU.Unbounded_String);

end Trendy_Command_Line.Parsers;
