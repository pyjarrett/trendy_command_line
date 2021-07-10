with Ada.Unchecked_Deallocation;

with Shared_Pointers;
with Trendy_Command_Line.Options; use Trendy_Command_Line.Options;

generic
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

    -- Called to parse arguments using a given parser out of an array of command line arguments.
    function Parse (P : aliased in out Parser; Args : in String_Vectors.Vector) return Parsed_Arguments;

    function Get_Boolean(P : in Parsed_Arguments; Name : Option_Name) return Boolean;


    ---------------------------------------------------------------------------
    -- Options
    ---------------------------------------------------------------------------

    procedure Add_Option (P            : in out Parser;
                          Name         : Option_Name;
                          Short_Option : String := "";
                          Long_Option  : String := "";
                          Help         : String;
                          Action       : Option_Action := True_When_Set
                          --  Validator : access function(Str : String) return Boolean;
                         );

    Wrong_Option_Type : exception;

private

    -- Arguments belong to one of multiple groups, which is affected both by the
    -- type of token and also the possible parses of the current parser.
    type Argument_Kind is (Argument_Short_Option,
                           -- An option like -v or -o

                           Argument_Short_Option_Group,
                           -- Multiple options used at the same time, none of
                           -- which may have operands.

                           Argument_Long_Option,
                           -- An argument which starts with two hyphens.

                           Argument_Integer,
                           -- An integer, with a possible leading negative sign.

                           Argument_Operand,

                           Argument_End_Of_Options
                           -- A double hyphen with nothing following, which acts
                           -- as a delimeter between options and operands.
                          );

    type Option_Format is record
        Short_Option : ASU.Unbounded_String;
        Long_Option  : ASU.Unbounded_String;
        Help         : ASU.Unbounded_String;
        Action       : Option_Action;
        -- Validator : access function (Str : String) return Boolean;
    end record;

    -- The type backing an option.
    type Option_Kind is (Boolean_Option, Integer_Option, String_Option, Operands_Option);

    Action_To_Kind : constant array (Option_Action) of Option_Kind :=
        (True_When_Set => Boolean_Option,
         False_When_Set => Boolean_Option,
         Store_Int => Integer_Option,
         Store_String => String_Option,
         Store_Operands => Operands_Option);

    --
    -- Backing values stored for options.
    --
    type Option_Value is record
        Kind          : Option_Kind;
        Boolean_Value : Boolean;
        Operands      : String_Vectors.Vector;
    end record;

    type Options_Array is array (Option_Name) of Option_Format;
    type Options_Values is array (Option_Name) of Option_Value;

    -- TODO: Need to add sub-parsers.
    type Parser is tagged limited record
        Options : Options_Array;
        Defaults : Options_Values;
    end record;

    type Parser_Parameters is null record;
    type Parser_Access is access Parser;
    function Allocate (Params : Parser_Parameters) return Parser_Access;
    procedure Free is new Ada.Unchecked_Deallocation (Parser, Parser_Access);

    package Parser_Pointers is new Shared_Pointers(T          => Parser,
                                                   T_Access   => Parser_Access,
                                                   Parameters => Parser_Parameters,
                                                   Allocate   => Allocate,
                                                   Free       => Free);

    type Parsed_Arguments is record
        Values : Options_Values;
    end record;

    type Parse_State is record
        Fresh_Parser                        : Boolean := True;
        Option_Terminator_Reached           : Boolean := False;
        Arguments_Processed_For_Last_Option : Natural := 0;
        Has_Last_Option                     : Boolean := False;
        Last_Option                         : ASU.Unbounded_String := ASU.Null_Unbounded_String;

        -- The list of arguments being parsed.  When the parsing is complete without,
        -- errors, this list will be empty.
        Unprocessed_Arguments               : String_Vectors.Vector;
        -- Current_Parser;
    end record;

end Trendy_Command_Line.Parsers;
