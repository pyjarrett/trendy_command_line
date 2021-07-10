with Ada.Containers.Ordered_Maps;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;

-- TODO: Pick a new name for this
-- Trendy_Smart_Pointers
with Shared_Pointers;

package Trendy_Command_Line is
    -- Command lines are a long list of elements to be considered singularly,
    -- in turn, to generate a space of configuration values.
    --
    -- https://www.gnu.org/software/libc/manual/html_node/Argument-Syntax.html
    --
    -- IEEE Std 1003.1-2017
    -- 12. Utility Conventions
    -- https://pubs.opengroup.org/onlinepubs/9699919799/
    --
    package ASU renames Ada.Strings.Unbounded;

    package String_Vectors is new Ada.Containers.Vectors(Index_Type   => Positive,
                                                         Element_Type => ASU.Unbounded_String,
                                                         "="          => ASU."=");

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

    function Get_Boolean(P : in Parsed_Arguments; Name : String) return Boolean;


    -- An option was found which might not exist.
    Unknown_Option : exception;

    -- The user provided some weird token that wasn't recognized.
    Unknown_Token : exception;


    ---------------------------------------------------------------------------
    -- Options
    ---------------------------------------------------------------------------

    -- Different things that options can do.
    type Option_Action is (True_When_Set,
                           False_When_Set,
                           Store_Int,
                           Store_String,
                           Store_Operands);

    procedure Add_Option (P            : in out Parser;
                          Name         : String;
                          Short_Option : String := "";
                          Long_Option  : String := "";
                          Help         : String;
                          Action       : Option_Action := True_When_Set
                          --  Validator : access function(Str : String) return Boolean;
                         );

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

    type Parser_Token_Kind is (Command_Or_Operand,
                               Short_Option_Or_Group,
                               Long_Option,
                               Option_Terminator);

    function General_Token_Kind (Str : String) return Parser_Token_Kind;

    Min_Following_Operands : constant array (Option_Action) of Natural :=
                               (True_When_Set  => 0,
                                False_When_Set => 0,
                                Store_Int      => 1,
                                Store_String   => 1,
                                Store_Operands => 1);

    Max_Following_Operands : constant array (Option_Action) of Natural :=
                               (True_When_Set  => 0,
                                False_When_Set => 0,
                                Store_Int      => 1,
                                Store_String   => 1,
                                Store_Operands => Natural'Last);

    type Option is record
        Name         : ASU.Unbounded_String;
        Short_Option : ASU.Unbounded_String;
        Long_Option  : ASU.Unbounded_String;
        Help         : ASU.Unbounded_String;
        Action       : Option_Action;
        -- Validator : access function (Str : String) return Boolean;
    end record;

    -- The type backing an option.
    type Option_Kind is (Boolean_Option, Integer_Option, String_Option, Operands_Option);

    --
    -- Backing values stored for options.
    --
    type Option_Value_Variant is record
        Kind          : Option_Kind;
        Boolean_Value : Boolean := False;
        Integer_Value : Integer := 0;
        Operands      : String_Vectors.Vector;
    end record;

    --
    -- Stores groups of values needed to back options.
    --
    package Option_Value_Maps is new Ada.Containers.Ordered_Maps(Key_Type => ASU.Unbounded_String,
                                                                 Element_Type => Option_Value_Variant,
                                                                 "<"          => ASU."<");

    package Option_Vectors is new Ada.Containers.Vectors(Index_Type   => Positive,
                                                         Element_Type => Option);

    -- TODO: Need to add sub-parsers.
    type Parser is tagged limited record
        Options : Option_Vectors.Vector;
        Defaults : Option_Value_Maps.Map;
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
        Values : Option_Value_Maps.Map;
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

end Trendy_Command_Line;
