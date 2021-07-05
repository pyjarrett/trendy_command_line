with Ada.Text_IO;
with Ada.Characters.Handling;

package body Trendy_Command_Line is
    --  use all type ASU.Unbounded_String;

    -- Valid option characters after the first hyphen.
    -- TODO: This might need to be reduced to only letters, using an additional
    -- check only 3rd -> last also allowing a hyphen.
    function Is_Valid_Option_Character (C : Character) return Boolean is
    begin
        return Ada.Characters.Handling.Is_Graphic (C)
            and then not Ada.Characters.Handling.Is_Space (C);
    end Is_Valid_Option_Character;

    ---------------------------------------------------------------------------
    -- Context-free checks
    ---------------------------------------------------------------------------

    function Is_Long_Option (Str : String) return Boolean is
    begin
        return
            Str'Length > 2 and then Str (Str'First) = '-' and then Str (Str'First + 1) = '-'
            and then Str (Str'First + 2) /= '-'
            and then (for all C in (Str'First + 2) .. Str'Last => Is_Valid_Option_Character (Str (C)));
    end Is_Long_Option;

    function Is_Short_Option (Str : String) return Boolean is
    begin
        return
            Str'Length = 2 and then Str (Str'First) = '-'
            and then Is_Valid_Option_Character (Str (Str'First + 1))
            and then Str (Str'First + 1) /= '-';
    end Is_Short_Option;

    -- It's not possible to disambiguate between a short option with an argument
    -- and a short option group of multiple options without any arguments.
    --
    -- https://www.gnu.org/software/libc/manual/html_node/Argument-Syntax.html
    --     "An option and its argument may or may not appear as separate tokens.
    --     (In other words, the whitespace separating them is optional.) Thus,
    --     -o foo and -ofoo are equivalent. "
    function Is_Short_Option_Or_Group (Str : String) return Boolean is
    begin
        return Str'Length >= 2 and then Str (Str'First) = '-'
            and then (for all C in (Str'First + 1) .. Str'Last =>
                                 Is_Valid_Option_Character(Str(C)) and then Str(C) /= '-');
    end Is_Short_Option_Or_Group;

    -- Whether somethign is a command or an operand depends on the parser.
    function Is_Command_Or_Operand (Str : String) return Boolean is
    begin
        return Str(Str'First) /= '-';
    end Is_Command_Or_Operand;

    ---------------------------------------------------------------------------
    --
    ---------------------------------------------------------------------------

    procedure Add_Option (P : in out Parser;
                          Name : String;
                          Short_Option : String := "";
                          Long_Option : String := "";
                          Help : String;
                          Action : Option_Action := True_When_Set) is
    begin
        P.Options.Append((ASU.To_Unbounded_String(Name),
                         ASU.To_Unbounded_String(Short_Option),
                         ASU.To_Unbounded_String(Long_Option),
                         ASU.To_Unbounded_String(Help),
                         Action));
    end Add_Option;

    procedure Initialize_Storage_And_Defaults (P : Parser; Result : in out Parsed_Arguments) is
        Kind : Option_Kind;
    begin
        for Opt of P.Options loop
            case Opt.Action is
                when True_When_Set => Kind := Boolean_Option;
                when False_When_Set => Kind := Boolean_Option;
                when Store_Int => Kind := Integer_Option;
                when Store_String => Kind := String_Option;
                when Store_Operands => Kind := Operands_Option;
            end case;

            Result.Values.Insert(Opt.Name, (Kind => Kind, others => <>));

            case Opt.Action is
                when True_When_Set => Result.Values(Opt.Name).Boolean_Value := False;
                when False_When_Set => Result.Values(Opt.Name).Boolean_Value := True;
                when Store_Int => null;
                when Store_String => null;
                when Store_Operands => null;
            end case;
        end loop;
    end Initialize_Storage_And_Defaults;


    ---------------------------------------------------------------------------
    -- Parser state.
    ---------------------------------------------------------------------------
    function Is_Done (State : Parse_State) return Boolean is
    begin
        return State.Unprocessed_Arguments.Is_Empty;
    end Is_Done;

    function Pop_Argument (State : in out Parse_State) return ASU.Unbounded_String
        with Pre => not Is_Done(State)
    is
    begin
        return Next_Arg : constant ASU.Unbounded_String := State.Unprocessed_Arguments.First_Element do
            State.Unprocessed_Arguments.Delete_First;
        end return;
    end Pop_Argument;

    ---------------------------------------------------------------------------
    --
    ---------------------------------------------------------------------------
    --  function Storage_For_Long_Option (P : Parser; Option_Text : ASU.Unbounded_String) return ASU.Unbounded_String is
    --  begin
    --      for Opt of P.Options loop
    --          if Opt.Long_Option = Option_Text then
    --              return Opt.Name;
    --          end if;
    --      end loop;
    --      raise Unknown_Option;
    --  end Storage_For_Long_Option;

    ---------------------------------------------------------------------------
    -- Handler functionss
    ---------------------------------------------------------------------------
    procedure Handle_Long_Option (State  : Parse_State;
                                  Str    : ASU.Unbounded_String;
                                  Parsed : in out Parsed_Arguments) is
        --  Storage : constant ASU.Unbounded_String := Storage_For_Long_Option (State.Current_ParserParser.all, Str);
    begin
        null;
        --  Parsed.Values(Storage).Boolean_Value := True;
    end Handle_Long_Option;

    ---------------------------------------------------------------------------
    -- Main Parse Function
    ---------------------------------------------------------------------------

    function Parse (P : aliased in out Parser; Args : in String_Vectors.Vector) return Parsed_Arguments is
        Next_Argument : ASU.Unbounded_String;
        State : Parse_State;
    begin
        State.Unprocessed_Arguments := Args.Copy;
        return Result : Parsed_Arguments do
            Initialize_Storage_And_Defaults (P, Result);

            while not Is_Done(State) loop
                Next_Argument := Pop_Argument(State);

                Ada.Text_IO.Put_Line (ASU.To_String(Next_Argument));

                case General_Token_Kind (ASU.To_String(Next_Argument)) is
                    when Command_Or_Operand => null;
                    when Short_Option_Or_Group => null;
                    when Long_Option => Handle_Long_Option(State, Next_Argument, Result);
                    when Option_Terminator => null;
                        -- Get the next argument type.
                        --  case Classify_Argument (Args_Left.First_Element) is
                        --      when Argument_Short_Option =>
                        --      when Argument_Short_Option_Group
                end case;
            end loop;
        end return;
    end Parse;

    function Boolean_Value_Of(P : in Parsed_Arguments; Name : String) return Boolean is
    begin
        return P.Values(ASU.To_Unbounded_String(Name)).Boolean_Value;
    end Boolean_Value_Of;

    function General_Token_Kind (Str : String) return Parser_Token_Kind is
    begin
        if Is_Long_Option (Str) then
            return Long_Option;
        end if;
        if Is_Short_Option_Or_Group (Str) then
            return Short_Option_Or_Group;
        end if;
        if Str = "--" then
            return Option_Terminator;
        end if;
        if Is_Command_Or_Operand (Str) then
            return Command_Or_Operand;
        end if;
        raise Unknown_Token;
    end General_Token_Kind;

end Trendy_Command_Line;
