with Ada.Text_IO;

with Trendy_Command_Line.Context_Free; use Trendy_Command_Line.Context_Free;

package body Trendy_Command_Line.Parsers is

    ---------------------------------------------------------------------------
    --
    ---------------------------------------------------------------------------
    procedure Add_Option (P            : in out Parser;
                          Name         : Option_Name;
                          Short_Option : String := "";
                          Long_Option  : String := "";
                          Help         : String;
                          Action       : Option_Action := True_When_Set) is
    begin
        P.Formats(Name) := (ASU.To_Unbounded_String(Short_Option),
                            ASU.To_Unbounded_String(Long_Option),
                            ASU.To_Unbounded_String(Help),
                            Action);
        P.Defaults(Name).Kind := Action_To_Kind(Action);

        case Action is
            when True_When_Set => P.Defaults(Name).Boolean_Value := False;
            when False_When_Set => P.Defaults(Name).Boolean_Value := True;
            when others => null;
        end case;
    end Add_Option;

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

    function Long_Option_To_Name(P : in Parser; Str : String) return Option_Name
        with Pre => Is_Long_Option (Str)
    is
        use Ada.Strings.Unbounded;
    begin
        for Opt in Option_Name loop
            if P.Formats(Opt).Long_Option = Str then
                return Opt;
            end if;
        end loop;
        raise Unknown_Option;
    end Long_Option_To_Name;

    ---------------------------------------------------------------------------
    -- Handler functions
    ---------------------------------------------------------------------------
    function Begin_Parse (P : aliased in Parser; Args : in String_Vectors.Vector) return Parse_State is
    begin
        return State : Parse_State (P'Access) do
            State.Unprocessed_Arguments := Args.Copy;
        end return;
    end Begin_Parse;

    procedure Handle_Long_Option (State  : in Parse_State;
                                  Str    : ASU.Unbounded_String;
                                  Parsed : in out Parsed_Arguments) is
        Name   : constant Option_Name := Long_Option_To_Name (State.Current_Parser.all, ASU.To_String(Str));
        Action : constant Option_Action := State.Current_Parser.Formats(Name).Action;
    begin
        -- if it's a toggle, set the boolean appropriately.
        if Action in Option_Flag then
            case Action is
                when True_When_Set => Parsed.Values(Name).Boolean_Value := True;
                when False_When_Set => Parsed.Values(Name).Boolean_Value := False;
                when others => raise Unknown_Option;
                    -- TODO: Handle
            end case;
        end if;

        -- some options start parsing of operands, or expect arguments.
        raise Unknown_Option;
    end Handle_Long_Option;

    ---------------------------------------------------------------------------
    -- Main Parse Function
    ---------------------------------------------------------------------------

    function Parse (P : aliased in out Parser; Args : in String_Vectors.Vector) return Parsed_Arguments is
        Next_Argument : ASU.Unbounded_String;
        State : Parse_State := Begin_Parse (P, Args);
    begin
        return Result : Parsed_Arguments do
            Result.Values := P.Defaults;

            while not Is_Done(State) loop
                Next_Argument := Pop_Argument(State);

                Ada.Text_IO.Put_Line ("Next argument " & ASU.To_String(Next_Argument));

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

    function Get_Boolean(P : in Parsed_Arguments; Name : Option_Name) return Boolean is
    begin
        case P.Values(Name).Kind is
            when Boolean_Option => return P.Values(Name).Boolean_Value;
            when others => raise Wrong_Option_Type;
        end case;
    end Get_Boolean;

end Trendy_Command_Line.Parsers;
