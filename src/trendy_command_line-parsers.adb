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
        P.Options(Name) := (ASU.To_Unbounded_String(Short_Option),
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
    --
    ---------------------------------------------------------------------------
    function Allocate (Params : Parser_Parameters) return Parser_Access is
    begin
        pragma Unreferenced (Params);
        return new Parser;
    end Allocate;

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
            Result.Values := P.Defaults;

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

    function Get_Boolean(P : in Parsed_Arguments; Name : Option_Name) return Boolean is
    begin
        case P.Values(Name).Kind is
            when Boolean_Option => return P.Values(Name).Boolean_Value;
            when others => raise Wrong_Option_Type;
        end case;
    end Get_Boolean;

end Trendy_Command_Line.Parsers;
