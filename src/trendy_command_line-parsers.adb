with Ada.Command_Line;
with Trendy_Command_Line.Context_Free; use Trendy_Command_Line.Context_Free;

package body Trendy_Command_Line.Parsers is
    ---------------------------------------------------------------------------
    --
    ---------------------------------------------------------------------------
    function Has_Format_For_Option (P : Parser; Name : Option_Name) return Boolean is (P.Formats(Name).Status = Added);

    procedure Add_Option (P            : in out Parser;
                          Name         : Option_Name;
                          Short_Option : String := "";
                          Long_Option  : String := "";
                          Help         : String;
                          Action       : Option_Action := True_When_Set) is
    begin
        P.Formats(Name) := (Added,
                            ASU.To_Unbounded_String(Short_Option),
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

    procedure Add_Operand (P     : in out Parser;
                           Name  : Operand_Name;
                           Arity : Operand_Arity := One;
                           Help  : String) is
    begin
        P.Operand_Formats(Name) := (Added, Arity => Arity, Help => ASU.To_Unbounded_String(Help));
    end Add_Operand;

    ---------------------------------------------------------------------------

    function Kind (P : Parser; Name : Option_Name) return Option_Kind is (Action_To_Kind(P.Formats(Name).Action));

    ---------------------------------------------------------------------------

    function Is_Flag (P : Parser; Name : Option_Name) return Boolean is
        Action : constant Option_Action := P.Formats(Name).Action;
    begin
        return Min_Following_Operands (Action) = 0 and then Max_Following_Operands (Action) = 0;
    end Is_Flag;

    ---------------------------------------------------------------------------

    procedure Default (P    : in out Parser;
                       Name : Option_Name;
                       Str  : String) is
    begin
        P.Defaults(Name).Operands.Clear;
        P.Defaults(Name).Operands.Append(ASU.To_Unbounded_String(Str));
    end Default;


    procedure No_Options (P : in out Parser) is
    begin
        for Option in Option_Name loop
            P.Formats(Option).Status := Ignored;
        end loop;
    end No_Options;

    procedure No_Operands (P : in out Parser) is
    begin
        for Operand in Operand_Name loop
            P.Operand_Formats(Operand).Status := Ignored;
        end loop;
    end No_Operands;
    ---------------------------------------------------------------------------
    -- Validation
    ---------------------------------------------------------------------------
    procedure Verify_Fulfilled (P : in Parser; Args : in Parsed_Arguments; Option : Option_Name) is
    begin
        pragma Unreferenced (P, Args, Option);
        --  raise Unfulfilled_Option;
        null;
    end Verify_Fulfilled;

    ---------------------------------------------------------------------------

    procedure Verify_Fulfilled (P : in Parser; Args : in Parsed_Arguments; Operand : in Operand_Name) is
        -- Num_Operands : constant Natural := Natural(Args.Operands(Operand).Length);
    begin
        --  case P.Operandnd_Formats(Operand).Arity is
        --      when One =>
        --          if Num_Operands /= 1 then
        --              raise Unfulfilled_Operand;
        --              null;
        --          end if;
        --      when One_Or_More =>
        --          if Num_Operands < 1 then
        --              raise Unfulfilled_Operand with "Operand is not fulfilled " & Operand'Image;
        --              null;
        --          end if;
        --      when Zero_Or_More =>
        --          null;
        --  end case;
        null;
    end Verify_Fulfilled;

    ---------------------------------------------------------------------------
    -- Handler functions
    ---------------------------------------------------------------------------

    function Short_Option_To_Name (P : in Parser; C : Character) return Option_Name is
        use Ada.Strings.Unbounded;
    begin
        for Opt in Option_Name loop
            if P.Formats(Opt).Short_Option = "-" & C then
                return Opt;
            end if;
        end loop;
        raise Unknown_Option with "Unknown option: " & C'Image;
    end Short_Option_To_Name;

    ---------------------------------------------------------------------------

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
        raise Unknown_Option with "Unknown option: " & Str;
    end Long_Option_To_Name;

    ---------------------------------------------------------------------------
    -- Main Parse Function
    ---------------------------------------------------------------------------
    function Parse (P : aliased in Parser) return Parsed_Arguments is
        Args : String_Vectors.Vector;
    begin
        for Index in 1 .. Ada.Command_Line.Argument_Count loop
            Args.Append(ASU.To_Unbounded_String(Ada.Command_Line.Argument(Index)));
        end loop;
        return Parse (P, Args);
    end Parse;

    ---------------------------------------------------------------------------

    procedure Process_Command_Or_Operand(P             : in Parser;
                                         Next_Argument : in ASU.Unbounded_String;
                                         State         : in out Parse_State)
    is
    begin
        if State.Has_Last_Option then
            declare
                Num_Current_Args : constant Natural := Natural(State.Last_Option_Arguments.Length);
                Max_Args : constant Natural := Max_Following_Operands (P.Formats(State.Last_Option).Action);
            begin
                if Num_Current_Args < Max_Args then
                    Process_Operand (State, Next_Argument);
                    if Num_Current_Args + 1 = Max_Args then
                        Clear_Option (P, State);
                    end if;
                else
                    raise Too_Many_Arguments with "Too many arguments provided for " & State.Last_Option'Image;
                end if;
            end;
        elsif State.Has_More_Operands then
            null;
        end if;
    end Process_Command_Or_Operand;

    ---------------------------------------------------------------------------

    procedure Process_Option_Named (P      : in Parser;
                                    Name   : in Option_Name;
                                    Result : in out Parsed_Arguments;
                                    State  : in out Parse_State)
    is
        Action : constant Option_Action := P.Formats (Name).Action;
    begin
        if Action in Option_Flag then
            if Result.Values(Name).Occurrences > 0 then
                raise Too_Many_Occurrences with Name'Image & " appeared too many times.";
            end if;
            case Action is
                when True_When_Set =>
                    Result.Values(Name).Boolean_Value := True;
                    Clear_Option (P, State);
                when False_When_Set =>
                    Result.Values(Name).Boolean_Value := False;
                    Clear_Option (P, State);
                when others =>
                    raise Wrong_Option_Type;
            end case;
        else
            case Action is
                when Store_Int => Start_Option (State, Name);
                when Store_String => Start_Option (State, Name);
                when Store_Operands => Start_Option (State, Name);
                when others =>
                    raise Program_Error;
            end case;
        end if;
        Result.Values(Name).Occurrences := Result.Values(Name).Occurrences + 1;
    end Process_Option_Named;

    ---------------------------------------------------------------------------

    procedure Process_Short_Option_Or_Group (P             : in Parser;
                                             Next_Argument : in ASU.Unbounded_String;
                                             Result        : in out Parsed_Arguments;
                                             State         : in out Parse_State)
    is
        Name   : Option_Name;
        Action : Option_Action;
    begin
        for C in 2 .. ASU.Length(Next_Argument) loop
            Name := Short_Option_To_Name (P, ASU.Element(Next_Argument, C));
            Action := P.Formats(Name).Action;

            if Is_Flag (P, Name) then
                Process_Option_Named(P, Name, Result, State);
            elsif Min_Following_Operands (Action) = 1 and then Max_Following_Operands (Action) = 1 then
                Process_Option_Named(P, Name, Result, State);

                -- The rest of the string is the argument.
                if C < ASU.Length(Next_Argument) then
                    -- Assume the rest of the "group option" is really an argument.
                    Process_Operand (State, ASU.Unbounded_Slice (Next_Argument, C + 1, ASU.Length(Next_Argument)));
                    exit;
                end if;
            else
                -- Found an option requiring an argument in the middle!!!!
                raise Wrong_Option_Type with "Argument requiring argument in the middle.";
            end if;
        end loop;
    end Process_Short_Option_Or_Group;

    ---------------------------------------------------------------------------

    procedure Process_Long_Option (P             : in Parser;
                                   Next_Argument : in ASU.Unbounded_String;
                                   Result        : in out Parsed_Arguments;
                                   State         : in out Parse_State)
    is
        Name : constant Option_Name := Long_Option_To_Name (P, ASU.To_String (Next_Argument));
    begin
        Process_Option_Named (P, Name, Result, State);
    end Process_Long_Option;

    ---------------------------------------------------------------------------

    function Parse (P : aliased in Parser; Args : in String_Vectors.Vector) return Parsed_Arguments is
        Next_Argument : ASU.Unbounded_String;
        Args_Left     : String_Vectors.Vector := Args.Copy;
        Result        : aliased Parsed_Arguments;
        State         : Parse_State (P'Access, Result'Access);
    begin
        for Option in Option_Name loop
            if P.Formats(Option).Status = Unused then
                raise Unused_Argument with "Unused Option: " & Option'Image;
            end if;
        end loop;

        for Operand in Operand_Name loop
            if P.Operand_Formats(Operand).Status = Unused then
                raise Unused_Argument with "Unused Operand: " & Operand'Image;
            end if;
        end loop;

        -- Assume we're going to get the defaults, and then override as needed.
        Result.Values := P.Defaults;

        while not Args_Left.Is_Empty loop
            Next_Argument := Args_Left.First_Element;
            Args_Left.Delete_First;

            case General_Token_Kind (ASU.To_String(Next_Argument)) is
                when Command_Or_Operand =>
                    --  Put_Line ("Command or operand: " & ASU.To_String(Next_Argument));
                    Process_Command_Or_Operand (P, Next_Argument, State);
                when Short_Option_Or_Group =>
                    --  Put_Line ("Short option or group: " & ASU.To_String(Next_Argument));
                    Process_Short_Option_Or_Group (P, Next_Argument, Result, State);
                when Long_Option =>
                    --  Put_Line ("Long option: " & ASU.To_String(Next_Argument));
                    Process_Long_Option (P, Next_Argument, Result, State);
                when Option_Terminator => raise Unimplemented;
                    -- Get the next argument type.
                    --  case Classify_Argument (Args_Left.First_Element) is
                    --      when Argument_Short_Option =>
                    --      when Argument_Short_Option_Group
            end case;
        end loop;

        Clear_Option (P, State);

        -- Ensure that all options and operands have been fulfilled correctly.
        for Option in Option_Name loop
            Verify_Fulfilled(P, Result, Option);
        end loop;

        for Operand in Operand_Name loop
            Verify_Fulfilled(P, Result, Operand);
        end loop;
        return Result;
    end Parse;

    ---------------------------------------------------------------------------

    function Get_Boolean(P : in Parsed_Arguments; Name : Option_Name) return Boolean is
    begin
        case P.Values(Name).Kind is
            when Boolean_Option => return P.Values(Name).Boolean_Value;
            when others => raise Wrong_Option_Type;
        end case;
    end Get_Boolean;

    ---------------------------------------------------------------------------

    function Get_String (P : in Parsed_Arguments; Name : Option_Name) return String is
    begin
        case P.Values(Name).Kind is
            when String_Option =>
                if P.Values(Name).Operands.Is_Empty then
                    raise No_Value with "No operands provided for " & Name'Image;
                else
                    return ASU.To_String(P.Values(Name).Operands.First_Element);
                end if;
            when others => raise Wrong_Option_Type;
        end case;
    end Get_String;

    ---------------------------------------------------------------------------

    function Is_Last_Option_Fulfilled (P : in Parser; State : in Parse_State) return Boolean is
        Num_Args : constant Natural := Natural(State.Last_Option_Arguments.Length);
    begin
        return Num_Args >= Min_Following_Operands(P.Formats(State.Last_Option).Action);
    end Is_Last_Option_Fulfilled;

    ---------------------------------------------------------------------------

    procedure Start_Option (P : in out Parse_State; Name : Option_Name) is
    begin
        P.Has_Last_Option := True;
        P.Last_Option := Name;
        P.Last_Option_Arguments.Clear;
    end Start_Option;

    ---------------------------------------------------------------------------

    procedure Clear_Option (P : in Parser; State : in out Parse_State) is
    begin
        pragma Unreferenced (P);
        if State.Has_Last_Option then
            State.Result.Values(State.Last_Option).Operands := State.Last_Option_Arguments;
            State.Has_Last_Option := False;
        end if;
    end Clear_Option;

    ---------------------------------------------------------------------------

    procedure Process_Operand (P : in out Parse_State; Operand : in ASU.Unbounded_String) is
    begin
        if P.Has_Last_Option then
            P.Last_Option_Arguments.Append (Operand);
        elsif P.Has_More_Operands then
            raise Unimplemented;
        end if;
    end Process_Operand;

end Trendy_Command_Line.Parsers;
