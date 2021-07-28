with Ada.Command_Line;
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
        raise Unknown_Option with C'Image;
    end Short_Option_To_Name;

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
                    Clear_Option (State);
                when False_When_Set =>
                    Result.Values(Name).Boolean_Value := False;
                    Clear_Option (State);
                when others => raise Unknown_Option;
                    -- TODO: Handle
            end case;
        end if;
        null;
        Result.Values(Name).Occurrences := Result.Values(Name).Occurrences + 1;
    end Process_Option_Named;

    procedure Process_Short_Option_Or_Group (P             : in Parser;
                                             Next_Argument : in ASU.Unbounded_String;
                                             Result        : in out Parsed_Arguments;
                                             State         : in out Parse_State)
    is
        Name                      : Option_Name;
        Action                    : Option_Action;
    begin
        for C in 2 .. ASU.Length(Next_Argument) loop
            Name := Short_Option_To_Name (P, ASU.Element(Next_Argument, C));
            Action := P.Formats(Name).Action;

            -- Flag
            if Min_Following_Operands (Action) = 0 and then Max_Following_Operands (Action) = 0 then
                Process_Option_Named(P, Name, Result, State);
            elsif Min_Following_Operands (Action) = 1 and then Max_Following_Operands (Action) = 1 then
                Start_Option(State, Name);

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

    procedure Process_Long_Option (P             : in Parser;
                                   Next_Argument : in ASU.Unbounded_String;
                                   Result        : in out Parsed_Arguments;
                                   State         : in out Parse_State)
    is
        Name : constant Option_Name := Long_Option_To_Name (P, ASU.To_String (Next_Argument));
    begin
        Process_Option_Named (P, Name, Result, State);
    end Process_Long_Option;

    function Parse (P : aliased in Parser; Args : in String_Vectors.Vector) return Parsed_Arguments is
        Next_Argument : ASU.Unbounded_String;
        Args_Left     : String_Vectors.Vector := Args.Copy;
        Result        : aliased Parsed_Arguments;
        State         : Parse_State (P'Access, Result'Access);
    begin
        -- TODO: Check that all Option types have been used.

        -- Assume we're going to get the defaults, and then override as needed.
        Result.Values := P.Defaults;

        while not Args_Left.Is_Empty loop
            Next_Argument := Args_Left.First_Element;
            Args_Left.Delete_First;

            case General_Token_Kind (ASU.To_String(Next_Argument)) is
                when Command_Or_Operand => raise Unimplemented;
                when Short_Option_Or_Group =>
                    Process_Short_Option_Or_Group (P, Next_Argument, Result, State);
                when Long_Option =>
                    Process_Long_Option (P, Next_Argument, Result, State);
                when Option_Terminator => raise Unimplemented;
                    -- Get the next argument type.
                    --  case Classify_Argument (Args_Left.First_Element) is
                    --      when Argument_Short_Option =>
                    --      when Argument_Short_Option_Group
            end case;
        end loop;

        Clear_Option (State);
        return Result;
    end Parse;

    function Get_Boolean(P : in Parsed_Arguments; Name : Option_Name) return Boolean is
    begin
        case P.Values(Name).Kind is
            when Boolean_Option => return P.Values(Name).Boolean_Value;
            when others => raise Wrong_Option_Type;
        end case;
    end Get_Boolean;

    function Get_String (P : in Parsed_Arguments; Name : Option_Name) return String is
    begin
        case P.Values(Name).Kind is
            when String_Option =>
                if P.Values(Name).Operands.Is_Empty then
                    raise No_Value;
                else
                    return ASU.To_String(P.Values(Name).Operands.First_Element);
                end if;
            when others => raise Wrong_Option_Type;
        end case;
    end Get_String;

    procedure Start_Option (P : in out Parse_State; Name : Option_Name) is
    begin
        P.Has_Last_Option := True;
        P.Last_Option := Name;
        P.Last_Option_Arguments.Clear;
    end Start_Option;

    procedure Clear_Option (P : in out Parse_State) is
    begin
        if P.Has_Last_Option then
            P.Result.Values(P.Last_Option).Operands := P.Last_Option_Arguments;
        end if;

        P.Has_Last_Option := False;
    end Clear_Option;

    procedure Process_Operand (P : in out Parse_State; Operand : in ASU.Unbounded_String) is
    begin
        P.Last_Option_Arguments.Append (Operand);
    end Process_Operand;

end Trendy_Command_Line.Parsers;
